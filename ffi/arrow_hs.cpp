#include <arrow/api.h>
#include <arrow/compute/api.h>
#include <arrow/io/file.h>
#include <parquet/arrow/reader.h>
#include <parquet/arrow/writer.h>
#include <climits>
#include <cstring>
#include <string>
#include <vector>

using ColPtr   = std::shared_ptr<arrow::Array>;
using ValPtr   = std::shared_ptr<arrow::Scalar>;
using BatchPtr = std::shared_ptr<arrow::RecordBatch>;

// ---------------------------------------------------------------------------
// Compute kernel registration — static constructors don't run under GHC's linker
// ---------------------------------------------------------------------------

namespace arrow::compute::internal {
void RegisterScalarArithmetic(FunctionRegistry*);
void RegisterScalarComparison(FunctionRegistry*);
void RegisterScalarBoolean(FunctionRegistry*);
void RegisterScalarValidity(FunctionRegistry*);
void RegisterScalarAggregateBasic(FunctionRegistry*);
void RegisterVectorSort(FunctionRegistry*);
void RegisterVectorArraySort(FunctionRegistry*);
void RegisterScalarSetLookup(FunctionRegistry*);
void RegisterScalarIfElse(FunctionRegistry*);
void RegisterScalarNested(FunctionRegistry*);
void RegisterScalarTemporalUnary(FunctionRegistry*);
void RegisterScalarTemporalBinary(FunctionRegistry*);
void RegisterScalarRoundArithmetic(FunctionRegistry*);
void RegisterVectorRank(FunctionRegistry*);
void RegisterVectorReplace(FunctionRegistry*);
void RegisterVectorSelectK(FunctionRegistry*);
void RegisterVectorPairwise(FunctionRegistry*);
void RegisterVectorCumulativeSum(FunctionRegistry*);
void RegisterVectorNested(FunctionRegistry*);
void RegisterVectorRunEndEncode(FunctionRegistry*);
void RegisterVectorRunEndDecode(FunctionRegistry*);
void RegisterVectorStatistics(FunctionRegistry*);
void RegisterScalarRandom(FunctionRegistry*);
void RegisterScalarStringAscii(FunctionRegistry*);
void RegisterScalarStringUtf8(FunctionRegistry*);
void RegisterScalarAggregateMode(FunctionRegistry*);
void RegisterScalarAggregatePivot(FunctionRegistry*);
void RegisterScalarAggregateQuantile(FunctionRegistry*);
void RegisterScalarAggregateVariance(FunctionRegistry*);
void RegisterScalarAggregateTDigest(FunctionRegistry*);
void RegisterHashAggregateBasic(FunctionRegistry*);
void RegisterHashAggregateNumeric(FunctionRegistry*);
void RegisterHashAggregatePivot(FunctionRegistry*);
}

static bool g_inited = false;

static void ensure_init() {
    if (g_inited) return;
    g_inited = true;
    auto* reg = arrow::compute::GetFunctionRegistry();
    arrow::compute::internal::RegisterScalarArithmetic(reg);
    arrow::compute::internal::RegisterScalarComparison(reg);
    arrow::compute::internal::RegisterScalarBoolean(reg);
    arrow::compute::internal::RegisterScalarValidity(reg);
    arrow::compute::internal::RegisterScalarAggregateBasic(reg);
    arrow::compute::internal::RegisterVectorSort(reg);
    arrow::compute::internal::RegisterVectorArraySort(reg);
    arrow::compute::internal::RegisterScalarSetLookup(reg);
    arrow::compute::internal::RegisterScalarIfElse(reg);
    arrow::compute::internal::RegisterScalarNested(reg);
    arrow::compute::internal::RegisterScalarTemporalUnary(reg);
    arrow::compute::internal::RegisterScalarTemporalBinary(reg);
    arrow::compute::internal::RegisterScalarRoundArithmetic(reg);
    arrow::compute::internal::RegisterVectorRank(reg);
    arrow::compute::internal::RegisterVectorReplace(reg);
    arrow::compute::internal::RegisterVectorSelectK(reg);
    arrow::compute::internal::RegisterVectorPairwise(reg);
    arrow::compute::internal::RegisterVectorCumulativeSum(reg);
    arrow::compute::internal::RegisterVectorNested(reg);
    arrow::compute::internal::RegisterVectorRunEndEncode(reg);
    arrow::compute::internal::RegisterVectorRunEndDecode(reg);
    arrow::compute::internal::RegisterVectorStatistics(reg);
    arrow::compute::internal::RegisterScalarRandom(reg);
    arrow::compute::internal::RegisterScalarStringAscii(reg);
    arrow::compute::internal::RegisterScalarStringUtf8(reg);
    arrow::compute::internal::RegisterScalarAggregateMode(reg);
    arrow::compute::internal::RegisterScalarAggregatePivot(reg);
    arrow::compute::internal::RegisterScalarAggregateQuantile(reg);
    arrow::compute::internal::RegisterScalarAggregateVariance(reg);
    arrow::compute::internal::RegisterScalarAggregateTDigest(reg);
    arrow::compute::internal::RegisterHashAggregateBasic(reg);
    arrow::compute::internal::RegisterHashAggregateNumeric(reg);
    arrow::compute::internal::RegisterHashAggregatePivot(reg);
}

// ---------------------------------------------------------------------------
// Error handling — thread-local, returned via arrow_hs_last_error
// ---------------------------------------------------------------------------

static thread_local std::string g_err;

static void* err(const std::string& msg) { g_err = msg; return nullptr; }

#define TRY(var, expr) \
    auto var##_ = (expr); \
    if (!var##_.ok()) return err(var##_.status().message()); \
    auto var = std::move(*var##_)

#define TRY_S(expr) \
    { auto s_ = (expr); if (!s_.ok()) return err(s_.message()); }

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

static inline ColPtr&   col  (void* p) { return *static_cast<ColPtr*>(p); }
static inline ValPtr&   val  (void* p) { return *static_cast<ValPtr*>(p); }
static inline BatchPtr& batch(void* p) { return *static_cast<BatchPtr*>(p); }

// Must match dtypeFromCode in src/Arrow/Dtype.hs.
static uint8_t arrow_type_to_dtype(const arrow::DataType& t) {
    switch (t.id()) {
    case arrow::Type::BOOL:    return 0;
    case arrow::Type::INT8:    return 1;
    case arrow::Type::INT16:   return 2;
    case arrow::Type::INT32:   return 3;
    case arrow::Type::INT64:   return 4;
    case arrow::Type::UINT8:   return 5;
    case arrow::Type::UINT16:  return 6;
    case arrow::Type::UINT32:  return 7;
    case arrow::Type::UINT64:  return 8;
    case arrow::Type::FLOAT:   return 9;
    case arrow::Type::DOUBLE:  return 10;
    case arrow::Type::STRING:  return 11;
    default: return 255;
    }
}

static char* schema_field_name(const arrow::Schema& s, int i) {
    if (i < 0 || i >= s.num_fields()) return nullptr;
    return strdup(s.field(i)->name().c_str());
}

static uint8_t schema_field_type(const arrow::Schema& s, int i) {
    if (i < 0 || i >= s.num_fields()) return 255;
    return arrow_type_to_dtype(*s.field(i)->type());
}

template <typename B, typename C>
static void* mk_numeric(const C* data, const uint8_t* valid, int64_t n) {
    B builder;
    TRY_S(builder.Reserve(n));
    for (int64_t i = 0; i < n; i++) {
        if (valid[i]) builder.UnsafeAppend(static_cast<typename B::value_type>(data[i]));
        else builder.UnsafeAppendNull();
    }
    TRY(arr, builder.Finish());
    return new ColPtr(arr);
}

static void* compute2(void* a, void* b, const char* fn) {
    ensure_init();
    TRY(r, arrow::compute::CallFunction(fn, {col(a), col(b)}));
    return new ColPtr(r.make_array());
}

static void* compute1(void* a, const char* fn) {
    ensure_init();
    TRY(r, arrow::compute::CallFunction(fn, {col(a)}));
    return new ColPtr(r.make_array());
}

static void* agg(void* a, const char* fn) {
    ensure_init();
    TRY(r, arrow::compute::CallFunction(fn, {col(a)}));
    return new ValPtr(r.scalar());
}

// ---------------------------------------------------------------------------
// extern "C" API
// ---------------------------------------------------------------------------

extern "C" {

const char* arrow_hs_last_error() { return g_err.c_str(); }

// -- Lifecycle --------------------------------------------------------------

void arrow_hs_col_free(void* p)  { delete static_cast<ColPtr*>(p); }
void arrow_hs_val_free(void* p)  { delete static_cast<ValPtr*>(p); }
void arrow_hs_string_free(char* s) { free(s); }

// -- Construction -----------------------------------------------------------

void* arrow_hs_mk_bool(const uint8_t* data, const uint8_t* valid, int64_t n) {
    arrow::BooleanBuilder b;
    TRY_S(b.Reserve(n));
    for (int64_t i = 0; i < n; i++) {
        if (valid[i]) b.UnsafeAppend(data[i] != 0); else b.UnsafeAppendNull();
    }
    TRY(arr, b.Finish());
    return new ColPtr(arr);
}

#define MK(name, Builder, CType) \
void* name(const CType* d, const uint8_t* v, int64_t n) { \
    return mk_numeric<Builder, CType>(d, v, n); \
}

MK(arrow_hs_mk_int8,    arrow::Int8Builder,    int8_t)
MK(arrow_hs_mk_int16,   arrow::Int16Builder,   int16_t)
MK(arrow_hs_mk_int32,   arrow::Int32Builder,   int32_t)
MK(arrow_hs_mk_int64,   arrow::Int64Builder,   int64_t)
MK(arrow_hs_mk_uint8,   arrow::UInt8Builder,   uint8_t)
MK(arrow_hs_mk_uint16,  arrow::UInt16Builder,  uint16_t)
MK(arrow_hs_mk_uint32,  arrow::UInt32Builder,  uint32_t)
MK(arrow_hs_mk_uint64,  arrow::UInt64Builder,  uint64_t)
MK(arrow_hs_mk_float32, arrow::FloatBuilder,   float)
MK(arrow_hs_mk_float64, arrow::DoubleBuilder,  double)

void* arrow_hs_mk_utf8(const char** strs, const int64_t* lens,
                        const uint8_t* valid, int64_t n) {
    arrow::StringBuilder b;
    TRY_S(b.Reserve(n));
    for (int64_t i = 0; i < n; i++) {
        if (valid[i]) { TRY_S(b.Append(strs[i], static_cast<int32_t>(lens[i]))); }
        else          { TRY_S(b.AppendNull()); }
    }
    TRY(arr, b.Finish());
    return new ColPtr(arr);
}

// -- Access -----------------------------------------------------------------

int64_t arrow_hs_col_len(void* p)        { return col(p)->length(); }
int64_t arrow_hs_col_null_count(void* p)  { return col(p)->null_count(); }
char*   arrow_hs_col_to_string(void* p)   { return strdup(col(p)->ToString().c_str()); }

// -1 = out of bounds, 0 = null, 1 = valid
int8_t arrow_hs_elem_valid(void* p, int64_t i) {
    auto& a = col(p);
    if (i < 0 || i >= a->length()) return -1;
    return a->IsValid(i) ? 1 : 0;
}

// Per-type element getters (caller must check validity first)
#define GET(name, ArrayT, CType) \
CType name(void* p, int64_t i) { \
    return static_cast<const ArrayT&>(*col(p)).Value(i); \
}

uint8_t arrow_hs_get_bool(void* p, int64_t i) {
    return static_cast<const arrow::BooleanArray&>(*col(p)).Value(i);
}

GET(arrow_hs_get_int8,    arrow::Int8Array,    int8_t)
GET(arrow_hs_get_int16,   arrow::Int16Array,   int16_t)
GET(arrow_hs_get_int32,   arrow::Int32Array,   int32_t)
GET(arrow_hs_get_int64,   arrow::Int64Array,   int64_t)
GET(arrow_hs_get_uint8,   arrow::UInt8Array,   uint8_t)
GET(arrow_hs_get_uint16,  arrow::UInt16Array,  uint16_t)
GET(arrow_hs_get_uint32,  arrow::UInt32Array,  uint32_t)
GET(arrow_hs_get_uint64,  arrow::UInt64Array,  uint64_t)
GET(arrow_hs_get_float32, arrow::FloatArray,   float)
GET(arrow_hs_get_float64, arrow::DoubleArray,  double)

// Utf8 get — returns pointer into Arrow buffer (valid while Col lives)
const char* arrow_hs_get_utf8(void* p, int64_t i, int64_t* out_len) {
    auto sv = static_cast<const arrow::StringArray&>(*col(p)).GetView(i);
    *out_len = sv.size();
    return sv.data();
}

// -- Compute ----------------------------------------------------------------

#define BIN(name, k) void* name(void* a, void* b) { return compute2(a, b, k); }
#define UNA(name, k) void* name(void* a)           { return compute1(a, k); }
#define AGG(name, k) void* name(void* a)           { return agg(a, k); }

// Arithmetic
BIN(arrow_hs_add, "add")
BIN(arrow_hs_sub, "subtract")
BIN(arrow_hs_mul, "multiply")
BIN(arrow_hs_div, "divide")
UNA(arrow_hs_neg,  "negate")
UNA(arrow_hs_abs,  "abs")
UNA(arrow_hs_sign, "sign")

// Comparison → Col Bool
BIN(arrow_hs_eq,  "equal")
BIN(arrow_hs_neq, "not_equal")
BIN(arrow_hs_lt,  "less")
BIN(arrow_hs_gt,  "greater")
BIN(arrow_hs_lte, "less_equal")
BIN(arrow_hs_gte, "greater_equal")

// Vector ops
BIN(arrow_hs_filter,    "filter")
BIN(arrow_hs_take,      "take")
BIN(arrow_hs_fill_null, "coalesce")
UNA(arrow_hs_unique,    "unique")
UNA(arrow_hs_drop_null, "drop_null")
UNA(arrow_hs_is_nulls,  "is_null")
UNA(arrow_hs_is_valids, "is_valid")

void* arrow_hs_sort(void* p, uint8_t asc) {
    ensure_init();
    auto order = asc ? arrow::compute::SortOrder::Ascending
                     : arrow::compute::SortOrder::Descending;
    auto idx = arrow::compute::SortIndices(
        col(p), arrow::compute::SortOptions({arrow::compute::SortKey("", order)}));
    if (!idx.ok()) return err(idx.status().message());
    TRY(r, arrow::compute::Take(col(p), *idx));
    return new ColPtr(r.make_array());
}

// Boolean logic
BIN(arrow_hs_log_and, "and")
BIN(arrow_hs_log_or,  "or")
UNA(arrow_hs_log_not, "invert")

// Conditional
void* arrow_hs_if_else(void* cond, void* left, void* right) {
    ensure_init();
    TRY(r, arrow::compute::CallFunction("if_else", {col(cond), col(left), col(right)}));
    return new ColPtr(r.make_array());
}

// Aggregation → Val
AGG(arrow_hs_sum,     "sum")
AGG(arrow_hs_mean,    "mean")
AGG(arrow_hs_min,     "min")
AGG(arrow_hs_max,     "max")
AGG(arrow_hs_product, "product")

// -- Array primitives (APL support) -----------------------------------------

// iota: [0, 1, ..., n-1]
void* arrow_hs_iota(int64_t n) {
    arrow::Int64Builder b;
    TRY_S(b.Reserve(n));
    for (int64_t i = 0; i < n; i++) b.UnsafeAppend(i);
    TRY(arr, b.Finish());
    return new ColPtr(arr);
}

// fill: constant column of n copies of val
void* arrow_hs_fill_int64(int64_t n, int64_t v) {
    arrow::Int64Builder b;
    TRY_S(b.Reserve(n));
    for (int64_t i = 0; i < n; i++) b.UnsafeAppend(v);
    TRY(arr, b.Finish());
    return new ColPtr(arr);
}

// where: bool mask → int64 indices of true values
void* arrow_hs_where(void* p) {
    auto& ba = static_cast<const arrow::BooleanArray&>(*col(p));
    int64_t n = ba.length();
    arrow::Int64Builder b;
    for (int64_t i = 0; i < n; i++)
        if (ba.IsValid(i) && ba.Value(i)) { TRY_S(b.Append(i)); }
    TRY(arr, b.Finish());
    return new ColPtr(arr);
}

// concat: append two columns
void* arrow_hs_concat(void* a, void* b_) {
    TRY(r, arrow::Concatenate({col(a), col(b_)}));
    return new ColPtr(r);
}

// isIn: element-wise membership test
void* arrow_hs_is_in(void* p, void* value_set) {
    ensure_init();
    arrow::compute::SetLookupOptions opts(col(value_set));
    TRY(r, arrow::compute::CallFunction("is_in", {col(p)}, &opts));
    return new ColPtr(r.make_array());
}

// indexOf: for each needle, position in haystack (null if absent)
void* arrow_hs_index_of(void* haystack, void* needles) {
    ensure_init();
    arrow::compute::SetLookupOptions opts(col(haystack));
    TRY(r, arrow::compute::CallFunction("index_in", {col(needles)}, &opts));
    return new ColPtr(r.make_array());
}

// cast: convert column to target dtype (encoded as uint8)
static std::shared_ptr<arrow::DataType> dtype_to_arrow(uint8_t d) {
    switch (d) {
    case 0:  return arrow::boolean();
    case 1:  return arrow::int8();
    case 2:  return arrow::int16();
    case 3:  return arrow::int32();
    case 4:  return arrow::int64();
    case 5:  return arrow::uint8();
    case 6:  return arrow::uint16();
    case 7:  return arrow::uint32();
    case 8:  return arrow::uint64();
    case 9:  return arrow::float32();
    case 10: return arrow::float64();
    default: return nullptr;
    }
}

void* arrow_hs_cast(void* p, uint8_t target) {
    ensure_init();
    auto tgt = dtype_to_arrow(target);
    if (!tgt) return err("cast: unknown target dtype");
    TRY(r, arrow::compute::Cast(col(p), tgt));
    return new ColPtr(r.make_array());
}

// slice: array slice at offset with length
void* arrow_hs_slice(void* p, int64_t offset, int64_t length) {
    return new ColPtr(col(p)->Slice(offset, length));
}

// scatter: result[indices[i]] = values[i], other positions from src
void* arrow_hs_scatter(void* src_, void* indices_, void* values_) {
    ensure_init();
    auto& src = col(src_);
    auto& idx_arr = static_cast<const arrow::Int64Array&>(*col(indices_));
    auto& vals = col(values_);
    int64_t n = src->length();
    // sel[i] = i (from src) unless overridden → n + j (from vals)
    std::vector<int64_t> sel(n);
    for (int64_t i = 0; i < n; i++) sel[i] = i;
    for (int64_t j = 0; j < idx_arr.length(); j++)
        if (idx_arr.IsValid(j)) {
            int64_t pos = idx_arr.Value(j);
            if (pos >= 0 && pos < n) sel[pos] = n + j;
        }
    arrow::Int64Builder sb;
    TRY_S(sb.Reserve(n));
    for (int64_t i = 0; i < n; i++) sb.UnsafeAppend(sel[i]);
    TRY(sel_arr, sb.Finish());
    TRY(combined, arrow::Concatenate({src, vals}));
    TRY(r, arrow::compute::Take(combined, sel_arr));
    return new ColPtr(r.make_array());
}

// scatterScalar: result[indices[i]] = v, else from original (int64 only)
void* arrow_hs_scatter_scalar(void* src_, void* indices_, int64_t v) {
    auto& src = static_cast<const arrow::Int64Array&>(*col(src_));
    auto& idx_arr = static_cast<const arrow::Int64Array&>(*col(indices_));
    int64_t n = src.length();
    std::vector<int64_t> out(n);
    std::vector<bool> vld(n);
    for (int64_t i = 0; i < n; i++) { vld[i] = src.IsValid(i); out[i] = vld[i] ? src.Value(i) : 0; }
    for (int64_t j = 0; j < idx_arr.length(); j++)
        if (idx_arr.IsValid(j)) {
            int64_t pos = idx_arr.Value(j);
            if (pos >= 0 && pos < n) { out[pos] = v; vld[pos] = true; }
        }
    arrow::Int64Builder b;
    TRY_S(b.Reserve(n));
    for (int64_t i = 0; i < n; i++) { if (vld[i]) b.UnsafeAppend(out[i]); else b.UnsafeAppendNull(); }
    TRY(arr, b.Finish());
    return new ColPtr(arr);
}

// scan: cumulative scan with function tag (0=add, 1=mul, 2=max, 3=min), int64 only
void* arrow_hs_scan(uint8_t fn_tag, void* p) {
    auto& arr = static_cast<const arrow::Int64Array&>(*col(p));
    int64_t n = arr.length();
    arrow::Int64Builder b;
    TRY_S(b.Reserve(n));
    int64_t cur;
    switch (fn_tag) { case 0: cur = 0; break; case 1: cur = 1; break;
                      case 2: cur = INT64_MIN; break; case 3: cur = INT64_MAX; break; default: cur = 0; }
    for (int64_t i = 0; i < n; i++) {
        if (arr.IsNull(i)) { b.UnsafeAppendNull(); continue; }
        int64_t v = arr.Value(i);
        switch (fn_tag) { case 0: cur += v; break; case 1: cur *= v; break;
                          case 2: cur = std::max(cur, v); break; case 3: cur = std::min(cur, v); break; }
        b.UnsafeAppend(cur);
    }
    TRY(result, b.Finish());
    return new ColPtr(result);
}

// cumulativeSum: Arrow compute cumulative_sum
void* arrow_hs_cumulative_sum(void* p) {
    ensure_init();
    arrow::compute::CumulativeSumOptions opts;
    opts.skip_nulls = true;
    TRY(r, arrow::compute::CallFunction("cumulative_sum", {col(p)}, &opts));
    return new ColPtr(r.make_array());
}

// reverse: via descending-index take
void* arrow_hs_reverse(void* p) {
    ensure_init();
    int64_t n = col(p)->length();
    arrow::Int64Builder ib;
    TRY_S(ib.Reserve(n));
    for (int64_t i = n - 1; i >= 0; i--) ib.UnsafeAppend(i);
    TRY(idx, ib.Finish());
    TRY(r, arrow::compute::Take(col(p), idx));
    return new ColPtr(r.make_array());
}

// sortIndices: return sort permutation
void* arrow_hs_sort_indices(void* p, uint8_t asc) {
    ensure_init();
    auto order = asc ? arrow::compute::SortOrder::Ascending
                     : arrow::compute::SortOrder::Descending;
    auto r = arrow::compute::SortIndices(
        col(p), arrow::compute::SortOptions({arrow::compute::SortKey("", order)}));
    if (!r.ok()) return err(r.status().message());
    return new ColPtr(*r);
}

// replicate: expand col by integer counts (APL ⍺/⍵)
void* arrow_hs_replicate(void* col_, void* counts_) {
    ensure_init();
    auto& src = col(col_);
    auto& cnt = static_cast<const arrow::Int64Array&>(*col(counts_));
    int64_t n = cnt.length();
    arrow::Int64Builder ib;
    for (int64_t i = 0; i < n; i++) {
        int64_t c = cnt.IsValid(i) ? cnt.Value(i) : 0;
        for (int64_t j = 0; j < c; j++) { TRY_S(ib.Append(i)); }
    }
    TRY(idx, ib.Finish());
    TRY(r, arrow::compute::Take(src, idx));
    return new ColPtr(r.make_array());
}

// -- Val access -------------------------------------------------------------

uint8_t arrow_hs_val_is_valid(void* p) { return val(p)->is_valid; }
char*   arrow_hs_val_to_string(void* p) { return strdup(val(p)->ToString().c_str()); }

#define VAL_GET(name, ScalarT, CType) \
CType name(void* p) { return std::static_pointer_cast<ScalarT>(val(p))->value; }

VAL_GET(arrow_hs_val_get_int8,    arrow::Int8Scalar,    int8_t)
VAL_GET(arrow_hs_val_get_int16,   arrow::Int16Scalar,   int16_t)
VAL_GET(arrow_hs_val_get_int32,   arrow::Int32Scalar,   int32_t)
VAL_GET(arrow_hs_val_get_int64,   arrow::Int64Scalar,   int64_t)
VAL_GET(arrow_hs_val_get_uint8,   arrow::UInt8Scalar,   uint8_t)
VAL_GET(arrow_hs_val_get_uint16,  arrow::UInt16Scalar,  uint16_t)
VAL_GET(arrow_hs_val_get_uint32,  arrow::UInt32Scalar,  uint32_t)
VAL_GET(arrow_hs_val_get_uint64,  arrow::UInt64Scalar,  uint64_t)
VAL_GET(arrow_hs_val_get_float32, arrow::FloatScalar,   float)
VAL_GET(arrow_hs_val_get_float64, arrow::DoubleScalar,  double)

// ---------------------------------------------------------------------------
// RecordBatch
// ---------------------------------------------------------------------------

void arrow_hs_batch_free(void* p) { delete static_cast<BatchPtr*>(p); }

int64_t arrow_hs_batch_num_rows(void* p) { return batch(p)->num_rows(); }
int64_t arrow_hs_batch_num_cols(void* p) { return batch(p)->num_columns(); }

void* arrow_hs_batch_col(void* p, int64_t i) {
    auto& b = batch(p);
    if (i < 0 || i >= b->num_columns()) return err("batch_col: index out of bounds");
    return new ColPtr(b->column(i));
}

char*   arrow_hs_batch_col_name(void* p, int64_t i) { return schema_field_name(*batch(p)->schema(), (int)i); }
uint8_t arrow_hs_batch_col_type(void* p, int64_t i) { return schema_field_type(*batch(p)->schema(), (int)i); }

void* arrow_hs_batch_make(const char** names, void** cols, int64_t n) {
    arrow::FieldVector fields;
    std::vector<ColPtr> arrays;
    fields.reserve(n);
    arrays.reserve(n);
    int64_t rows = 0;
    for (int64_t i = 0; i < n; i++) {
        auto& a = col(cols[i]);
        fields.push_back(arrow::field(names[i], a->type()));
        arrays.push_back(a);
        if (i == 0) rows = a->length();
        else if (a->length() != rows) return err("batch_make: column length mismatch");
    }
    return new BatchPtr(arrow::RecordBatch::Make(arrow::schema(fields), rows, arrays));
}

// ---------------------------------------------------------------------------
// Parquet reader (streaming)
// ---------------------------------------------------------------------------

struct HsParquetReader {
    std::unique_ptr<parquet::arrow::FileReader> file_reader;
    std::shared_ptr<arrow::RecordBatchReader>   batch_reader;
    std::shared_ptr<arrow::Schema>              schema;
};

void* arrow_hs_parquet_open(const char* path, int64_t batch_size) {
    TRY(input, arrow::io::ReadableFile::Open(path));
    TRY(fr,    parquet::arrow::OpenFile(input, arrow::default_memory_pool()));

    if (batch_size > 0) fr->set_batch_size(batch_size);

    std::shared_ptr<arrow::Schema> schema;
    auto st = fr->GetSchema(&schema);
    if (!st.ok()) return err(st.message());

    TRY(br, fr->GetRecordBatchReader());
    std::shared_ptr<arrow::RecordBatchReader> br_shared = std::move(br);

    return new HsParquetReader{std::move(fr), std::move(br_shared), std::move(schema)};
}

void arrow_hs_parquet_close(void* p) { delete static_cast<HsParquetReader*>(p); }

int64_t arrow_hs_parquet_num_rows(void* p) {
    return static_cast<HsParquetReader*>(p)->file_reader->parquet_reader()->metadata()->num_rows();
}
int arrow_hs_parquet_num_cols(void* p) { return static_cast<HsParquetReader*>(p)->schema->num_fields(); }

char*   arrow_hs_parquet_col_name(void* p, int i) { return schema_field_name(*static_cast<HsParquetReader*>(p)->schema, i); }
uint8_t arrow_hs_parquet_col_type(void* p, int i) { return schema_field_type(*static_cast<HsParquetReader*>(p)->schema, i); }

// Returns 0 = ok (*out=batch), 1 = EOF (*out=null), -1 = error (g_err set).
int arrow_hs_parquet_next_batch(void* p, void** out_batch) {
    auto* pr = static_cast<HsParquetReader*>(p);
    std::shared_ptr<arrow::RecordBatch> b;
    auto st = pr->batch_reader->ReadNext(&b);
    if (!st.ok()) { g_err = st.message(); *out_batch = nullptr; return -1; }
    if (!b)       {                        *out_batch = nullptr; return  1; }
    *out_batch = new BatchPtr(b);
    return 0;
}

// ---------------------------------------------------------------------------
// Parquet writer
// ---------------------------------------------------------------------------

struct HsParquetWriter {
    std::shared_ptr<arrow::io::FileOutputStream> out;
    std::unique_ptr<parquet::arrow::FileWriter>  writer;
};

void* arrow_hs_parquet_writer_open(const char* path, void* schema_batch) {
    TRY(out, arrow::io::FileOutputStream::Open(path));
    auto& bp = batch(schema_batch);
    TRY(writer, parquet::arrow::FileWriter::Open(
        *bp->schema(), arrow::default_memory_pool(), out,
        parquet::default_writer_properties(),
        parquet::default_arrow_writer_properties()));
    return new HsParquetWriter{out, std::move(writer)};
}

// Returns 0 on success, -1 on error.
int arrow_hs_parquet_writer_write(void* w, void* b) {
    auto* pw = static_cast<HsParquetWriter*>(w);
    auto st = pw->writer->WriteRecordBatch(*batch(b));
    if (!st.ok()) { g_err = st.message(); return -1; }
    return 0;
}

int arrow_hs_parquet_writer_close(void* w) {
    auto* pw = static_cast<HsParquetWriter*>(w);
    auto st = pw->writer->Close();       if (!st.ok())  { g_err = st.message();  delete pw; return -1; }
    auto st2 = pw->out->Close();         if (!st2.ok()) { g_err = st2.message(); delete pw; return -1; }
    delete pw;
    return 0;
}

} // extern "C"
