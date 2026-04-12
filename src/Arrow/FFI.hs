module Arrow.FFI
  ( RawCol, RawVal
  , ArrowError(..)
  , throwIfNull
  -- * Lifecycle
  , rawColFreePtr, rawValFreePtr, rawStringFree
  -- * Construction
  , rawMkBool
  , rawMkInt8,  rawMkInt16,  rawMkInt32,  rawMkInt64
  , rawMkUInt8, rawMkUInt16, rawMkUInt32, rawMkUInt64
  , rawMkFloat32, rawMkFloat64
  , rawMkUtf8
  -- * Access
  , rawColLen, rawColNullCount, rawColToString
  , rawElemValid
  , rawGetBool
  , rawGetInt8,  rawGetInt16,  rawGetInt32,  rawGetInt64
  , rawGetUInt8, rawGetUInt16, rawGetUInt32, rawGetUInt64
  , rawGetFloat32, rawGetFloat64
  , rawGetUtf8
  -- * Compute
  , rawAdd, rawSub, rawMul, rawDiv, rawNeg, rawAbs
  , rawEq, rawNeq, rawLt, rawGt, rawLte, rawGte
  , rawFilter, rawTake, rawFillNull
  , rawUnique, rawDropNull, rawIsNulls, rawIsValids
  , rawSort
  , rawLogAnd, rawLogOr, rawIfElse
  , rawSum, rawMean, rawMin, rawMax, rawProduct
  -- * Array primitives
  , rawIota, rawFillInt64, rawWhere, rawConcat
  , rawIsIn, rawIndexOf, rawCast, rawSlice
  , rawScatter, rawScatterScalar, rawScan, rawCumulativeSum
  , rawReverse, rawSortIndices, rawReplicate
  -- * Val access
  , rawValIsValid, rawValToString
  , rawValGetInt8,  rawValGetInt16,  rawValGetInt32,  rawValGetInt64
  , rawValGetUInt8, rawValGetUInt16, rawValGetUInt32, rawValGetUInt64
  , rawValGetFloat32, rawValGetFloat64
  ) where

import Control.Exception (Exception, throwIO)
import Data.Int          (Int8, Int16, Int32, Int64)
import Data.Word         (Word8, Word16, Word32, Word64)
import Foreign.C.String  (CString, peekCString)
import Foreign.Ptr       (Ptr, FunPtr, nullPtr)

-- | Opaque C types — never dereferenced from Haskell
data RawCol
data RawVal

-- | Arrow compute error propagated from the C++ shim
newtype ArrowError = ArrowError String deriving (Show)
instance Exception ArrowError

foreign import ccall "arrow_hs_last_error" rawLastError :: IO CString

throwIfNull :: IO (Ptr a) -> IO (Ptr a)
throwIfNull act = do
    p <- act
    if p == nullPtr then do
        msg <- peekCString =<< rawLastError
        throwIO (ArrowError msg)
    else pure p

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

foreign import ccall "&arrow_hs_col_free"  rawColFreePtr  :: FunPtr (Ptr RawCol -> IO ())
foreign import ccall "&arrow_hs_val_free"  rawValFreePtr  :: FunPtr (Ptr RawVal -> IO ())
foreign import ccall "arrow_hs_string_free" rawStringFree :: Ptr a -> IO ()

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

foreign import ccall "arrow_hs_mk_bool"    rawMkBool    :: Ptr Word8  -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_int8"    rawMkInt8    :: Ptr Int8   -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_int16"   rawMkInt16   :: Ptr Int16  -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_int32"   rawMkInt32   :: Ptr Int32  -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_int64"   rawMkInt64   :: Ptr Int64  -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_uint8"   rawMkUInt8   :: Ptr Word8  -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_uint16"  rawMkUInt16  :: Ptr Word16 -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_uint32"  rawMkUInt32  :: Ptr Word32 -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_uint64"  rawMkUInt64  :: Ptr Word64 -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_float32" rawMkFloat32 :: Ptr Float  -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_float64" rawMkFloat64 :: Ptr Double -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mk_utf8"    rawMkUtf8    :: Ptr CString -> Ptr Int64 -> Ptr Word8 -> Int64 -> IO (Ptr RawCol)

-- ---------------------------------------------------------------------------
-- Access
-- ---------------------------------------------------------------------------

foreign import ccall "arrow_hs_col_len"        rawColLen       :: Ptr RawCol -> IO Int64
foreign import ccall "arrow_hs_col_null_count"  rawColNullCount :: Ptr RawCol -> IO Int64
foreign import ccall "arrow_hs_col_to_string"   rawColToString  :: Ptr RawCol -> IO CString
foreign import ccall "arrow_hs_elem_valid"      rawElemValid    :: Ptr RawCol -> Int64 -> IO Int8

foreign import ccall "arrow_hs_get_bool"    rawGetBool    :: Ptr RawCol -> Int64 -> IO Word8
foreign import ccall "arrow_hs_get_int8"    rawGetInt8    :: Ptr RawCol -> Int64 -> IO Int8
foreign import ccall "arrow_hs_get_int16"   rawGetInt16   :: Ptr RawCol -> Int64 -> IO Int16
foreign import ccall "arrow_hs_get_int32"   rawGetInt32   :: Ptr RawCol -> Int64 -> IO Int32
foreign import ccall "arrow_hs_get_int64"   rawGetInt64   :: Ptr RawCol -> Int64 -> IO Int64
foreign import ccall "arrow_hs_get_uint8"   rawGetUInt8   :: Ptr RawCol -> Int64 -> IO Word8
foreign import ccall "arrow_hs_get_uint16"  rawGetUInt16  :: Ptr RawCol -> Int64 -> IO Word16
foreign import ccall "arrow_hs_get_uint32"  rawGetUInt32  :: Ptr RawCol -> Int64 -> IO Word32
foreign import ccall "arrow_hs_get_uint64"  rawGetUInt64  :: Ptr RawCol -> Int64 -> IO Word64
foreign import ccall "arrow_hs_get_float32" rawGetFloat32 :: Ptr RawCol -> Int64 -> IO Float
foreign import ccall "arrow_hs_get_float64" rawGetFloat64 :: Ptr RawCol -> Int64 -> IO Double
foreign import ccall "arrow_hs_get_utf8"    rawGetUtf8    :: Ptr RawCol -> Int64 -> Ptr Int64 -> IO CString

-- ---------------------------------------------------------------------------
-- Compute
-- ---------------------------------------------------------------------------

foreign import ccall "arrow_hs_add" rawAdd :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_sub" rawSub :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_mul" rawMul :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_div" rawDiv :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_neg" rawNeg :: Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_abs" rawAbs :: Ptr RawCol -> IO (Ptr RawCol)

foreign import ccall "arrow_hs_eq"  rawEq  :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_neq" rawNeq :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_lt"  rawLt  :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_gt"  rawGt  :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_lte" rawLte :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_gte" rawGte :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)

foreign import ccall "arrow_hs_filter"    rawFilter   :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_take"      rawTake     :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_fill_null" rawFillNull :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_unique"    rawUnique   :: Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_drop_null" rawDropNull :: Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_is_nulls"  rawIsNulls  :: Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_is_valids" rawIsValids :: Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_sort"      rawSort     :: Ptr RawCol -> Word8 -> IO (Ptr RawCol)

-- Boolean logic
foreign import ccall "arrow_hs_log_and" rawLogAnd :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_log_or"  rawLogOr  :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)

-- Conditional
foreign import ccall "arrow_hs_if_else" rawIfElse :: Ptr RawCol -> Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)

-- Aggregation
foreign import ccall "arrow_hs_sum"     rawSum     :: Ptr RawCol -> IO (Ptr RawVal)
foreign import ccall "arrow_hs_mean"    rawMean    :: Ptr RawCol -> IO (Ptr RawVal)
foreign import ccall "arrow_hs_min"     rawMin     :: Ptr RawCol -> IO (Ptr RawVal)
foreign import ccall "arrow_hs_max"     rawMax     :: Ptr RawCol -> IO (Ptr RawVal)
foreign import ccall "arrow_hs_product" rawProduct :: Ptr RawCol -> IO (Ptr RawVal)

-- Array primitives (APL support)
foreign import ccall "arrow_hs_iota"           rawIota          :: Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_fill_int64"     rawFillInt64     :: Int64 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_where"          rawWhere         :: Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_concat"         rawConcat        :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_is_in"          rawIsIn          :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_index_of"       rawIndexOf       :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_cast"           rawCast          :: Ptr RawCol -> Word8 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_slice"          rawSlice         :: Ptr RawCol -> Int64 -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_scatter"        rawScatter       :: Ptr RawCol -> Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_scatter_scalar" rawScatterScalar :: Ptr RawCol -> Ptr RawCol -> Int64 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_scan"           rawScan          :: Word8 -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_cumulative_sum" rawCumulativeSum :: Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_reverse"        rawReverse       :: Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_sort_indices"   rawSortIndices   :: Ptr RawCol -> Word8 -> IO (Ptr RawCol)
foreign import ccall "arrow_hs_replicate"      rawReplicate     :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)

-- ---------------------------------------------------------------------------
-- Val access
-- ---------------------------------------------------------------------------

foreign import ccall "arrow_hs_val_is_valid"  rawValIsValid  :: Ptr RawVal -> IO Word8
foreign import ccall "arrow_hs_val_to_string" rawValToString :: Ptr RawVal -> IO CString

foreign import ccall "arrow_hs_val_get_int8"    rawValGetInt8    :: Ptr RawVal -> IO Int8
foreign import ccall "arrow_hs_val_get_int16"   rawValGetInt16   :: Ptr RawVal -> IO Int16
foreign import ccall "arrow_hs_val_get_int32"   rawValGetInt32   :: Ptr RawVal -> IO Int32
foreign import ccall "arrow_hs_val_get_int64"   rawValGetInt64   :: Ptr RawVal -> IO Int64
foreign import ccall "arrow_hs_val_get_uint8"   rawValGetUInt8   :: Ptr RawVal -> IO Word8
foreign import ccall "arrow_hs_val_get_uint16"  rawValGetUInt16  :: Ptr RawVal -> IO Word16
foreign import ccall "arrow_hs_val_get_uint32"  rawValGetUInt32  :: Ptr RawVal -> IO Word32
foreign import ccall "arrow_hs_val_get_uint64"  rawValGetUInt64  :: Ptr RawVal -> IO Word64
foreign import ccall "arrow_hs_val_get_float32" rawValGetFloat32 :: Ptr RawVal -> IO Float
foreign import ccall "arrow_hs_val_get_float64" rawValGetFloat64 :: Ptr RawVal -> IO Double
