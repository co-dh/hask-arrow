# hask-arrow: Type-Safe Haskell Bindings to Apache Arrow

## Goal

Haskell bindings to Apache Arrow that enforce Arrow type safety at compile time via GADTs and type-level
programming. Same architecture as lean-arrow but using GHC's type system instead of Lean's dependent types.

## Why not just use `dataframe`?

The Haskell `dataframe` library (DataHaskell, 1.0.0.0, March 2026) is a pure-Haskell columnar DataFrame:

- **Custom format, not Arrow** — uses `Vector`/`UVector` internally, not Arrow memory layout
- **Typed API** — tracks schema at compile time via Template Haskell + type-level strings
- **Arrow C Data Interface** — only for interop (Python/Polars bridge), not the internal representation
- **Rationale** — pure Haskell gives GHC fusion/unboxing; Arrow backend would mean FFI overhead on every op
  or re-implementing Arrow compute in Haskell

Our approach is different: **use Arrow's actual compute kernels** via a thin C++ FFI shim, and use Haskell's type
system to make the API safe. We get Arrow's optimized C++ compute (SIMD, vectorized) while GHC enforces that you
can't add a string column to an int column.

## Architecture

```
Haskell (Arrow.Col, Arrow.Val)    — GADT-indexed: Col (d :: Dtype), Val (d :: Dtype)
    | Foreign.Ptr to C++ shared_ptr<arrow::Array>
C++ shim (ffi/arrow_hs.cpp, extern "C")
    | arrow::compute::*
Arrow C++ (libarrow)              — compute kernels, memory management
```

Same as lean-arrow:
- Haskell ↔ C boundary: opaque `ForeignPtr` wrapping `shared_ptr<arrow::Array>`
- C++ stays inside the shim: `extern "C"` functions import → compute → return opaque pointer
- GADT-indexed types: `Col 'Int64`, `Col 'Float64` — can't mix at compile time

## Type Design

```haskell
-- Phantom-indexed column type
data Dtype = Bool | Int8 | Int16 | Int32 | Int64
           | UInt8 | UInt16 | UInt32 | UInt64
           | Float32 | Float64
           | Utf8 | Binary
           | Date32 | Date64
           deriving (Show, Eq)

-- GADT for compile-time dtype witness
type Col :: Dtype -> Type
data Col d where
  MkCol :: ForeignPtr RawCol -> Col d

type Val :: Dtype -> Type
data Val d where
  MkVal :: ForeignPtr RawVal -> Val d

-- Type family mapping Dtype to Haskell host type
type family HostType (d :: Dtype) :: Type where
  HostType 'Bool    = Bool
  HostType 'Int32   = Int32
  HostType 'Int64   = Int64
  HostType 'Float64 = Double
  HostType 'Utf8    = Text
  -- etc.

-- Typeclass gating arithmetic to numeric types
type IsNumeric :: Dtype -> Constraint
class IsNumeric d
instance IsNumeric 'Int8
instance IsNumeric 'Int32
instance IsNumeric 'Int64
instance IsNumeric 'Float64
-- etc.

-- API examples:
mk      :: [Maybe (HostType d)] -> IO (Col d)    -- needs TypeApplications: mk @'Int64 [Just 1, Nothing, Just 3]
len     :: Col d -> IO Int
add     :: IsNumeric d => Col d -> Col d -> IO (Col d)
eq      :: Col d -> Col d -> IO (Col 'Bool)
filter  :: Col d -> Col 'Bool -> IO (Col d)
sum     :: IsNumeric d => Col d -> IO (Val d)
sort    :: IsOrd d => Col d -> IO (Col d)
```

## C++ Shim Pattern

Same as lean-arrow but adapted for GHC FFI:

```cpp
// Opaque pointer — Haskell sees void*
typedef struct { std::shared_ptr<arrow::Array>* ptr; } ColData;

extern "C" {
  // Haskell calls these via FFI import
  ColData* arrow_col_mk_int64(int64_t* data, uint8_t* nulls, int64_t len);
  int64_t  arrow_col_len(ColData* col);
  ColData* arrow_col_add(ColData* a, ColData* b);
  ColData* arrow_col_filter(ColData* col, ColData* mask);
  void     arrow_col_free(ColData* col);
  // ... same set of compute kernels as lean-arrow
}
```

Haskell side:
```haskell
foreign import ccall "arrow_col_add"   rawAdd    :: Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)
foreign import ccall "&arrow_col_free" rawFree   :: FunPtr (Ptr RawCol -> IO ())

add :: IsNumeric d => Col d -> Col d -> IO (Col d)
add (MkCol fp1) (MkCol fp2) =
  withForeignPtr fp1 $ \p1 ->
  withForeignPtr fp2 $ \p2 -> do
    p <- rawAdd p1 p2
    MkCol <$> newForeignPtr rawFree p
```

## File Structure

```
hask-arrow/
├── hask-arrow.cabal
├── PLAN.md
├── src/
│   └── Arrow/
│       ├── Dtype.hs      — Dtype kind, IsNumeric, IsOrd, HostType
│       ├── FFI.hs        — raw foreign imports
│       ├── Col.hs        — Col d GADT + typed API
│       └── Val.hs        — Val d scalar type
├── ffi/
│   └── arrow_hs.cpp      — extern "C" shim (same kernels as lean-arrow)
└── test/
    └── Main.hs           — smoke tests
```

## Implementation Order

1. Cabal project + Dtype.hs (kind, type families, constraints)
2. ffi/arrow_hs.cpp — init, int64 constructor, len, free (port from lean-arrow's arrow_lean.cpp)
3. Arrow.FFI — raw foreign imports
4. Arrow.Col — GADT wrapper, mk, len, toString
5. test/Main.hs — smoke: create int64 col, get length
6. Arithmetic compute (add/sub/mul/div)
7. Comparison compute (eq/lt/gt) -> Col 'Bool
8. Vector ops (filter/take/sort/unique)
9. Aggregation (sum/min/max/mean) -> Val d
10. String, temporal, remaining types

## Key Differences from lean-arrow

| Aspect           | lean-arrow                          | hask-arrow                             |
|------------------|-------------------------------------|----------------------------------------|
| Type indexing    | `Col d` where `d : Dtype`          | `Col (d :: Dtype)` GADT                |
| Constraints      | `class IsNumeric (d : Dtype)`       | `type IsNumeric :: Dtype -> Constraint` |
| Host type map    | overloaded `ColMk` typeclass        | `type family HostType d`               |
| Memory           | `lean_external_object`              | `ForeignPtr` + C finalizer             |
| Monad            | `IO` everywhere                     | `IO` (could lift to `MonadIO m =>`)    |
| Build            | Lake                                | Cabal                                  |
| C++ shim         | ~identical                          | ~identical                             |

## Prerequisites

```bash
sudo pacman -S arrow ghc cabal-install
```
