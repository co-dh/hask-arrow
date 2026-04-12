# hask-arrow

Type-safe Haskell bindings to Apache Arrow, focused on high-performance columnar data processing.

## Goal

Process large parquet files (NYSE tick data, billion-row taxi datasets) in Haskell. Requirements:
- **Parquet I/O** — native read/write via Arrow's `parquet::arrow` API
- **Streaming** — RecordBatch-level iteration, never load the whole file
- **Bounded memory** — process billion rows in constant space
- **Fast** — Arrow's SIMD-optimized C++ compute kernels, not pure Haskell

## Architecture

```
Parquet file
  → ParquetReader (streams RecordBatches, e.g. 64K rows each)
  → per-batch Col operations (filter, scan, gather, aggregate)
  → ParquetWriter / result
```

- **Col d** — GADT wrapping a single Arrow array, indexed by Dtype at the type level
- **RecordBatch** — multiple named columns (one chunk of a table)
- **C++ FFI shim** (`ffi/arrow_hs.cpp`) — thin wrapper calling Arrow compute kernels
- **unsafePerformIO** — Col operations are pure (immutable arrays in, new array out);
  IO is only an FFI artifact. Wrap with unsafePerformIO so Col supports Num/Eq/Ord instances.

## Module layout

| Module      | Purpose                                              |
|-------------|------------------------------------------------------|
| Arrow.Dtype | Dtype kind, HostType family, constraint classes       |
| Arrow.FFI   | Foreign imports (one per C++ entry point)             |
| Arrow.Col   | Col GADT, pure infix operators via unsafePerformIO    |
| Arrow.Val   | Scalar value wrapper                                  |
| Arrow.Apl   | APL-style operators, literal PS.apl translations      |

## Build

```
cabal build      # needs libarrow, libarrow_compute, libparquet
cabal test        # smoke tests
```
