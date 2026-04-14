module Arrow.Parquet
  ( -- * Reader
    Reader
  , open, close, withReader
  , numRows, numCols
  , colName, colDtype
  , nextBatch
  , foldBatches
  , readAll
    -- * Writer
  , Writer
  , writerOpen, writerClose, withWriter
  , writerWrite
  , writeBatches
  ) where

import Arrow.Batch  (Batch, fromRawPtr, withBatchPtr)
import Arrow.Dtype  (Dtype, dtypeFromCodeOrThrow)
import Arrow.FFI

import Control.Exception    (bracket)
import Control.Monad        (unless, (>=>))
import Data.Foldable        (traverse_)
import Data.Int             (Int64)
import Data.List.NonEmpty   (NonEmpty(..))
import Foreign.C.String     (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr          (Ptr)
import Foreign.Storable     (peek)

newtype Reader = MkReader (Ptr RawParquetReader)

-- | Open a Parquet file. @batchSize@ is the target number of rows per
-- emitted 'Batch' (0 = Arrow's default).
open :: FilePath -> Int64 -> IO Reader
open path batchSize = withCString path $ \cs ->
    MkReader <$> throwIfNull (rawParquetOpen cs batchSize)

close :: Reader -> IO ()
close (MkReader p) = rawParquetClose p

withReader :: FilePath -> Int64 -> (Reader -> IO a) -> IO a
withReader path bs = bracket (open path bs) close

numRows :: Reader -> IO Int64
numRows (MkReader p) = rawParquetNumRows p

numCols :: Reader -> IO Int
numCols (MkReader p) = fromIntegral <$> rawParquetNumCols p

colName :: Reader -> Int -> IO String
colName (MkReader p) i = peekCStringFree =<< rawParquetColName p (fromIntegral i)

colDtype :: Reader -> Int -> IO Dtype
colDtype (MkReader p) i = dtypeFromCodeOrThrow =<< rawParquetColType p (fromIntegral i)

-- | Fetch the next batch, or 'Nothing' at end-of-stream.
nextBatch :: Reader -> IO (Maybe Batch)
nextBatch (MkReader p) = alloca $ \outPtr -> do
    st <- rawParquetNextBatch p outPtr
    case st of
        0 -> Just <$> fromRawPtr (peek outPtr)
        1 -> pure Nothing
        _ -> throwLastError

-- | Consume the reader to the end, folding each batch into an accumulator.
-- Bounded memory — the previous batch becomes garbage after @f@ returns.
foldBatches :: Reader -> a -> (a -> Batch -> IO a) -> IO a
foldBatches r z f = go z
  where
    go !acc = nextBatch r >>= maybe (pure acc) (f acc >=> go)

-- | Collect all batches into a list. Unbounded memory — prefer 'foldBatches'
-- on real data; this exists for tests and small files.
readAll :: Reader -> IO [Batch]
readAll r = reverse <$> foldBatches r [] (\acc b -> pure (b : acc))

newtype Writer = MkWriter (Ptr RawParquetWriter)

-- | Open a writer. The schema is taken from the sample batch; all batches
-- passed to 'writerWrite' must share that schema.
writerOpen :: FilePath -> Batch -> IO Writer
writerOpen path b = withCString path $ \cs ->
    withBatchPtr b $ \bp ->
        MkWriter <$> throwIfNull (rawParquetWriterOpen cs bp)

writerWrite :: Writer -> Batch -> IO ()
writerWrite (MkWriter w) b = withBatchPtr b $ \bp ->
    rawParquetWriterWrite w bp >>= \st -> unless (st == 0) throwLastError

writerClose :: Writer -> IO ()
writerClose (MkWriter w) =
    rawParquetWriterClose w >>= \st -> unless (st == 0) throwLastError

withWriter :: FilePath -> Batch -> (Writer -> IO a) -> IO a
withWriter path schema = bracket (writerOpen path schema) writerClose

-- | Write all batches in order. The first batch supplies the schema.
writeBatches :: FilePath -> NonEmpty Batch -> IO ()
writeBatches path bs@(b :| _) = withWriter path b $ \w -> traverse_ (writerWrite w) bs
