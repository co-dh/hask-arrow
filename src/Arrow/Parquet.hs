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
import Control.Monad        ((>=>))
import Data.Foldable        (traverse_)
import Data.Int             (Int64)
import Data.List.NonEmpty   (NonEmpty(..))
import Foreign.C.String     (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr          (Ptr)
import Foreign.Storable     (peek)

newtype Reader = MkReader (Ptr RawParquetReader)

-- | @batchSize@: target rows per emitted 'Batch' (0 = Arrow default).
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

nextBatch :: Reader -> IO (Maybe Batch)
nextBatch (MkReader p) = alloca $ \outPtr -> do
    st <- rawParquetNextBatch p outPtr
    case st of
        0 -> Just <$> fromRawPtr (peek outPtr)
        1 -> pure Nothing
        _ -> throwLastError

-- | Bounded memory — each batch becomes garbage after @f@ returns.
foldBatches :: Reader -> a -> (a -> Batch -> IO a) -> IO a
foldBatches r z f = go z
  where
    go !acc = nextBatch r >>= maybe (pure acc) (f acc >=> go)

-- | Unbounded memory — tests and small files only; use 'foldBatches' otherwise.
readAll :: Reader -> IO [Batch]
readAll r = reverse <$> foldBatches r [] (\acc b -> pure (b : acc))

newtype Writer = MkWriter (Ptr RawParquetWriter)

-- | Schema is taken from the sample batch; all subsequent batches must match.
writerOpen :: FilePath -> Batch -> IO Writer
writerOpen path b = withCString path $ \cs ->
    withBatchPtr b $ \bp ->
        MkWriter <$> throwIfNull (rawParquetWriterOpen cs bp)

writerWrite :: Writer -> Batch -> IO ()
writerWrite (MkWriter w) b = withBatchPtr b $ checkStatus . rawParquetWriterWrite w

writerClose :: Writer -> IO ()
writerClose (MkWriter w) = checkStatus (rawParquetWriterClose w)

withWriter :: FilePath -> Batch -> (Writer -> IO a) -> IO a
withWriter path schema = bracket (writerOpen path schema) writerClose

writeBatches :: FilePath -> NonEmpty Batch -> IO ()
writeBatches path bs@(b :| _) = withWriter path b $ \w -> traverse_ (writerWrite w) bs
