module Arrow.Batch
  ( Batch(..)
  , numRows, numCols
  , colName, colDtype
  , unsafeCol
  , NamedCol(..)
  , fromCols
  , fromRawPtr
  , withBatchPtr
  ) where

import Arrow.Col    (Col(..), wrapCol)
import Arrow.Dtype  (Dtype, dtypeFromCodeOrThrow)
import Arrow.FFI

import Foreign.C.String     (withCString)
import Foreign.ForeignPtr   (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr          (Ptr)

newtype Batch = MkBatch (ForeignPtr RawBatch)

withBatchPtr :: Batch -> (Ptr RawBatch -> IO a) -> IO a
withBatchPtr (MkBatch fp) = withForeignPtr fp

numRows, numCols :: Batch -> Int
numRows b = pu $ withBatchPtr b (fmap fromIntegral . rawBatchNumRows)
numCols b = pu $ withBatchPtr b (fmap fromIntegral . rawBatchNumCols)

colName :: Batch -> Int -> String
colName b i = pu $ withBatchPtr b $ \p ->
    rawBatchColName p (fromIntegral i) >>= peekCStringFree

colDtype :: Batch -> Int -> Dtype
colDtype b i = pu $ withBatchPtr b $ \p ->
    rawBatchColType p (fromIntegral i) >>= dtypeFromCodeOrThrow

-- | Reinterpret column @i@ as @Col d@. The caller must choose a 'd' matching
-- 'colDtype'; a mismatch is undefined behavior in the C++ shim.
unsafeCol :: Batch -> Int -> Col d
unsafeCol b i = pu $ withBatchPtr b $ \p ->
    wrapCol (rawBatchCol p (fromIntegral i))

data NamedCol where
    NamedCol :: String -> Col d -> NamedCol

-- | All columns must share a length.
fromCols :: [NamedCol] -> Batch
fromCols ncs = pu $
    withMany withCString    names $ \cstrs   ->
    withMany withForeignPtr cfps  $ \ps      ->
    withArray cstrs               $ \namesPtr ->
    withArray ps                  $ \colsPtr  ->
        fromRawPtr (rawBatchMake namesPtr colsPtr (fromIntegral (length ncs)))
  where
    (names, cfps) = unzip [(s, f) | NamedCol s (MkCol f) <- ncs]

fromRawPtr :: IO (Ptr RawBatch) -> IO Batch
fromRawPtr act = MkBatch <$> (throwIfNull act >>= newForeignPtr rawBatchFreePtr)
