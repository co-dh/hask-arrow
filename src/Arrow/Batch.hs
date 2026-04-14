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

import Control.Exception    (bracket)
import Foreign.C.String     (CString, newCString)
import Foreign.ForeignPtr   (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr          (Ptr)

-- | Opaque handle to an Arrow RecordBatch. Row/column counts are cached so
-- introspection is FFI-free after construction.
data Batch = MkBatch !Int !Int !(ForeignPtr RawBatch)

numRows :: Batch -> Int
numRows (MkBatch r _ _) = r

numCols :: Batch -> Int
numCols (MkBatch _ c _) = c

withBatchPtr :: Batch -> (Ptr RawBatch -> IO a) -> IO a
withBatchPtr (MkBatch _ _ fp) = withForeignPtr fp

colName :: Batch -> Int -> String
colName (MkBatch _ _ fp) i = pu $ withForeignPtr fp $ \p ->
    rawBatchColName p (fromIntegral i) >>= peekCStringFree

colDtype :: Batch -> Int -> Dtype
colDtype (MkBatch _ _ fp) i = pu $ withForeignPtr fp $ \p ->
    rawBatchColType p (fromIntegral i) >>= dtypeFromCodeOrThrow

-- | Reinterpret column @i@ as @Col d@. The caller is responsible for choosing
-- a 'd' that matches 'colDtype'; a mismatch is undefined behavior inside the
-- C++ shim.
unsafeCol :: Batch -> Int -> Col d
unsafeCol (MkBatch _ _ fp) i = pu $ withForeignPtr fp $ \p ->
    wrapCol (rawBatchCol p (fromIntegral i))

-- | A name plus a column of any element type. The existential hides 'd' so
-- heterogeneously-typed columns fit in one list.
data NamedCol where
    NamedCol :: String -> Col d -> NamedCol

-- | Build a RecordBatch. All columns must share a length.
fromCols :: [NamedCol] -> Batch
fromCols ncs = pu $
    withCStrings  names $ \cstrs   ->
    withForeignPtrs cfps $ \ps      ->
    withArray     cstrs $ \namesPtr ->
    withArray     ps    $ \colsPtr  ->
        fromRawPtr (rawBatchMake namesPtr colsPtr (fromIntegral (length ncs)))
  where
    (names, cfps) = unzip [(s, f) | NamedCol s (MkCol f) <- ncs]

-- | Wrap a raw batch pointer, caching row/column counts.
fromRawPtr :: IO (Ptr RawBatch) -> IO Batch
fromRawPtr act = do
    raw <- throwIfNull act
    fp  <- newForeignPtr rawBatchFreePtr raw
    withForeignPtr fp $ \p ->
        MkBatch
            <$> (fromIntegral <$> rawBatchNumRows p)
            <*> (fromIntegral <$> rawBatchNumCols p)
            <*> pure fp

-- | Bracketed @newCString@ over a list — frees every C string on exit.
withCStrings :: [String] -> ([CString] -> IO r) -> IO r
withCStrings []     k = k []
withCStrings (s:ss) k =
    bracket (newCString s) free $ \c ->
        withCStrings ss (k . (c:))

-- | CPS-fold @withForeignPtr@ over a list, accumulating raw pointers.
withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO r) -> IO r
withForeignPtrs []     k = k []
withForeignPtrs (f:fs) k = withForeignPtr f $ \p -> withForeignPtrs fs (k . (p:))
