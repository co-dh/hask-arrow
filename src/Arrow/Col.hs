module Arrow.Col
  ( Col(..)
  -- * Construction
  , ColMk(..)
  -- * Element access
  , ColGet(..)
  , toList
  -- * Access
  , len, nullCount, toString
  -- * Arithmetic
  , add, sub, mul, div, neg, abs
  -- * Comparison
  , eq, neq, lt, gt, lte, gte
  -- * Boolean logic
  , logAnd, logOr
  -- * Vector ops
  , filter, take, sort, unique, dropNull
  -- * Null handling
  , isNulls, isValids, fillNull
  -- * Conditional
  , ifElse
  -- * Aggregation
  , sum, mean, colMin, colMax, product
  -- * Array primitives (APL support)
  , iota, fillInt64, where_, concat, isIn, indexOf
  , cast, slice, scatter, scatterScalar
  , scan, ScanFn(..), cumulativeSum
  , reverseCol, sortIndices, replicate
  ) where

import Prelude hiding (filter, take, sum, div, abs, product, concat, replicate)

import Arrow.Dtype
import Arrow.FFI
import Arrow.Val (Val(..))
import Data.Kind            (Type)

import Control.Exception    (finally, throwIO)
import Data.Int             (Int64)
import Data.Word            (Word8)
import Foreign.ForeignPtr   (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (Storable, pokeElemOff)
import Foreign.C.String      (peekCString)

-- ---------------------------------------------------------------------------
-- Col GADT
-- ---------------------------------------------------------------------------

type Col :: Dtype -> Type
data Col d where
  MkCol :: ForeignPtr RawCol -> Col d

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

class ColMk (d :: Dtype) where
  mk :: [Maybe (HostType d)] -> IO (Col d)

-- Helper for Storable numeric types
mkNum :: Storable a
      => (Ptr a -> Ptr Word8 -> Int64 -> IO (Ptr RawCol))
      -> [Maybe a] -> IO (Col d)
mkNum rawMk xs = do
    let n = Prelude.length xs
    dPtr <- mallocArray (max 1 n)
    vPtr <- mallocArray (max 1 n)
    r <- (do
        sequence_ [case mx of
            Nothing -> pokeElemOff vPtr i 0
            Just v  -> pokeElemOff dPtr i v >> pokeElemOff vPtr i 1
          | (i, mx) <- zip [0..] xs]
        throwIfNull $ rawMk dPtr vPtr (fromIntegral n)
      ) `finally` (free dPtr >> free vPtr)
    MkCol <$> newForeignPtr rawColFreePtr r

instance ColMk 'Int8    where mk = mkNum rawMkInt8
instance ColMk 'Int16   where mk = mkNum rawMkInt16
instance ColMk 'Int32   where mk = mkNum rawMkInt32
instance ColMk 'Int64   where mk = mkNum rawMkInt64
instance ColMk 'UInt8   where mk = mkNum rawMkUInt8
instance ColMk 'UInt16  where mk = mkNum rawMkUInt16
instance ColMk 'UInt32  where mk = mkNum rawMkUInt32
instance ColMk 'UInt64  where mk = mkNum rawMkUInt64
instance ColMk 'Float32 where mk = mkNum rawMkFloat32
instance ColMk 'Float64 where mk = mkNum rawMkFloat64

instance ColMk 'Bool where
  mk xs = do
    let n = Prelude.length xs
    dPtr <- mallocArray (max 1 n) :: IO (Ptr Word8)
    vPtr <- mallocArray (max 1 n)
    r <- (do
        sequence_ [case mx of
            Nothing -> pokeElemOff vPtr i 0
            Just b  -> pokeElemOff dPtr i (if b then 1 else 0 :: Word8) >> pokeElemOff vPtr i 1
          | (i, mx) <- zip [0..] xs]
        throwIfNull $ rawMkBool dPtr vPtr (fromIntegral n)
      ) `finally` (free dPtr >> free vPtr)
    MkCol <$> newForeignPtr rawColFreePtr r

-- ---------------------------------------------------------------------------
-- Element access
-- ---------------------------------------------------------------------------

class ColGet (d :: Dtype) where
  get :: Col d -> Int -> IO (Maybe (HostType d))

getElem :: (Ptr RawCol -> Int64 -> IO a) -> Col d -> Int -> IO (Maybe a)
getElem rawGet (MkCol fp) i = withForeignPtr fp $ \p -> do
    v <- rawElemValid p (fromIntegral i)
    case v of
      (-1) -> throwIO $ ArrowError "index out of bounds"
      0    -> pure Nothing
      _    -> Just <$> rawGet p (fromIntegral i)

instance ColGet 'Int8    where get = getElem rawGetInt8
instance ColGet 'Int16   where get = getElem rawGetInt16
instance ColGet 'Int32   where get = getElem rawGetInt32
instance ColGet 'Int64   where get = getElem rawGetInt64
instance ColGet 'UInt8   where get = getElem rawGetUInt8
instance ColGet 'UInt16  where get = getElem rawGetUInt16
instance ColGet 'UInt32  where get = getElem rawGetUInt32
instance ColGet 'UInt64  where get = getElem rawGetUInt64
instance ColGet 'Float32 where get = getElem rawGetFloat32
instance ColGet 'Float64 where get = getElem rawGetFloat64

instance ColGet 'Bool where
  get (MkCol fp) i = withForeignPtr fp $ \p -> do
    v <- rawElemValid p (fromIntegral i)
    case v of
      (-1) -> throwIO $ ArrowError "index out of bounds"
      0    -> pure Nothing
      _    -> Just . (/= 0) <$> rawGetBool p (fromIntegral i)

toList :: ColGet d => Col d -> IO [Maybe (HostType d)]
toList c = do n <- len c; mapM (get c) [0..n-1]

-- ---------------------------------------------------------------------------
-- Access
-- ---------------------------------------------------------------------------

len :: Col d -> IO Int
len (MkCol fp) = withForeignPtr fp $ \p -> fromIntegral <$> rawColLen p

nullCount :: Col d -> IO Int
nullCount (MkCol fp) = withForeignPtr fp $ \p -> fromIntegral <$> rawColNullCount p

toString :: Col d -> IO String
toString (MkCol fp) = withForeignPtr fp $ \p -> do
    cs <- rawColToString p
    s <- peekCString cs
    rawStringFree cs
    pure s

-- ---------------------------------------------------------------------------
-- Compute helpers
-- ---------------------------------------------------------------------------

tri :: (Ptr RawCol -> Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol))
    -> Col a -> Col b -> Col c -> IO (Col d)
tri raw (MkCol a) (MkCol b) (MkCol c) =
    withForeignPtr a $ \pa -> withForeignPtr b $ \pb -> withForeignPtr c $ \pc -> do
        r <- throwIfNull $ raw pa pb pc
        MkCol <$> newForeignPtr rawColFreePtr r

bin :: (Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)) -> Col d -> Col e -> IO (Col f)
bin raw (MkCol a) (MkCol b) = withForeignPtr a $ \pa -> withForeignPtr b $ \pb -> do
    r <- throwIfNull $ raw pa pb
    MkCol <$> newForeignPtr rawColFreePtr r

una :: (Ptr RawCol -> IO (Ptr RawCol)) -> Col d -> IO (Col e)
una raw (MkCol a) = withForeignPtr a $ \pa -> do
    r <- throwIfNull $ raw pa
    MkCol <$> newForeignPtr rawColFreePtr r

aggr :: (Ptr RawCol -> IO (Ptr RawVal)) -> Col d -> IO (Val e)
aggr raw (MkCol a) = withForeignPtr a $ \pa -> do
    r <- throwIfNull $ raw pa
    MkVal <$> newForeignPtr rawValFreePtr r

-- ---------------------------------------------------------------------------
-- Arithmetic
-- ---------------------------------------------------------------------------

add, sub, mul, div :: IsNumeric d => Col d -> Col d -> IO (Col d)
add = bin rawAdd
sub = bin rawSub
mul = bin rawMul
div = bin rawDiv

neg, abs :: IsNumeric d => Col d -> IO (Col d)
neg = una rawNeg
abs = una rawAbs

-- ---------------------------------------------------------------------------
-- Comparison → Col 'Bool
-- ---------------------------------------------------------------------------

eq, neq :: Col d -> Col d -> IO (Col 'Bool)
eq  = bin rawEq
neq = bin rawNeq

-- ---------------------------------------------------------------------------
-- Boolean logic
-- ---------------------------------------------------------------------------

logAnd, logOr :: Col 'Bool -> Col 'Bool -> IO (Col 'Bool)
logAnd = bin rawLogAnd
logOr  = bin rawLogOr

-- ---------------------------------------------------------------------------
-- Comparison → Col 'Bool
-- ---------------------------------------------------------------------------

lt, gt, lte, gte :: IsOrd d => Col d -> Col d -> IO (Col 'Bool)
lt  = bin rawLt
gt  = bin rawGt
lte = bin rawLte
gte = bin rawGte

-- ---------------------------------------------------------------------------
-- Vector ops
-- ---------------------------------------------------------------------------

filter :: Col d -> Col 'Bool -> IO (Col d)
filter = bin rawFilter

take :: Col d -> Col idx -> IO (Col d)
take = bin rawTake

sort :: IsOrd d => Col d -> Bool -> IO (Col d)
sort (MkCol fp) asc = withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawSort p (if asc then 1 else 0)
    MkCol <$> newForeignPtr rawColFreePtr r

unique :: Col d -> IO (Col d)
unique = una rawUnique

dropNull :: Col d -> IO (Col d)
dropNull = una rawDropNull

-- ---------------------------------------------------------------------------
-- Null handling
-- ---------------------------------------------------------------------------

isNulls :: Col d -> IO (Col 'Bool)
isNulls = una rawIsNulls

isValids :: Col d -> IO (Col 'Bool)
isValids = una rawIsValids

fillNull :: Col d -> Col d -> IO (Col d)
fillNull = bin rawFillNull

-- ---------------------------------------------------------------------------
-- Conditional
-- ---------------------------------------------------------------------------

ifElse :: Col 'Bool -> Col d -> Col d -> IO (Col d)
ifElse = tri rawIfElse

-- ---------------------------------------------------------------------------
-- Aggregation
-- ---------------------------------------------------------------------------

sum :: IsNumeric d => Col d -> IO (Val d)
sum = aggr rawSum

mean :: IsNumeric d => Col d -> IO (Val 'Float64)
mean = aggr rawMean

colMin, colMax :: IsOrd d => Col d -> IO (Val d)
colMin = aggr rawMin
colMax = aggr rawMax

product :: IsNumeric d => Col d -> IO (Val d)
product = aggr rawProduct

-- ---------------------------------------------------------------------------
-- Array primitives (APL support)
-- ---------------------------------------------------------------------------

-- | @iota n@ = [0, 1, ..., n-1]
iota :: Int64 -> IO (Col 'Int64)
iota n = do r <- throwIfNull $ rawIota n; MkCol <$> newForeignPtr rawColFreePtr r

-- | @fillInt64 n v@ = n copies of v
fillInt64 :: Int64 -> Int64 -> IO (Col 'Int64)
fillInt64 n v = do r <- throwIfNull $ rawFillInt64 n v; MkCol <$> newForeignPtr rawColFreePtr r

-- | Indices of true values in boolean mask
where_ :: Col 'Bool -> IO (Col 'Int64)
where_ = una rawWhere

-- | Concatenate two columns
concat :: Col d -> Col d -> IO (Col d)
concat = bin rawConcat

-- | Element-wise membership test
isIn :: Col d -> Col d -> IO (Col 'Bool)
isIn = bin rawIsIn

-- | For each needle, position in haystack (null if absent)
indexOf :: Col d -> Col d -> IO (Col 'Int32)
indexOf = bin rawIndexOf

-- | Type cast. Target dtype encoded as Word8 matching Dtype constructor order.
cast :: Word8 -> Col d -> IO (Col e)
cast target (MkCol fp) = withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawCast p target
    MkCol <$> newForeignPtr rawColFreePtr r

-- | Array slice at offset with length
slice :: Col d -> Int64 -> Int64 -> IO (Col d)
slice (MkCol fp) off n = withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawSlice p off n
    MkCol <$> newForeignPtr rawColFreePtr r

-- | Scatter: @scatter target indices values@ — writes values at indices into target
scatter :: Col d -> Col 'Int64 -> Col d -> IO (Col d)
scatter = tri rawScatter

-- | Scatter scalar: @scatterScalar target indices val@ (Int64 only)
scatterScalar :: Col 'Int64 -> Col 'Int64 -> Int64 -> IO (Col 'Int64)
scatterScalar (MkCol fp1) (MkCol fp2) v =
    withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> do
        r <- throwIfNull $ rawScatterScalar p1 p2 v
        MkCol <$> newForeignPtr rawColFreePtr r

-- | Scan function tags
data ScanFn = SfAdd | SfMul | SfMax | SfMin

scanTag :: ScanFn -> Word8
scanTag SfAdd = 0; scanTag SfMul = 1; scanTag SfMax = 2; scanTag SfMin = 3

-- | Cumulative scan with function selector (Int64 only)
scan :: ScanFn -> Col 'Int64 -> IO (Col 'Int64)
scan f (MkCol fp) = withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawScan (scanTag f) p
    MkCol <$> newForeignPtr rawColFreePtr r

-- | Cumulative sum (any numeric type, uses Arrow compute)
cumulativeSum :: IsNumeric d => Col d -> IO (Col d)
cumulativeSum = una rawCumulativeSum

-- | Reverse column
reverseCol :: Col d -> IO (Col d)
reverseCol = una rawReverse

-- | Sort indices (returns permutation)
sortIndices :: IsOrd d => Col d -> Bool -> IO (Col 'Int64)
sortIndices (MkCol fp) asc = withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawSortIndices p (if asc then 1 else 0)
    MkCol <$> newForeignPtr rawColFreePtr r

-- | APL replicate: expand col by integer counts
replicate :: Col d -> Col 'Int64 -> IO (Col d)
replicate = bin rawReplicate
