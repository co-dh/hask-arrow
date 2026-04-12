-- | Type-safe column handle wrapping an Arrow array via C++ FFI.
--
-- Col is a GADT indexed by 'Dtype' — each column carries its Arrow type
-- at the Haskell type level. All operations go through the C++ shim
-- (ffi\/arrow_hs.cpp) which calls Arrow compute kernels.
--
-- Arrow kernels are referentially transparent (immutable arrays in, new
-- array out), so all operations are pure — IO is hidden via
-- unsafeDupablePerformIO.  Construction ('mk') is the only IO entry point.
module Arrow.Col
  ( Col(..)
  -- * Construction
  , ColMk(..)
  -- * Element access
  , ColGet(..)
  , toList
  -- * Access
  , len, nullCount, toString
  -- * Arithmetic (Num instance provides +, -, *, negate, abs, signum, fromInteger)
  , (/.)
  -- * Comparison
  , (==.), (/=.), (<.), (>.), (<=.), (>=.)
  -- * Boolean logic
  , (&&.), (||.)
  -- * Vector ops
  , filter, take, sort, unique, dropNull
  -- * Null handling
  , isNulls, isValids, fillNull
  -- * Conditional
  , ifElse
  -- * Aggregation
  , sum, mean, colMin, colMax, product
  -- * Array primitives
  , iota, fillInt64, where_, concat, isIn, indexOf
  , cast, slice, scatter, scatterScalar
  , scan, ScanFn(..), cumulativeSum
  , reverseCol, sortIndices, expand
  ) where

import Prelude hiding (filter, take, sum, product, concat)

import Arrow.Dtype
import Arrow.FFI
import Arrow.Val (Val(..))
import Data.Kind            (Type)

import Control.Exception     (finally)
import Data.Int             (Int64)
import Data.Word            (Word8)
import Foreign.ForeignPtr   (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (Storable, pokeElemOff)
import Foreign.C.String      (peekCString)
import System.IO.Unsafe      (unsafeDupablePerformIO)

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
  mk :: [Maybe (HostType d)] -> Col d

mkNum :: Storable a
      => (Ptr a -> Ptr Word8 -> Int64 -> IO (Ptr RawCol))
      -> [Maybe a] -> Col d
mkNum rawMk xs = pu $ do
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
  mk xs = pu $ do
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
  get :: Col d -> Int -> Maybe (HostType d)

getElem :: (Ptr RawCol -> Int64 -> IO a) -> Col d -> Int -> Maybe a
getElem rawGet (MkCol fp) i = pu $ withForeignPtr fp $ \p -> do
    v <- rawElemValid p (fromIntegral i)
    case v of
      (-1) -> error "Col.get: index out of bounds"
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
  get (MkCol fp) i = pu $ withForeignPtr fp $ \p -> do
    v <- rawElemValid p (fromIntegral i)
    case v of
      (-1) -> error "Col.get: index out of bounds"
      0    -> pure Nothing
      _    -> Just . (/= 0) <$> rawGetBool p (fromIntegral i)

toList :: ColGet d => Col d -> [Maybe (HostType d)]
toList c = map (get c) [0..len c - 1]

-- ---------------------------------------------------------------------------
-- Access
-- ---------------------------------------------------------------------------

len :: Col d -> Int
len (MkCol fp) = pu $ withForeignPtr fp $ \p -> fromIntegral <$> rawColLen p

nullCount :: Col d -> Int
nullCount (MkCol fp) = pu $ withForeignPtr fp $ \p -> fromIntegral <$> rawColNullCount p

toString :: Col d -> String
toString (MkCol fp) = pu $ withForeignPtr fp $ \p -> do
    cs <- rawColToString p
    s <- peekCString cs
    rawStringFree cs
    pure s

-- ---------------------------------------------------------------------------
-- Compute helpers — pure wrappers via unsafeDupablePerformIO.
-- Arrow kernels are referentially transparent; IO is just FFI ceremony.
-- ---------------------------------------------------------------------------

pu :: IO a -> a
pu = unsafeDupablePerformIO

tri :: (Ptr RawCol -> Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol))
    -> Col a -> Col b -> Col c -> Col d
tri raw (MkCol a) (MkCol b) (MkCol c) = pu $
    withForeignPtr a $ \pa -> withForeignPtr b $ \pb -> withForeignPtr c $ \pc -> do
        r <- throwIfNull $ raw pa pb pc
        MkCol <$> newForeignPtr rawColFreePtr r

bin :: (Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)) -> Col d -> Col e -> Col f
bin raw (MkCol a) (MkCol b) = pu $
    withForeignPtr a $ \pa -> withForeignPtr b $ \pb -> do
        r <- throwIfNull $ raw pa pb
        MkCol <$> newForeignPtr rawColFreePtr r

una :: (Ptr RawCol -> IO (Ptr RawCol)) -> Col d -> Col e
una raw (MkCol a) = pu $ withForeignPtr a $ \pa -> do
    r <- throwIfNull $ raw pa
    MkCol <$> newForeignPtr rawColFreePtr r

aggr :: (Ptr RawCol -> IO (Ptr RawVal)) -> Col d -> Val e
aggr raw (MkCol a) = pu $ withForeignPtr a $ \pa -> do
    r <- throwIfNull $ raw pa
    MkVal <$> newForeignPtr rawValFreePtr r

-- ---------------------------------------------------------------------------
-- Arithmetic — Num instance gives +, -, *, negate, abs, signum, fromInteger
-- ---------------------------------------------------------------------------

instance (IsNumeric d, IsOrd d, ColMk d, Num (HostType d)) => Num (Col d) where
  (+) = bin rawAdd
  (-) = bin rawSub
  (*) = bin rawMul
  negate = una rawNeg
  abs = una rawAbs
  signum x = ifElse (x >. z) one (ifElse (x <. z) neg1 z)
    where n = len x; z = fill' n 0; one = fill' n 1; neg1 = fill' n (-1)
  fromInteger n = mk [Just (fromInteger n)]

fill' :: (ColMk d, Num (HostType d)) => Int -> HostType d -> Col d
fill' n v = mk (Prelude.replicate n (Just v))

infixl 7 /.

(/.) :: IsNumeric d => Col d -> Col d -> Col d
(/.) = bin rawDiv

-- ---------------------------------------------------------------------------
-- Comparison → Col 'Bool
-- ---------------------------------------------------------------------------

infixl 4 ==., /=., <., >., <=., >=.

(==.), (/=.) :: Col d -> Col d -> Col 'Bool
(==.) = bin rawEq
(/=.) = bin rawNeq

(<.), (>.), (<=.), (>=.) :: IsOrd d => Col d -> Col d -> Col 'Bool
(<.) = bin rawLt
(>.) = bin rawGt
(<=.) = bin rawLte
(>=.) = bin rawGte

-- ---------------------------------------------------------------------------
-- Boolean logic
-- ---------------------------------------------------------------------------

infixl 3 &&.
infixl 2 ||.

(&&.), (||.) :: Col 'Bool -> Col 'Bool -> Col 'Bool
(&&.) = bin rawLogAnd
(||.) = bin rawLogOr

-- ---------------------------------------------------------------------------
-- Vector ops
-- ---------------------------------------------------------------------------

filter :: Col d -> Col 'Bool -> Col d
filter = bin rawFilter

take :: Col d -> Col idx -> Col d
take = bin rawTake

sort :: IsOrd d => Col d -> Bool -> Col d
sort (MkCol fp) asc = pu $ withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawSort p (if asc then 1 else 0)
    MkCol <$> newForeignPtr rawColFreePtr r

unique :: Col d -> Col d
unique = una rawUnique

dropNull :: Col d -> Col d
dropNull = una rawDropNull

-- ---------------------------------------------------------------------------
-- Null handling
-- ---------------------------------------------------------------------------

isNulls :: Col d -> Col 'Bool
isNulls = una rawIsNulls

isValids :: Col d -> Col 'Bool
isValids = una rawIsValids

fillNull :: Col d -> Col d -> Col d
fillNull = bin rawFillNull

-- ---------------------------------------------------------------------------
-- Conditional
-- ---------------------------------------------------------------------------

ifElse :: Col 'Bool -> Col d -> Col d -> Col d
ifElse = tri rawIfElse

-- ---------------------------------------------------------------------------
-- Aggregation
-- ---------------------------------------------------------------------------

sum :: IsNumeric d => Col d -> Val d
sum = aggr rawSum

mean :: IsNumeric d => Col d -> Val 'Float64
mean = aggr rawMean

colMin, colMax :: IsOrd d => Col d -> Val d
colMin = aggr rawMin
colMax = aggr rawMax

product :: IsNumeric d => Col d -> Val d
product = aggr rawProduct

-- ---------------------------------------------------------------------------
-- Array primitives
-- ---------------------------------------------------------------------------

iota :: Int64 -> Col 'Int64
iota n = pu $ do r <- throwIfNull $ rawIota n; MkCol <$> newForeignPtr rawColFreePtr r

fillInt64 :: Int64 -> Int64 -> Col 'Int64
fillInt64 n v = pu $ do r <- throwIfNull $ rawFillInt64 n v; MkCol <$> newForeignPtr rawColFreePtr r

where_ :: Col 'Bool -> Col 'Int64
where_ = una rawWhere

concat :: Col d -> Col d -> Col d
concat = bin rawConcat

isIn :: Col d -> Col d -> Col 'Bool
isIn = bin rawIsIn

indexOf :: Col d -> Col d -> Col 'Int32
indexOf = bin rawIndexOf

cast :: Word8 -> Col d -> Col e
cast target (MkCol fp) = pu $ withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawCast p target
    MkCol <$> newForeignPtr rawColFreePtr r

slice :: Col d -> Int64 -> Int64 -> Col d
slice (MkCol fp) off n = pu $ withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawSlice p off n
    MkCol <$> newForeignPtr rawColFreePtr r

scatter :: Col d -> Col 'Int64 -> Col d -> Col d
scatter = tri rawScatter

scatterScalar :: Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
scatterScalar (MkCol fp1) (MkCol fp2) v = pu $
    withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> do
        r <- throwIfNull $ rawScatterScalar p1 p2 v
        MkCol <$> newForeignPtr rawColFreePtr r

data ScanFn = SfAdd | SfMul | SfMax | SfMin

scanTag :: ScanFn -> Word8
scanTag SfAdd = 0; scanTag SfMul = 1; scanTag SfMax = 2; scanTag SfMin = 3

scan :: ScanFn -> Col 'Int64 -> Col 'Int64
scan f (MkCol fp) = pu $ withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawScan (scanTag f) p
    MkCol <$> newForeignPtr rawColFreePtr r

cumulativeSum :: IsNumeric d => Col d -> Col d
cumulativeSum = una rawCumulativeSum

reverseCol :: Col d -> Col d
reverseCol = una rawReverse

sortIndices :: IsOrd d => Col d -> Bool -> Col 'Int64
sortIndices (MkCol fp) asc = pu $ withForeignPtr fp $ \p -> do
    r <- throwIfNull $ rawSortIndices p (if asc then 1 else 0)
    MkCol <$> newForeignPtr rawColFreePtr r

expand :: Col d -> Col 'Int64 -> Col d
expand = bin rawReplicate
