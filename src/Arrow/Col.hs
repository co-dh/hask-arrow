module Arrow.Col
  ( Col(..)
  , ColMk(..), wrapCol
  , ColGet(..), toList
  , len, nullCount, toString
  , (/.)
  , (==.), (/=.), (<.), (>.), (<=.), (>=.)
  , (&&.), (||.), notC
  , filter, take, sort, unique, dropNull
  , isNulls, isValids, fillNull
  , ifElse
  , sum, mean, colMin, colMax, product
  , iota, fillInt64, where_, concat, isIn, indexOf
  , cast, slice, scatter, scatterScalar
  , scan, ScanFn(..), cumulativeSum
  , reverseCol, sortIndices, expand
  ) where

import Prelude hiding (filter, take, sum, product, concat)

import Arrow.Dtype
import Arrow.FFI
import Arrow.Val (Val(..), wrapVal)
import Data.Kind            (Type)

import Control.Exception     (finally)
import Control.Monad         ((<=<), (>=>))
import Data.Int             (Int8, Int64)
import Data.Word            (Word8)
import Foreign.ForeignPtr   (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray, pokeArray)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (Storable)

type Col :: Dtype -> Type
data Col d where
  MkCol :: ForeignPtr RawCol -> Col d

class ColMk (d :: Dtype) where
  mk :: [Maybe (HostType d)] -> Col d

-- | Wrap a raw-pointer producer: throw on null, attach Arrow's free finalizer.
wrapCol :: IO (Ptr RawCol) -> IO (Col d)
wrapCol = throwIfNull >=> fmap MkCol . newForeignPtr rawColFreePtr

-- | Allocate two parallel buffers (data + validity), poke them from a list
-- via the supplied splitter, hand them to a raw constructor, and free.
mkBuf :: Storable a
      => (Ptr a -> Ptr Word8 -> Int64 -> IO (Ptr RawCol))
      -> (Maybe b -> (a, Word8))
      -> [Maybe b] -> Col d
mkBuf rawMk split xs = pu $ do
    let n          = Prelude.length xs
        (ds, vs)   = unzip (map split xs)
        sz         = max 1 n
    dPtr <- mallocArray sz
    vPtr <- mallocArray sz
    (do pokeArray dPtr ds
        pokeArray vPtr vs
        wrapCol (rawMk dPtr vPtr (fromIntegral n)))
        `finally` (free dPtr >> free vPtr)

mkNum :: (Storable a, Num a)
      => (Ptr a -> Ptr Word8 -> Int64 -> IO (Ptr RawCol))
      -> [Maybe a] -> Col d
mkNum rawMk = mkBuf rawMk (maybe (0, 0) (\v -> (v, 1)))

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
  mk = mkBuf rawMkBool (maybe (0, 0) (\b -> (if b then 1 else 0, 1)))

class ColGet (d :: Dtype) where
  get :: Col d -> Int -> Maybe (HostType d)

getElem :: (Ptr RawCol -> Int64 -> IO a) -> Col d -> Int -> Maybe a
getElem rawGet (MkCol fp) i = pu $ withForeignPtr fp $ \p ->
    rawElemValid p (fromIntegral i) >>= maybeValid (rawGet p (fromIntegral i))

-- | Dispatch on the C-side validity flag (-1 = OOB, 0 = null, _ = valid).
maybeValid :: IO a -> Int8 -> IO (Maybe a)
maybeValid _   (-1) = error "Col.get: index out of bounds"
maybeValid _   0    = pure Nothing
maybeValid get _    = Just <$> get

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
  get (MkCol fp) i = pu $ withForeignPtr fp $ \p ->
      rawElemValid p (fromIntegral i)
        >>= maybeValid ((/= 0) <$> rawGetBool p (fromIntegral i))

toList :: ColGet d => Col d -> [Maybe (HostType d)]
toList c = map (get c) [0..len c - 1]

len :: Col d -> Int
len (MkCol fp) = pu $ withForeignPtr fp (fmap fromIntegral . rawColLen)

nullCount :: Col d -> Int
nullCount (MkCol fp) = pu $ withForeignPtr fp (fmap fromIntegral . rawColNullCount)

toString :: Col d -> String
toString (MkCol fp) = pu $ withForeignPtr fp $ peekCStringFree <=< rawColToString

tri :: (Ptr RawCol -> Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol))
    -> Col a -> Col b -> Col c -> Col d
tri raw (MkCol a) (MkCol b) (MkCol c) = pu $
    withForeignPtr a $ \pa -> withForeignPtr b $ \pb -> withForeignPtr c $ \pc ->
        wrapCol (raw pa pb pc)

bin :: (Ptr RawCol -> Ptr RawCol -> IO (Ptr RawCol)) -> Col d -> Col e -> Col f
bin raw (MkCol a) (MkCol b) = pu $
    withForeignPtr a $ \pa -> withForeignPtr b $ \pb -> wrapCol (raw pa pb)

una :: (Ptr RawCol -> IO (Ptr RawCol)) -> Col d -> Col e
una raw (MkCol a) = pu $ withForeignPtr a (wrapCol . raw)

aggr :: (Ptr RawCol -> IO (Ptr RawVal)) -> Col d -> Val e
aggr raw (MkCol a) = pu $ withForeignPtr a (wrapVal . raw)

instance (IsNumeric d, ColMk d, Num (HostType d)) => Num (Col d) where
  (+) = bin rawAdd
  (-) = bin rawSub
  (*) = bin rawMul
  negate = una rawNeg
  abs = una rawAbs
  signum = una rawSign
  fromInteger n = mk [Just (fromInteger n)]

infixl 7 /.

(/.) :: IsNumeric d => Col d -> Col d -> Col d
(/.) = bin rawDiv

infixl 4 ==., /=., <., >., <=., >=.

(==.), (/=.) :: Col d -> Col d -> Col 'Bool
(==.) = bin rawEq
(/=.) = bin rawNeq

(<.), (>.), (<=.), (>=.) :: IsOrd d => Col d -> Col d -> Col 'Bool
(<.) = bin rawLt
(>.) = bin rawGt
(<=.) = bin rawLte
(>=.) = bin rawGte

infixl 3 &&.
infixl 2 ||.

(&&.), (||.) :: Col 'Bool -> Col 'Bool -> Col 'Bool
(&&.) = bin rawLogAnd
(||.) = bin rawLogOr

notC :: Col 'Bool -> Col 'Bool
notC = una rawLogNot

filter :: Col d -> Col 'Bool -> Col d
filter = bin rawFilter

take :: Col d -> Col idx -> Col d
take = bin rawTake

sort :: IsOrd d => Col d -> Bool -> Col d
sort (MkCol fp) asc = pu $ withForeignPtr fp $ \p -> wrapCol (rawSort p (boolByte asc))

boolByte :: Bool -> Word8
boolByte b = if b then 1 else 0

unique :: Col d -> Col d
unique = una rawUnique

dropNull :: Col d -> Col d
dropNull = una rawDropNull

isNulls :: Col d -> Col 'Bool
isNulls = una rawIsNulls

isValids :: Col d -> Col 'Bool
isValids = una rawIsValids

fillNull :: Col d -> Col d -> Col d
fillNull = bin rawFillNull

ifElse :: Col 'Bool -> Col d -> Col d -> Col d
ifElse = tri rawIfElse

sum :: IsNumeric d => Col d -> Val d
sum = aggr rawSum

mean :: IsNumeric d => Col d -> Val 'Float64
mean = aggr rawMean

colMin, colMax :: IsOrd d => Col d -> Val d
colMin = aggr rawMin
colMax = aggr rawMax

product :: IsNumeric d => Col d -> Val d
product = aggr rawProduct

iota :: Int64 -> Col 'Int64
iota n = pu $ wrapCol (rawIota n)

fillInt64 :: Int64 -> Int64 -> Col 'Int64
fillInt64 n v = pu $ wrapCol (rawFillInt64 n v)

where_ :: Col 'Bool -> Col 'Int64
where_ = una rawWhere

concat :: Col d -> Col d -> Col d
concat = bin rawConcat

isIn :: Col d -> Col d -> Col 'Bool
isIn = bin rawIsIn

indexOf :: Col d -> Col d -> Col 'Int32
indexOf = bin rawIndexOf

cast :: Word8 -> Col d -> Col e
cast target (MkCol fp) = pu $ withForeignPtr fp $ \p -> wrapCol (rawCast p target)

slice :: Col d -> Int64 -> Int64 -> Col d
slice (MkCol fp) off n = pu $ withForeignPtr fp $ \p -> wrapCol (rawSlice p off n)

scatter :: Col d -> Col 'Int64 -> Col d -> Col d
scatter = tri rawScatter

scatterScalar :: Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
scatterScalar (MkCol fp1) (MkCol fp2) v = pu $
    withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 ->
        wrapCol (rawScatterScalar p1 p2 v)

data ScanFn = SfAdd | SfMul | SfMax | SfMin

scanTag :: ScanFn -> Word8
scanTag SfAdd = 0
scanTag SfMul = 1
scanTag SfMax = 2
scanTag SfMin = 3

scan :: ScanFn -> Col 'Int64 -> Col 'Int64
scan f (MkCol fp) = pu $ withForeignPtr fp $ wrapCol . rawScan (scanTag f)

cumulativeSum :: IsNumeric d => Col d -> Col d
cumulativeSum = una rawCumulativeSum

reverseCol :: Col d -> Col d
reverseCol = una rawReverse

sortIndices :: IsOrd d => Col d -> Bool -> Col 'Int64
sortIndices (MkCol fp) asc = pu $ withForeignPtr fp $ \p -> wrapCol (rawSortIndices p (boolByte asc))

expand :: Col d -> Col 'Int64 -> Col d
expand = bin rawReplicate
