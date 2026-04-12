module Arrow.Val
  ( Val(..)
  , ExtractVal(..)
  , isValid
  , toString
  ) where

import Arrow.Dtype   (Dtype(..), HostType)
import Data.Kind     (Type)
import Arrow.FFI
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr        (Ptr)
import Foreign.C.String   (peekCString)
import System.IO.Unsafe   (unsafeDupablePerformIO)

type Val :: Dtype -> Type
data Val d where
  MkVal :: ForeignPtr RawVal -> Val d

pu :: IO a -> a
pu = unsafeDupablePerformIO

isValid :: Val d -> Bool
isValid (MkVal fp) = pu $ withForeignPtr fp $ \p -> (/= 0) <$> rawValIsValid p

toString :: Val d -> String
toString (MkVal fp) = pu $ withForeignPtr fp $ \p -> do
    cs <- rawValToString p
    s <- peekCString cs
    rawStringFree cs
    pure s

class ExtractVal (d :: Dtype) where
  extract :: Val d -> Maybe (HostType d)

extractWith :: (Ptr RawVal -> IO a) -> Val d -> Maybe a
extractWith rawGet (MkVal fp) = pu $ withForeignPtr fp $ \p -> do
    v <- rawValIsValid p
    if v == 0 then pure Nothing else Just <$> rawGet p

instance ExtractVal 'Int8    where extract = extractWith rawValGetInt8
instance ExtractVal 'Int16   where extract = extractWith rawValGetInt16
instance ExtractVal 'Int32   where extract = extractWith rawValGetInt32
instance ExtractVal 'Int64   where extract = extractWith rawValGetInt64
instance ExtractVal 'UInt8   where extract = extractWith rawValGetUInt8
instance ExtractVal 'UInt16  where extract = extractWith rawValGetUInt16
instance ExtractVal 'UInt32  where extract = extractWith rawValGetUInt32
instance ExtractVal 'UInt64  where extract = extractWith rawValGetUInt64
instance ExtractVal 'Float32 where extract = extractWith rawValGetFloat32
instance ExtractVal 'Float64 where extract = extractWith rawValGetFloat64
