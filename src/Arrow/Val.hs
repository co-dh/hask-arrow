module Arrow.Val
  ( Val(..)
  , ExtractVal(..)
  , isValid
  , toString
  , wrapVal
  ) where

import Arrow.Dtype   (Dtype(..), HostType)
import Arrow.FFI
import Control.Monad      ((<=<), (>=>))
import Data.Kind          (Type)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr        (Ptr)

type Val :: Dtype -> Type
data Val d where
  MkVal :: ForeignPtr RawVal -> Val d

-- | Wrap a raw-pointer producer: throw on null, attach Arrow's free finalizer.
wrapVal :: IO (Ptr RawVal) -> IO (Val d)
wrapVal = throwIfNull >=> fmap MkVal . newForeignPtr rawValFreePtr

isValid :: Val d -> Bool
isValid (MkVal fp) = pu $ withForeignPtr fp $ fmap (/= 0) . rawValIsValid

toString :: Val d -> String
toString (MkVal fp) = pu $ withForeignPtr fp $ peekCStringFree <=< rawValToString

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
