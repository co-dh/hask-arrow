module Arrow.Dtype
  ( Dtype(..)
  , HostType
  , IsNumeric
  , IsOrd
  , IsIntegral
  , IsFloat
  ) where

import Data.Int  (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Kind (Constraint, Type)

data Dtype
  = Bool
  | Int8 | Int16 | Int32 | Int64
  | UInt8 | UInt16 | UInt32 | UInt64
  | Float32 | Float64
  | Utf8
  | Date32 | Date64
  deriving (Show, Eq)

type family HostType (d :: Dtype) :: Type where
  HostType 'Bool    = Prelude.Bool
  HostType 'Int8    = Int8
  HostType 'Int16   = Int16
  HostType 'Int32   = Int32
  HostType 'Int64   = Int64
  HostType 'UInt8   = Word8
  HostType 'UInt16  = Word16
  HostType 'UInt32  = Word32
  HostType 'UInt64  = Word64
  HostType 'Float32 = Float
  HostType 'Float64 = Double
  HostType 'Utf8    = String
  HostType 'Date32  = Int32
  HostType 'Date64  = Int64

-- Numeric: arithmetic allowed
type IsNumeric :: Dtype -> Constraint
class IsNumeric d
instance IsNumeric 'Int8
instance IsNumeric 'Int16
instance IsNumeric 'Int32
instance IsNumeric 'Int64
instance IsNumeric 'UInt8
instance IsNumeric 'UInt16
instance IsNumeric 'UInt32
instance IsNumeric 'UInt64
instance IsNumeric 'Float32
instance IsNumeric 'Float64

-- Orderable: comparison/sort allowed
type IsOrd :: Dtype -> Constraint
class IsOrd d
instance IsOrd 'Bool
instance IsOrd 'Int8
instance IsOrd 'Int16
instance IsOrd 'Int32
instance IsOrd 'Int64
instance IsOrd 'UInt8
instance IsOrd 'UInt16
instance IsOrd 'UInt32
instance IsOrd 'UInt64
instance IsOrd 'Float32
instance IsOrd 'Float64
instance IsOrd 'Utf8
instance IsOrd 'Date32
instance IsOrd 'Date64

-- Integral: bitwise ops allowed
type IsIntegral :: Dtype -> Constraint
class IsIntegral d
instance IsIntegral 'Int8
instance IsIntegral 'Int16
instance IsIntegral 'Int32
instance IsIntegral 'Int64
instance IsIntegral 'UInt8
instance IsIntegral 'UInt16
instance IsIntegral 'UInt32
instance IsIntegral 'UInt64

-- Floating-point
type IsFloat :: Dtype -> Constraint
class IsFloat d
instance IsFloat 'Float32
instance IsFloat 'Float64
