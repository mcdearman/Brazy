-- |
-- The types of C, each as the type of a fixed width it is on a 64-bit
-- system other than Windows.
--
-- In GHC each is a newtype of that type. Here each is a name for it: a
-- foreign function is given a newtype as what it holds, which the compiler
-- does not see through yet. So a value of one of them is a value of the
-- type it names, and there are no constructors to match. @long@ is taken
-- to be 64 bits, as it is everywhere but on Windows.
module Foreign.C.Types
  ( CChar,
    CSChar,
    CUChar,
    CShort,
    CUShort,
    CInt,
    CUInt,
    CLong,
    CULong,
    CLLong,
    CULLong,
    CPtrdiff,
    CSize,
    CWchar,
    CSigAtomic,
    CBool,
    CIntPtr,
    CUIntPtr,
    CIntMax,
    CUIntMax,
    CClock,
    CTime,
    CUSeconds,
    CSUSeconds,
    CFloat,
    CDouble,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

type CChar = Int8

type CSChar = Int8

type CUChar = Word8

type CShort = Int16

type CUShort = Word16

type CInt = Int32

type CUInt = Word32

type CLong = Int64

type CULong = Word64

type CLLong = Int64

type CULLong = Word64

type CPtrdiff = Int64

type CSize = Word64

type CWchar = Int32

type CSigAtomic = Int32

type CBool = Word8

type CIntPtr = Int64

type CUIntPtr = Word64

type CIntMax = Int64

type CUIntMax = Word64

type CClock = Int64

type CTime = Int64

type CUSeconds = Word32

type CSUSeconds = Int64

type CFloat = Float

type CDouble = Double
