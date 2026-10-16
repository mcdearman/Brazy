-- |
-- The types of C, each a newtype of the type it is on a 64-bit system other
-- than Windows, as GHC has them. A foreign function is given each as what
-- it holds. Each is shown and read, and counts, as what it holds does.
-- @long@ is taken to be 64 bits, as it is everywhere but on Windows.
module Foreign.C.Types
  ( CChar (..),
    CSChar (..),
    CUChar (..),
    CShort (..),
    CUShort (..),
    CInt (..),
    CUInt (..),
    CLong (..),
    CULong (..),
    CLLong (..),
    CULLong (..),
    CPtrdiff (..),
    CSize (..),
    CWchar (..),
    CSigAtomic (..),
    CBool (..),
    CIntPtr (..),
    CUIntPtr (..),
    CIntMax (..),
    CUIntMax (..),
    CClock (..),
    CTime (..),
    CUSeconds (..),
    CSUSeconds (..),
    CFloat (..),
    CDouble (..),
  )
where

import Data.Bits (Bits (..), FiniteBits (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))

newtype CChar = CChar Int8

instance Eq CChar where
  CChar a == CChar b = a == b

instance Ord CChar where
  compare (CChar a) (CChar b) = compare a b

instance Show CChar where
  showsPrec d (CChar a) = showsPrec d a

instance Read CChar where
  readsPrec d text = [(CChar a, rest) | (a, rest) <- readsPrec d text]

instance Num CChar where
  CChar a + CChar b = CChar (a + b)
  CChar a - CChar b = CChar (a - b)
  CChar a * CChar b = CChar (a * b)
  negate (CChar a) = CChar (negate a)
  abs (CChar a) = CChar (abs a)
  signum (CChar a) = CChar (signum a)
  fromInteger n = CChar (fromInteger n)

instance Enum CChar where
  succ (CChar a) = CChar (succ a)
  pred (CChar a) = CChar (pred a)
  toEnum n = CChar (toEnum n)
  fromEnum (CChar a) = fromEnum a
  enumFrom (CChar a) = map CChar (enumFrom a)
  enumFromThen (CChar a) (CChar b) = map CChar (enumFromThen a b)
  enumFromTo (CChar a) (CChar b) = map CChar (enumFromTo a b)
  enumFromThenTo (CChar a) (CChar b) (CChar c) = map CChar (enumFromThenTo a b c)

instance Real CChar where
  toRational (CChar a) = toRational a

instance Storable CChar where
  sizeOf _ = sizeOf (undefined :: Int8)
  alignment _ = alignment (undefined :: Int8)
  peek address = CChar <$> peek (castPtr address)
  poke address (CChar a) = poke (castPtr address) a

instance Integral CChar where
  quot (CChar a) (CChar b) = CChar (quot a b)
  rem (CChar a) (CChar b) = CChar (rem a b)
  div (CChar a) (CChar b) = CChar (div a b)
  mod (CChar a) (CChar b) = CChar (mod a b)
  quotRem (CChar a) (CChar b) = case quotRem a b of
    (q, r) -> (CChar q, CChar r)
  divMod (CChar a) (CChar b) = case divMod a b of
    (q, r) -> (CChar q, CChar r)
  toInteger (CChar a) = toInteger a

instance Bounded CChar where
  minBound = CChar minBound
  maxBound = CChar maxBound

instance Bits CChar where
  CChar a .&. CChar b = CChar (a .&. b)
  CChar a .|. CChar b = CChar (a .|. b)
  xor (CChar a) (CChar b) = CChar (xor a b)
  complement (CChar a) = CChar (complement a)
  shift (CChar a) i = CChar (shift a i)
  rotate (CChar a) i = CChar (rotate a i)
  zeroBits = CChar zeroBits
  bit i = CChar (bit i)
  setBit (CChar a) i = CChar (setBit a i)
  clearBit (CChar a) i = CChar (clearBit a i)
  testBit (CChar a) i = testBit a i
  bitSizeMaybe (CChar a) = bitSizeMaybe a
  bitSize (CChar a) = bitSize a
  isSigned (CChar a) = isSigned a
  shiftL (CChar a) i = CChar (shiftL a i)
  shiftR (CChar a) i = CChar (shiftR a i)
  rotateL (CChar a) i = CChar (rotateL a i)
  rotateR (CChar a) i = CChar (rotateR a i)
  popCount (CChar a) = popCount a

instance FiniteBits CChar where
  finiteBitSize (CChar a) = finiteBitSize a

newtype CSChar = CSChar Int8

instance Eq CSChar where
  CSChar a == CSChar b = a == b

instance Ord CSChar where
  compare (CSChar a) (CSChar b) = compare a b

instance Show CSChar where
  showsPrec d (CSChar a) = showsPrec d a

instance Read CSChar where
  readsPrec d text = [(CSChar a, rest) | (a, rest) <- readsPrec d text]

instance Num CSChar where
  CSChar a + CSChar b = CSChar (a + b)
  CSChar a - CSChar b = CSChar (a - b)
  CSChar a * CSChar b = CSChar (a * b)
  negate (CSChar a) = CSChar (negate a)
  abs (CSChar a) = CSChar (abs a)
  signum (CSChar a) = CSChar (signum a)
  fromInteger n = CSChar (fromInteger n)

instance Enum CSChar where
  succ (CSChar a) = CSChar (succ a)
  pred (CSChar a) = CSChar (pred a)
  toEnum n = CSChar (toEnum n)
  fromEnum (CSChar a) = fromEnum a
  enumFrom (CSChar a) = map CSChar (enumFrom a)
  enumFromThen (CSChar a) (CSChar b) = map CSChar (enumFromThen a b)
  enumFromTo (CSChar a) (CSChar b) = map CSChar (enumFromTo a b)
  enumFromThenTo (CSChar a) (CSChar b) (CSChar c) = map CSChar (enumFromThenTo a b c)

instance Real CSChar where
  toRational (CSChar a) = toRational a

instance Storable CSChar where
  sizeOf _ = sizeOf (undefined :: Int8)
  alignment _ = alignment (undefined :: Int8)
  peek address = CSChar <$> peek (castPtr address)
  poke address (CSChar a) = poke (castPtr address) a

instance Integral CSChar where
  quot (CSChar a) (CSChar b) = CSChar (quot a b)
  rem (CSChar a) (CSChar b) = CSChar (rem a b)
  div (CSChar a) (CSChar b) = CSChar (div a b)
  mod (CSChar a) (CSChar b) = CSChar (mod a b)
  quotRem (CSChar a) (CSChar b) = case quotRem a b of
    (q, r) -> (CSChar q, CSChar r)
  divMod (CSChar a) (CSChar b) = case divMod a b of
    (q, r) -> (CSChar q, CSChar r)
  toInteger (CSChar a) = toInteger a

instance Bounded CSChar where
  minBound = CSChar minBound
  maxBound = CSChar maxBound

instance Bits CSChar where
  CSChar a .&. CSChar b = CSChar (a .&. b)
  CSChar a .|. CSChar b = CSChar (a .|. b)
  xor (CSChar a) (CSChar b) = CSChar (xor a b)
  complement (CSChar a) = CSChar (complement a)
  shift (CSChar a) i = CSChar (shift a i)
  rotate (CSChar a) i = CSChar (rotate a i)
  zeroBits = CSChar zeroBits
  bit i = CSChar (bit i)
  setBit (CSChar a) i = CSChar (setBit a i)
  clearBit (CSChar a) i = CSChar (clearBit a i)
  testBit (CSChar a) i = testBit a i
  bitSizeMaybe (CSChar a) = bitSizeMaybe a
  bitSize (CSChar a) = bitSize a
  isSigned (CSChar a) = isSigned a
  shiftL (CSChar a) i = CSChar (shiftL a i)
  shiftR (CSChar a) i = CSChar (shiftR a i)
  rotateL (CSChar a) i = CSChar (rotateL a i)
  rotateR (CSChar a) i = CSChar (rotateR a i)
  popCount (CSChar a) = popCount a

instance FiniteBits CSChar where
  finiteBitSize (CSChar a) = finiteBitSize a

newtype CUChar = CUChar Word8

instance Eq CUChar where
  CUChar a == CUChar b = a == b

instance Ord CUChar where
  compare (CUChar a) (CUChar b) = compare a b

instance Show CUChar where
  showsPrec d (CUChar a) = showsPrec d a

instance Read CUChar where
  readsPrec d text = [(CUChar a, rest) | (a, rest) <- readsPrec d text]

instance Num CUChar where
  CUChar a + CUChar b = CUChar (a + b)
  CUChar a - CUChar b = CUChar (a - b)
  CUChar a * CUChar b = CUChar (a * b)
  negate (CUChar a) = CUChar (negate a)
  abs (CUChar a) = CUChar (abs a)
  signum (CUChar a) = CUChar (signum a)
  fromInteger n = CUChar (fromInteger n)

instance Enum CUChar where
  succ (CUChar a) = CUChar (succ a)
  pred (CUChar a) = CUChar (pred a)
  toEnum n = CUChar (toEnum n)
  fromEnum (CUChar a) = fromEnum a
  enumFrom (CUChar a) = map CUChar (enumFrom a)
  enumFromThen (CUChar a) (CUChar b) = map CUChar (enumFromThen a b)
  enumFromTo (CUChar a) (CUChar b) = map CUChar (enumFromTo a b)
  enumFromThenTo (CUChar a) (CUChar b) (CUChar c) = map CUChar (enumFromThenTo a b c)

instance Real CUChar where
  toRational (CUChar a) = toRational a

instance Storable CUChar where
  sizeOf _ = sizeOf (undefined :: Word8)
  alignment _ = alignment (undefined :: Word8)
  peek address = CUChar <$> peek (castPtr address)
  poke address (CUChar a) = poke (castPtr address) a

instance Integral CUChar where
  quot (CUChar a) (CUChar b) = CUChar (quot a b)
  rem (CUChar a) (CUChar b) = CUChar (rem a b)
  div (CUChar a) (CUChar b) = CUChar (div a b)
  mod (CUChar a) (CUChar b) = CUChar (mod a b)
  quotRem (CUChar a) (CUChar b) = case quotRem a b of
    (q, r) -> (CUChar q, CUChar r)
  divMod (CUChar a) (CUChar b) = case divMod a b of
    (q, r) -> (CUChar q, CUChar r)
  toInteger (CUChar a) = toInteger a

instance Bounded CUChar where
  minBound = CUChar minBound
  maxBound = CUChar maxBound

instance Bits CUChar where
  CUChar a .&. CUChar b = CUChar (a .&. b)
  CUChar a .|. CUChar b = CUChar (a .|. b)
  xor (CUChar a) (CUChar b) = CUChar (xor a b)
  complement (CUChar a) = CUChar (complement a)
  shift (CUChar a) i = CUChar (shift a i)
  rotate (CUChar a) i = CUChar (rotate a i)
  zeroBits = CUChar zeroBits
  bit i = CUChar (bit i)
  setBit (CUChar a) i = CUChar (setBit a i)
  clearBit (CUChar a) i = CUChar (clearBit a i)
  testBit (CUChar a) i = testBit a i
  bitSizeMaybe (CUChar a) = bitSizeMaybe a
  bitSize (CUChar a) = bitSize a
  isSigned (CUChar a) = isSigned a
  shiftL (CUChar a) i = CUChar (shiftL a i)
  shiftR (CUChar a) i = CUChar (shiftR a i)
  rotateL (CUChar a) i = CUChar (rotateL a i)
  rotateR (CUChar a) i = CUChar (rotateR a i)
  popCount (CUChar a) = popCount a

instance FiniteBits CUChar where
  finiteBitSize (CUChar a) = finiteBitSize a

newtype CShort = CShort Int16

instance Eq CShort where
  CShort a == CShort b = a == b

instance Ord CShort where
  compare (CShort a) (CShort b) = compare a b

instance Show CShort where
  showsPrec d (CShort a) = showsPrec d a

instance Read CShort where
  readsPrec d text = [(CShort a, rest) | (a, rest) <- readsPrec d text]

instance Num CShort where
  CShort a + CShort b = CShort (a + b)
  CShort a - CShort b = CShort (a - b)
  CShort a * CShort b = CShort (a * b)
  negate (CShort a) = CShort (negate a)
  abs (CShort a) = CShort (abs a)
  signum (CShort a) = CShort (signum a)
  fromInteger n = CShort (fromInteger n)

instance Enum CShort where
  succ (CShort a) = CShort (succ a)
  pred (CShort a) = CShort (pred a)
  toEnum n = CShort (toEnum n)
  fromEnum (CShort a) = fromEnum a
  enumFrom (CShort a) = map CShort (enumFrom a)
  enumFromThen (CShort a) (CShort b) = map CShort (enumFromThen a b)
  enumFromTo (CShort a) (CShort b) = map CShort (enumFromTo a b)
  enumFromThenTo (CShort a) (CShort b) (CShort c) = map CShort (enumFromThenTo a b c)

instance Real CShort where
  toRational (CShort a) = toRational a

instance Storable CShort where
  sizeOf _ = sizeOf (undefined :: Int16)
  alignment _ = alignment (undefined :: Int16)
  peek address = CShort <$> peek (castPtr address)
  poke address (CShort a) = poke (castPtr address) a

instance Integral CShort where
  quot (CShort a) (CShort b) = CShort (quot a b)
  rem (CShort a) (CShort b) = CShort (rem a b)
  div (CShort a) (CShort b) = CShort (div a b)
  mod (CShort a) (CShort b) = CShort (mod a b)
  quotRem (CShort a) (CShort b) = case quotRem a b of
    (q, r) -> (CShort q, CShort r)
  divMod (CShort a) (CShort b) = case divMod a b of
    (q, r) -> (CShort q, CShort r)
  toInteger (CShort a) = toInteger a

instance Bounded CShort where
  minBound = CShort minBound
  maxBound = CShort maxBound

instance Bits CShort where
  CShort a .&. CShort b = CShort (a .&. b)
  CShort a .|. CShort b = CShort (a .|. b)
  xor (CShort a) (CShort b) = CShort (xor a b)
  complement (CShort a) = CShort (complement a)
  shift (CShort a) i = CShort (shift a i)
  rotate (CShort a) i = CShort (rotate a i)
  zeroBits = CShort zeroBits
  bit i = CShort (bit i)
  setBit (CShort a) i = CShort (setBit a i)
  clearBit (CShort a) i = CShort (clearBit a i)
  testBit (CShort a) i = testBit a i
  bitSizeMaybe (CShort a) = bitSizeMaybe a
  bitSize (CShort a) = bitSize a
  isSigned (CShort a) = isSigned a
  shiftL (CShort a) i = CShort (shiftL a i)
  shiftR (CShort a) i = CShort (shiftR a i)
  rotateL (CShort a) i = CShort (rotateL a i)
  rotateR (CShort a) i = CShort (rotateR a i)
  popCount (CShort a) = popCount a

instance FiniteBits CShort where
  finiteBitSize (CShort a) = finiteBitSize a

newtype CUShort = CUShort Word16

instance Eq CUShort where
  CUShort a == CUShort b = a == b

instance Ord CUShort where
  compare (CUShort a) (CUShort b) = compare a b

instance Show CUShort where
  showsPrec d (CUShort a) = showsPrec d a

instance Read CUShort where
  readsPrec d text = [(CUShort a, rest) | (a, rest) <- readsPrec d text]

instance Num CUShort where
  CUShort a + CUShort b = CUShort (a + b)
  CUShort a - CUShort b = CUShort (a - b)
  CUShort a * CUShort b = CUShort (a * b)
  negate (CUShort a) = CUShort (negate a)
  abs (CUShort a) = CUShort (abs a)
  signum (CUShort a) = CUShort (signum a)
  fromInteger n = CUShort (fromInteger n)

instance Enum CUShort where
  succ (CUShort a) = CUShort (succ a)
  pred (CUShort a) = CUShort (pred a)
  toEnum n = CUShort (toEnum n)
  fromEnum (CUShort a) = fromEnum a
  enumFrom (CUShort a) = map CUShort (enumFrom a)
  enumFromThen (CUShort a) (CUShort b) = map CUShort (enumFromThen a b)
  enumFromTo (CUShort a) (CUShort b) = map CUShort (enumFromTo a b)
  enumFromThenTo (CUShort a) (CUShort b) (CUShort c) = map CUShort (enumFromThenTo a b c)

instance Real CUShort where
  toRational (CUShort a) = toRational a

instance Storable CUShort where
  sizeOf _ = sizeOf (undefined :: Word16)
  alignment _ = alignment (undefined :: Word16)
  peek address = CUShort <$> peek (castPtr address)
  poke address (CUShort a) = poke (castPtr address) a

instance Integral CUShort where
  quot (CUShort a) (CUShort b) = CUShort (quot a b)
  rem (CUShort a) (CUShort b) = CUShort (rem a b)
  div (CUShort a) (CUShort b) = CUShort (div a b)
  mod (CUShort a) (CUShort b) = CUShort (mod a b)
  quotRem (CUShort a) (CUShort b) = case quotRem a b of
    (q, r) -> (CUShort q, CUShort r)
  divMod (CUShort a) (CUShort b) = case divMod a b of
    (q, r) -> (CUShort q, CUShort r)
  toInteger (CUShort a) = toInteger a

instance Bounded CUShort where
  minBound = CUShort minBound
  maxBound = CUShort maxBound

instance Bits CUShort where
  CUShort a .&. CUShort b = CUShort (a .&. b)
  CUShort a .|. CUShort b = CUShort (a .|. b)
  xor (CUShort a) (CUShort b) = CUShort (xor a b)
  complement (CUShort a) = CUShort (complement a)
  shift (CUShort a) i = CUShort (shift a i)
  rotate (CUShort a) i = CUShort (rotate a i)
  zeroBits = CUShort zeroBits
  bit i = CUShort (bit i)
  setBit (CUShort a) i = CUShort (setBit a i)
  clearBit (CUShort a) i = CUShort (clearBit a i)
  testBit (CUShort a) i = testBit a i
  bitSizeMaybe (CUShort a) = bitSizeMaybe a
  bitSize (CUShort a) = bitSize a
  isSigned (CUShort a) = isSigned a
  shiftL (CUShort a) i = CUShort (shiftL a i)
  shiftR (CUShort a) i = CUShort (shiftR a i)
  rotateL (CUShort a) i = CUShort (rotateL a i)
  rotateR (CUShort a) i = CUShort (rotateR a i)
  popCount (CUShort a) = popCount a

instance FiniteBits CUShort where
  finiteBitSize (CUShort a) = finiteBitSize a

newtype CInt = CInt Int32

instance Eq CInt where
  CInt a == CInt b = a == b

instance Ord CInt where
  compare (CInt a) (CInt b) = compare a b

instance Show CInt where
  showsPrec d (CInt a) = showsPrec d a

instance Read CInt where
  readsPrec d text = [(CInt a, rest) | (a, rest) <- readsPrec d text]

instance Num CInt where
  CInt a + CInt b = CInt (a + b)
  CInt a - CInt b = CInt (a - b)
  CInt a * CInt b = CInt (a * b)
  negate (CInt a) = CInt (negate a)
  abs (CInt a) = CInt (abs a)
  signum (CInt a) = CInt (signum a)
  fromInteger n = CInt (fromInteger n)

instance Enum CInt where
  succ (CInt a) = CInt (succ a)
  pred (CInt a) = CInt (pred a)
  toEnum n = CInt (toEnum n)
  fromEnum (CInt a) = fromEnum a
  enumFrom (CInt a) = map CInt (enumFrom a)
  enumFromThen (CInt a) (CInt b) = map CInt (enumFromThen a b)
  enumFromTo (CInt a) (CInt b) = map CInt (enumFromTo a b)
  enumFromThenTo (CInt a) (CInt b) (CInt c) = map CInt (enumFromThenTo a b c)

instance Real CInt where
  toRational (CInt a) = toRational a

instance Storable CInt where
  sizeOf _ = sizeOf (undefined :: Int32)
  alignment _ = alignment (undefined :: Int32)
  peek address = CInt <$> peek (castPtr address)
  poke address (CInt a) = poke (castPtr address) a

instance Integral CInt where
  quot (CInt a) (CInt b) = CInt (quot a b)
  rem (CInt a) (CInt b) = CInt (rem a b)
  div (CInt a) (CInt b) = CInt (div a b)
  mod (CInt a) (CInt b) = CInt (mod a b)
  quotRem (CInt a) (CInt b) = case quotRem a b of
    (q, r) -> (CInt q, CInt r)
  divMod (CInt a) (CInt b) = case divMod a b of
    (q, r) -> (CInt q, CInt r)
  toInteger (CInt a) = toInteger a

instance Bounded CInt where
  minBound = CInt minBound
  maxBound = CInt maxBound

instance Bits CInt where
  CInt a .&. CInt b = CInt (a .&. b)
  CInt a .|. CInt b = CInt (a .|. b)
  xor (CInt a) (CInt b) = CInt (xor a b)
  complement (CInt a) = CInt (complement a)
  shift (CInt a) i = CInt (shift a i)
  rotate (CInt a) i = CInt (rotate a i)
  zeroBits = CInt zeroBits
  bit i = CInt (bit i)
  setBit (CInt a) i = CInt (setBit a i)
  clearBit (CInt a) i = CInt (clearBit a i)
  testBit (CInt a) i = testBit a i
  bitSizeMaybe (CInt a) = bitSizeMaybe a
  bitSize (CInt a) = bitSize a
  isSigned (CInt a) = isSigned a
  shiftL (CInt a) i = CInt (shiftL a i)
  shiftR (CInt a) i = CInt (shiftR a i)
  rotateL (CInt a) i = CInt (rotateL a i)
  rotateR (CInt a) i = CInt (rotateR a i)
  popCount (CInt a) = popCount a

instance FiniteBits CInt where
  finiteBitSize (CInt a) = finiteBitSize a

newtype CUInt = CUInt Word32

instance Eq CUInt where
  CUInt a == CUInt b = a == b

instance Ord CUInt where
  compare (CUInt a) (CUInt b) = compare a b

instance Show CUInt where
  showsPrec d (CUInt a) = showsPrec d a

instance Read CUInt where
  readsPrec d text = [(CUInt a, rest) | (a, rest) <- readsPrec d text]

instance Num CUInt where
  CUInt a + CUInt b = CUInt (a + b)
  CUInt a - CUInt b = CUInt (a - b)
  CUInt a * CUInt b = CUInt (a * b)
  negate (CUInt a) = CUInt (negate a)
  abs (CUInt a) = CUInt (abs a)
  signum (CUInt a) = CUInt (signum a)
  fromInteger n = CUInt (fromInteger n)

instance Enum CUInt where
  succ (CUInt a) = CUInt (succ a)
  pred (CUInt a) = CUInt (pred a)
  toEnum n = CUInt (toEnum n)
  fromEnum (CUInt a) = fromEnum a
  enumFrom (CUInt a) = map CUInt (enumFrom a)
  enumFromThen (CUInt a) (CUInt b) = map CUInt (enumFromThen a b)
  enumFromTo (CUInt a) (CUInt b) = map CUInt (enumFromTo a b)
  enumFromThenTo (CUInt a) (CUInt b) (CUInt c) = map CUInt (enumFromThenTo a b c)

instance Real CUInt where
  toRational (CUInt a) = toRational a

instance Storable CUInt where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek address = CUInt <$> peek (castPtr address)
  poke address (CUInt a) = poke (castPtr address) a

instance Integral CUInt where
  quot (CUInt a) (CUInt b) = CUInt (quot a b)
  rem (CUInt a) (CUInt b) = CUInt (rem a b)
  div (CUInt a) (CUInt b) = CUInt (div a b)
  mod (CUInt a) (CUInt b) = CUInt (mod a b)
  quotRem (CUInt a) (CUInt b) = case quotRem a b of
    (q, r) -> (CUInt q, CUInt r)
  divMod (CUInt a) (CUInt b) = case divMod a b of
    (q, r) -> (CUInt q, CUInt r)
  toInteger (CUInt a) = toInteger a

instance Bounded CUInt where
  minBound = CUInt minBound
  maxBound = CUInt maxBound

instance Bits CUInt where
  CUInt a .&. CUInt b = CUInt (a .&. b)
  CUInt a .|. CUInt b = CUInt (a .|. b)
  xor (CUInt a) (CUInt b) = CUInt (xor a b)
  complement (CUInt a) = CUInt (complement a)
  shift (CUInt a) i = CUInt (shift a i)
  rotate (CUInt a) i = CUInt (rotate a i)
  zeroBits = CUInt zeroBits
  bit i = CUInt (bit i)
  setBit (CUInt a) i = CUInt (setBit a i)
  clearBit (CUInt a) i = CUInt (clearBit a i)
  testBit (CUInt a) i = testBit a i
  bitSizeMaybe (CUInt a) = bitSizeMaybe a
  bitSize (CUInt a) = bitSize a
  isSigned (CUInt a) = isSigned a
  shiftL (CUInt a) i = CUInt (shiftL a i)
  shiftR (CUInt a) i = CUInt (shiftR a i)
  rotateL (CUInt a) i = CUInt (rotateL a i)
  rotateR (CUInt a) i = CUInt (rotateR a i)
  popCount (CUInt a) = popCount a

instance FiniteBits CUInt where
  finiteBitSize (CUInt a) = finiteBitSize a

newtype CLong = CLong Int64

instance Eq CLong where
  CLong a == CLong b = a == b

instance Ord CLong where
  compare (CLong a) (CLong b) = compare a b

instance Show CLong where
  showsPrec d (CLong a) = showsPrec d a

instance Read CLong where
  readsPrec d text = [(CLong a, rest) | (a, rest) <- readsPrec d text]

instance Num CLong where
  CLong a + CLong b = CLong (a + b)
  CLong a - CLong b = CLong (a - b)
  CLong a * CLong b = CLong (a * b)
  negate (CLong a) = CLong (negate a)
  abs (CLong a) = CLong (abs a)
  signum (CLong a) = CLong (signum a)
  fromInteger n = CLong (fromInteger n)

instance Enum CLong where
  succ (CLong a) = CLong (succ a)
  pred (CLong a) = CLong (pred a)
  toEnum n = CLong (toEnum n)
  fromEnum (CLong a) = fromEnum a
  enumFrom (CLong a) = map CLong (enumFrom a)
  enumFromThen (CLong a) (CLong b) = map CLong (enumFromThen a b)
  enumFromTo (CLong a) (CLong b) = map CLong (enumFromTo a b)
  enumFromThenTo (CLong a) (CLong b) (CLong c) = map CLong (enumFromThenTo a b c)

instance Real CLong where
  toRational (CLong a) = toRational a

instance Storable CLong where
  sizeOf _ = sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek address = CLong <$> peek (castPtr address)
  poke address (CLong a) = poke (castPtr address) a

instance Integral CLong where
  quot (CLong a) (CLong b) = CLong (quot a b)
  rem (CLong a) (CLong b) = CLong (rem a b)
  div (CLong a) (CLong b) = CLong (div a b)
  mod (CLong a) (CLong b) = CLong (mod a b)
  quotRem (CLong a) (CLong b) = case quotRem a b of
    (q, r) -> (CLong q, CLong r)
  divMod (CLong a) (CLong b) = case divMod a b of
    (q, r) -> (CLong q, CLong r)
  toInteger (CLong a) = toInteger a

instance Bounded CLong where
  minBound = CLong minBound
  maxBound = CLong maxBound

instance Bits CLong where
  CLong a .&. CLong b = CLong (a .&. b)
  CLong a .|. CLong b = CLong (a .|. b)
  xor (CLong a) (CLong b) = CLong (xor a b)
  complement (CLong a) = CLong (complement a)
  shift (CLong a) i = CLong (shift a i)
  rotate (CLong a) i = CLong (rotate a i)
  zeroBits = CLong zeroBits
  bit i = CLong (bit i)
  setBit (CLong a) i = CLong (setBit a i)
  clearBit (CLong a) i = CLong (clearBit a i)
  testBit (CLong a) i = testBit a i
  bitSizeMaybe (CLong a) = bitSizeMaybe a
  bitSize (CLong a) = bitSize a
  isSigned (CLong a) = isSigned a
  shiftL (CLong a) i = CLong (shiftL a i)
  shiftR (CLong a) i = CLong (shiftR a i)
  rotateL (CLong a) i = CLong (rotateL a i)
  rotateR (CLong a) i = CLong (rotateR a i)
  popCount (CLong a) = popCount a

instance FiniteBits CLong where
  finiteBitSize (CLong a) = finiteBitSize a

newtype CULong = CULong Word64

instance Eq CULong where
  CULong a == CULong b = a == b

instance Ord CULong where
  compare (CULong a) (CULong b) = compare a b

instance Show CULong where
  showsPrec d (CULong a) = showsPrec d a

instance Read CULong where
  readsPrec d text = [(CULong a, rest) | (a, rest) <- readsPrec d text]

instance Num CULong where
  CULong a + CULong b = CULong (a + b)
  CULong a - CULong b = CULong (a - b)
  CULong a * CULong b = CULong (a * b)
  negate (CULong a) = CULong (negate a)
  abs (CULong a) = CULong (abs a)
  signum (CULong a) = CULong (signum a)
  fromInteger n = CULong (fromInteger n)

instance Enum CULong where
  succ (CULong a) = CULong (succ a)
  pred (CULong a) = CULong (pred a)
  toEnum n = CULong (toEnum n)
  fromEnum (CULong a) = fromEnum a
  enumFrom (CULong a) = map CULong (enumFrom a)
  enumFromThen (CULong a) (CULong b) = map CULong (enumFromThen a b)
  enumFromTo (CULong a) (CULong b) = map CULong (enumFromTo a b)
  enumFromThenTo (CULong a) (CULong b) (CULong c) = map CULong (enumFromThenTo a b c)

instance Real CULong where
  toRational (CULong a) = toRational a

instance Storable CULong where
  sizeOf _ = sizeOf (undefined :: Word64)
  alignment _ = alignment (undefined :: Word64)
  peek address = CULong <$> peek (castPtr address)
  poke address (CULong a) = poke (castPtr address) a

instance Integral CULong where
  quot (CULong a) (CULong b) = CULong (quot a b)
  rem (CULong a) (CULong b) = CULong (rem a b)
  div (CULong a) (CULong b) = CULong (div a b)
  mod (CULong a) (CULong b) = CULong (mod a b)
  quotRem (CULong a) (CULong b) = case quotRem a b of
    (q, r) -> (CULong q, CULong r)
  divMod (CULong a) (CULong b) = case divMod a b of
    (q, r) -> (CULong q, CULong r)
  toInteger (CULong a) = toInteger a

instance Bounded CULong where
  minBound = CULong minBound
  maxBound = CULong maxBound

instance Bits CULong where
  CULong a .&. CULong b = CULong (a .&. b)
  CULong a .|. CULong b = CULong (a .|. b)
  xor (CULong a) (CULong b) = CULong (xor a b)
  complement (CULong a) = CULong (complement a)
  shift (CULong a) i = CULong (shift a i)
  rotate (CULong a) i = CULong (rotate a i)
  zeroBits = CULong zeroBits
  bit i = CULong (bit i)
  setBit (CULong a) i = CULong (setBit a i)
  clearBit (CULong a) i = CULong (clearBit a i)
  testBit (CULong a) i = testBit a i
  bitSizeMaybe (CULong a) = bitSizeMaybe a
  bitSize (CULong a) = bitSize a
  isSigned (CULong a) = isSigned a
  shiftL (CULong a) i = CULong (shiftL a i)
  shiftR (CULong a) i = CULong (shiftR a i)
  rotateL (CULong a) i = CULong (rotateL a i)
  rotateR (CULong a) i = CULong (rotateR a i)
  popCount (CULong a) = popCount a

instance FiniteBits CULong where
  finiteBitSize (CULong a) = finiteBitSize a

newtype CLLong = CLLong Int64

instance Eq CLLong where
  CLLong a == CLLong b = a == b

instance Ord CLLong where
  compare (CLLong a) (CLLong b) = compare a b

instance Show CLLong where
  showsPrec d (CLLong a) = showsPrec d a

instance Read CLLong where
  readsPrec d text = [(CLLong a, rest) | (a, rest) <- readsPrec d text]

instance Num CLLong where
  CLLong a + CLLong b = CLLong (a + b)
  CLLong a - CLLong b = CLLong (a - b)
  CLLong a * CLLong b = CLLong (a * b)
  negate (CLLong a) = CLLong (negate a)
  abs (CLLong a) = CLLong (abs a)
  signum (CLLong a) = CLLong (signum a)
  fromInteger n = CLLong (fromInteger n)

instance Enum CLLong where
  succ (CLLong a) = CLLong (succ a)
  pred (CLLong a) = CLLong (pred a)
  toEnum n = CLLong (toEnum n)
  fromEnum (CLLong a) = fromEnum a
  enumFrom (CLLong a) = map CLLong (enumFrom a)
  enumFromThen (CLLong a) (CLLong b) = map CLLong (enumFromThen a b)
  enumFromTo (CLLong a) (CLLong b) = map CLLong (enumFromTo a b)
  enumFromThenTo (CLLong a) (CLLong b) (CLLong c) = map CLLong (enumFromThenTo a b c)

instance Real CLLong where
  toRational (CLLong a) = toRational a

instance Storable CLLong where
  sizeOf _ = sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek address = CLLong <$> peek (castPtr address)
  poke address (CLLong a) = poke (castPtr address) a

instance Integral CLLong where
  quot (CLLong a) (CLLong b) = CLLong (quot a b)
  rem (CLLong a) (CLLong b) = CLLong (rem a b)
  div (CLLong a) (CLLong b) = CLLong (div a b)
  mod (CLLong a) (CLLong b) = CLLong (mod a b)
  quotRem (CLLong a) (CLLong b) = case quotRem a b of
    (q, r) -> (CLLong q, CLLong r)
  divMod (CLLong a) (CLLong b) = case divMod a b of
    (q, r) -> (CLLong q, CLLong r)
  toInteger (CLLong a) = toInteger a

instance Bounded CLLong where
  minBound = CLLong minBound
  maxBound = CLLong maxBound

instance Bits CLLong where
  CLLong a .&. CLLong b = CLLong (a .&. b)
  CLLong a .|. CLLong b = CLLong (a .|. b)
  xor (CLLong a) (CLLong b) = CLLong (xor a b)
  complement (CLLong a) = CLLong (complement a)
  shift (CLLong a) i = CLLong (shift a i)
  rotate (CLLong a) i = CLLong (rotate a i)
  zeroBits = CLLong zeroBits
  bit i = CLLong (bit i)
  setBit (CLLong a) i = CLLong (setBit a i)
  clearBit (CLLong a) i = CLLong (clearBit a i)
  testBit (CLLong a) i = testBit a i
  bitSizeMaybe (CLLong a) = bitSizeMaybe a
  bitSize (CLLong a) = bitSize a
  isSigned (CLLong a) = isSigned a
  shiftL (CLLong a) i = CLLong (shiftL a i)
  shiftR (CLLong a) i = CLLong (shiftR a i)
  rotateL (CLLong a) i = CLLong (rotateL a i)
  rotateR (CLLong a) i = CLLong (rotateR a i)
  popCount (CLLong a) = popCount a

instance FiniteBits CLLong where
  finiteBitSize (CLLong a) = finiteBitSize a

newtype CULLong = CULLong Word64

instance Eq CULLong where
  CULLong a == CULLong b = a == b

instance Ord CULLong where
  compare (CULLong a) (CULLong b) = compare a b

instance Show CULLong where
  showsPrec d (CULLong a) = showsPrec d a

instance Read CULLong where
  readsPrec d text = [(CULLong a, rest) | (a, rest) <- readsPrec d text]

instance Num CULLong where
  CULLong a + CULLong b = CULLong (a + b)
  CULLong a - CULLong b = CULLong (a - b)
  CULLong a * CULLong b = CULLong (a * b)
  negate (CULLong a) = CULLong (negate a)
  abs (CULLong a) = CULLong (abs a)
  signum (CULLong a) = CULLong (signum a)
  fromInteger n = CULLong (fromInteger n)

instance Enum CULLong where
  succ (CULLong a) = CULLong (succ a)
  pred (CULLong a) = CULLong (pred a)
  toEnum n = CULLong (toEnum n)
  fromEnum (CULLong a) = fromEnum a
  enumFrom (CULLong a) = map CULLong (enumFrom a)
  enumFromThen (CULLong a) (CULLong b) = map CULLong (enumFromThen a b)
  enumFromTo (CULLong a) (CULLong b) = map CULLong (enumFromTo a b)
  enumFromThenTo (CULLong a) (CULLong b) (CULLong c) = map CULLong (enumFromThenTo a b c)

instance Real CULLong where
  toRational (CULLong a) = toRational a

instance Storable CULLong where
  sizeOf _ = sizeOf (undefined :: Word64)
  alignment _ = alignment (undefined :: Word64)
  peek address = CULLong <$> peek (castPtr address)
  poke address (CULLong a) = poke (castPtr address) a

instance Integral CULLong where
  quot (CULLong a) (CULLong b) = CULLong (quot a b)
  rem (CULLong a) (CULLong b) = CULLong (rem a b)
  div (CULLong a) (CULLong b) = CULLong (div a b)
  mod (CULLong a) (CULLong b) = CULLong (mod a b)
  quotRem (CULLong a) (CULLong b) = case quotRem a b of
    (q, r) -> (CULLong q, CULLong r)
  divMod (CULLong a) (CULLong b) = case divMod a b of
    (q, r) -> (CULLong q, CULLong r)
  toInteger (CULLong a) = toInteger a

instance Bounded CULLong where
  minBound = CULLong minBound
  maxBound = CULLong maxBound

instance Bits CULLong where
  CULLong a .&. CULLong b = CULLong (a .&. b)
  CULLong a .|. CULLong b = CULLong (a .|. b)
  xor (CULLong a) (CULLong b) = CULLong (xor a b)
  complement (CULLong a) = CULLong (complement a)
  shift (CULLong a) i = CULLong (shift a i)
  rotate (CULLong a) i = CULLong (rotate a i)
  zeroBits = CULLong zeroBits
  bit i = CULLong (bit i)
  setBit (CULLong a) i = CULLong (setBit a i)
  clearBit (CULLong a) i = CULLong (clearBit a i)
  testBit (CULLong a) i = testBit a i
  bitSizeMaybe (CULLong a) = bitSizeMaybe a
  bitSize (CULLong a) = bitSize a
  isSigned (CULLong a) = isSigned a
  shiftL (CULLong a) i = CULLong (shiftL a i)
  shiftR (CULLong a) i = CULLong (shiftR a i)
  rotateL (CULLong a) i = CULLong (rotateL a i)
  rotateR (CULLong a) i = CULLong (rotateR a i)
  popCount (CULLong a) = popCount a

instance FiniteBits CULLong where
  finiteBitSize (CULLong a) = finiteBitSize a

newtype CPtrdiff = CPtrdiff Int64

instance Eq CPtrdiff where
  CPtrdiff a == CPtrdiff b = a == b

instance Ord CPtrdiff where
  compare (CPtrdiff a) (CPtrdiff b) = compare a b

instance Show CPtrdiff where
  showsPrec d (CPtrdiff a) = showsPrec d a

instance Read CPtrdiff where
  readsPrec d text = [(CPtrdiff a, rest) | (a, rest) <- readsPrec d text]

instance Num CPtrdiff where
  CPtrdiff a + CPtrdiff b = CPtrdiff (a + b)
  CPtrdiff a - CPtrdiff b = CPtrdiff (a - b)
  CPtrdiff a * CPtrdiff b = CPtrdiff (a * b)
  negate (CPtrdiff a) = CPtrdiff (negate a)
  abs (CPtrdiff a) = CPtrdiff (abs a)
  signum (CPtrdiff a) = CPtrdiff (signum a)
  fromInteger n = CPtrdiff (fromInteger n)

instance Enum CPtrdiff where
  succ (CPtrdiff a) = CPtrdiff (succ a)
  pred (CPtrdiff a) = CPtrdiff (pred a)
  toEnum n = CPtrdiff (toEnum n)
  fromEnum (CPtrdiff a) = fromEnum a
  enumFrom (CPtrdiff a) = map CPtrdiff (enumFrom a)
  enumFromThen (CPtrdiff a) (CPtrdiff b) = map CPtrdiff (enumFromThen a b)
  enumFromTo (CPtrdiff a) (CPtrdiff b) = map CPtrdiff (enumFromTo a b)
  enumFromThenTo (CPtrdiff a) (CPtrdiff b) (CPtrdiff c) = map CPtrdiff (enumFromThenTo a b c)

instance Real CPtrdiff where
  toRational (CPtrdiff a) = toRational a

instance Storable CPtrdiff where
  sizeOf _ = sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek address = CPtrdiff <$> peek (castPtr address)
  poke address (CPtrdiff a) = poke (castPtr address) a

instance Integral CPtrdiff where
  quot (CPtrdiff a) (CPtrdiff b) = CPtrdiff (quot a b)
  rem (CPtrdiff a) (CPtrdiff b) = CPtrdiff (rem a b)
  div (CPtrdiff a) (CPtrdiff b) = CPtrdiff (div a b)
  mod (CPtrdiff a) (CPtrdiff b) = CPtrdiff (mod a b)
  quotRem (CPtrdiff a) (CPtrdiff b) = case quotRem a b of
    (q, r) -> (CPtrdiff q, CPtrdiff r)
  divMod (CPtrdiff a) (CPtrdiff b) = case divMod a b of
    (q, r) -> (CPtrdiff q, CPtrdiff r)
  toInteger (CPtrdiff a) = toInteger a

instance Bounded CPtrdiff where
  minBound = CPtrdiff minBound
  maxBound = CPtrdiff maxBound

instance Bits CPtrdiff where
  CPtrdiff a .&. CPtrdiff b = CPtrdiff (a .&. b)
  CPtrdiff a .|. CPtrdiff b = CPtrdiff (a .|. b)
  xor (CPtrdiff a) (CPtrdiff b) = CPtrdiff (xor a b)
  complement (CPtrdiff a) = CPtrdiff (complement a)
  shift (CPtrdiff a) i = CPtrdiff (shift a i)
  rotate (CPtrdiff a) i = CPtrdiff (rotate a i)
  zeroBits = CPtrdiff zeroBits
  bit i = CPtrdiff (bit i)
  setBit (CPtrdiff a) i = CPtrdiff (setBit a i)
  clearBit (CPtrdiff a) i = CPtrdiff (clearBit a i)
  testBit (CPtrdiff a) i = testBit a i
  bitSizeMaybe (CPtrdiff a) = bitSizeMaybe a
  bitSize (CPtrdiff a) = bitSize a
  isSigned (CPtrdiff a) = isSigned a
  shiftL (CPtrdiff a) i = CPtrdiff (shiftL a i)
  shiftR (CPtrdiff a) i = CPtrdiff (shiftR a i)
  rotateL (CPtrdiff a) i = CPtrdiff (rotateL a i)
  rotateR (CPtrdiff a) i = CPtrdiff (rotateR a i)
  popCount (CPtrdiff a) = popCount a

instance FiniteBits CPtrdiff where
  finiteBitSize (CPtrdiff a) = finiteBitSize a

newtype CSize = CSize Word64

instance Eq CSize where
  CSize a == CSize b = a == b

instance Ord CSize where
  compare (CSize a) (CSize b) = compare a b

instance Show CSize where
  showsPrec d (CSize a) = showsPrec d a

instance Read CSize where
  readsPrec d text = [(CSize a, rest) | (a, rest) <- readsPrec d text]

instance Num CSize where
  CSize a + CSize b = CSize (a + b)
  CSize a - CSize b = CSize (a - b)
  CSize a * CSize b = CSize (a * b)
  negate (CSize a) = CSize (negate a)
  abs (CSize a) = CSize (abs a)
  signum (CSize a) = CSize (signum a)
  fromInteger n = CSize (fromInteger n)

instance Enum CSize where
  succ (CSize a) = CSize (succ a)
  pred (CSize a) = CSize (pred a)
  toEnum n = CSize (toEnum n)
  fromEnum (CSize a) = fromEnum a
  enumFrom (CSize a) = map CSize (enumFrom a)
  enumFromThen (CSize a) (CSize b) = map CSize (enumFromThen a b)
  enumFromTo (CSize a) (CSize b) = map CSize (enumFromTo a b)
  enumFromThenTo (CSize a) (CSize b) (CSize c) = map CSize (enumFromThenTo a b c)

instance Real CSize where
  toRational (CSize a) = toRational a

instance Storable CSize where
  sizeOf _ = sizeOf (undefined :: Word64)
  alignment _ = alignment (undefined :: Word64)
  peek address = CSize <$> peek (castPtr address)
  poke address (CSize a) = poke (castPtr address) a

instance Integral CSize where
  quot (CSize a) (CSize b) = CSize (quot a b)
  rem (CSize a) (CSize b) = CSize (rem a b)
  div (CSize a) (CSize b) = CSize (div a b)
  mod (CSize a) (CSize b) = CSize (mod a b)
  quotRem (CSize a) (CSize b) = case quotRem a b of
    (q, r) -> (CSize q, CSize r)
  divMod (CSize a) (CSize b) = case divMod a b of
    (q, r) -> (CSize q, CSize r)
  toInteger (CSize a) = toInteger a

instance Bounded CSize where
  minBound = CSize minBound
  maxBound = CSize maxBound

instance Bits CSize where
  CSize a .&. CSize b = CSize (a .&. b)
  CSize a .|. CSize b = CSize (a .|. b)
  xor (CSize a) (CSize b) = CSize (xor a b)
  complement (CSize a) = CSize (complement a)
  shift (CSize a) i = CSize (shift a i)
  rotate (CSize a) i = CSize (rotate a i)
  zeroBits = CSize zeroBits
  bit i = CSize (bit i)
  setBit (CSize a) i = CSize (setBit a i)
  clearBit (CSize a) i = CSize (clearBit a i)
  testBit (CSize a) i = testBit a i
  bitSizeMaybe (CSize a) = bitSizeMaybe a
  bitSize (CSize a) = bitSize a
  isSigned (CSize a) = isSigned a
  shiftL (CSize a) i = CSize (shiftL a i)
  shiftR (CSize a) i = CSize (shiftR a i)
  rotateL (CSize a) i = CSize (rotateL a i)
  rotateR (CSize a) i = CSize (rotateR a i)
  popCount (CSize a) = popCount a

instance FiniteBits CSize where
  finiteBitSize (CSize a) = finiteBitSize a

newtype CWchar = CWchar Int32

instance Eq CWchar where
  CWchar a == CWchar b = a == b

instance Ord CWchar where
  compare (CWchar a) (CWchar b) = compare a b

instance Show CWchar where
  showsPrec d (CWchar a) = showsPrec d a

instance Read CWchar where
  readsPrec d text = [(CWchar a, rest) | (a, rest) <- readsPrec d text]

instance Num CWchar where
  CWchar a + CWchar b = CWchar (a + b)
  CWchar a - CWchar b = CWchar (a - b)
  CWchar a * CWchar b = CWchar (a * b)
  negate (CWchar a) = CWchar (negate a)
  abs (CWchar a) = CWchar (abs a)
  signum (CWchar a) = CWchar (signum a)
  fromInteger n = CWchar (fromInteger n)

instance Enum CWchar where
  succ (CWchar a) = CWchar (succ a)
  pred (CWchar a) = CWchar (pred a)
  toEnum n = CWchar (toEnum n)
  fromEnum (CWchar a) = fromEnum a
  enumFrom (CWchar a) = map CWchar (enumFrom a)
  enumFromThen (CWchar a) (CWchar b) = map CWchar (enumFromThen a b)
  enumFromTo (CWchar a) (CWchar b) = map CWchar (enumFromTo a b)
  enumFromThenTo (CWchar a) (CWchar b) (CWchar c) = map CWchar (enumFromThenTo a b c)

instance Real CWchar where
  toRational (CWchar a) = toRational a

instance Storable CWchar where
  sizeOf _ = sizeOf (undefined :: Int32)
  alignment _ = alignment (undefined :: Int32)
  peek address = CWchar <$> peek (castPtr address)
  poke address (CWchar a) = poke (castPtr address) a

instance Integral CWchar where
  quot (CWchar a) (CWchar b) = CWchar (quot a b)
  rem (CWchar a) (CWchar b) = CWchar (rem a b)
  div (CWchar a) (CWchar b) = CWchar (div a b)
  mod (CWchar a) (CWchar b) = CWchar (mod a b)
  quotRem (CWchar a) (CWchar b) = case quotRem a b of
    (q, r) -> (CWchar q, CWchar r)
  divMod (CWchar a) (CWchar b) = case divMod a b of
    (q, r) -> (CWchar q, CWchar r)
  toInteger (CWchar a) = toInteger a

instance Bounded CWchar where
  minBound = CWchar minBound
  maxBound = CWchar maxBound

instance Bits CWchar where
  CWchar a .&. CWchar b = CWchar (a .&. b)
  CWchar a .|. CWchar b = CWchar (a .|. b)
  xor (CWchar a) (CWchar b) = CWchar (xor a b)
  complement (CWchar a) = CWchar (complement a)
  shift (CWchar a) i = CWchar (shift a i)
  rotate (CWchar a) i = CWchar (rotate a i)
  zeroBits = CWchar zeroBits
  bit i = CWchar (bit i)
  setBit (CWchar a) i = CWchar (setBit a i)
  clearBit (CWchar a) i = CWchar (clearBit a i)
  testBit (CWchar a) i = testBit a i
  bitSizeMaybe (CWchar a) = bitSizeMaybe a
  bitSize (CWchar a) = bitSize a
  isSigned (CWchar a) = isSigned a
  shiftL (CWchar a) i = CWchar (shiftL a i)
  shiftR (CWchar a) i = CWchar (shiftR a i)
  rotateL (CWchar a) i = CWchar (rotateL a i)
  rotateR (CWchar a) i = CWchar (rotateR a i)
  popCount (CWchar a) = popCount a

instance FiniteBits CWchar where
  finiteBitSize (CWchar a) = finiteBitSize a

newtype CSigAtomic = CSigAtomic Int32

instance Eq CSigAtomic where
  CSigAtomic a == CSigAtomic b = a == b

instance Ord CSigAtomic where
  compare (CSigAtomic a) (CSigAtomic b) = compare a b

instance Show CSigAtomic where
  showsPrec d (CSigAtomic a) = showsPrec d a

instance Read CSigAtomic where
  readsPrec d text = [(CSigAtomic a, rest) | (a, rest) <- readsPrec d text]

instance Num CSigAtomic where
  CSigAtomic a + CSigAtomic b = CSigAtomic (a + b)
  CSigAtomic a - CSigAtomic b = CSigAtomic (a - b)
  CSigAtomic a * CSigAtomic b = CSigAtomic (a * b)
  negate (CSigAtomic a) = CSigAtomic (negate a)
  abs (CSigAtomic a) = CSigAtomic (abs a)
  signum (CSigAtomic a) = CSigAtomic (signum a)
  fromInteger n = CSigAtomic (fromInteger n)

instance Enum CSigAtomic where
  succ (CSigAtomic a) = CSigAtomic (succ a)
  pred (CSigAtomic a) = CSigAtomic (pred a)
  toEnum n = CSigAtomic (toEnum n)
  fromEnum (CSigAtomic a) = fromEnum a
  enumFrom (CSigAtomic a) = map CSigAtomic (enumFrom a)
  enumFromThen (CSigAtomic a) (CSigAtomic b) = map CSigAtomic (enumFromThen a b)
  enumFromTo (CSigAtomic a) (CSigAtomic b) = map CSigAtomic (enumFromTo a b)
  enumFromThenTo (CSigAtomic a) (CSigAtomic b) (CSigAtomic c) = map CSigAtomic (enumFromThenTo a b c)

instance Real CSigAtomic where
  toRational (CSigAtomic a) = toRational a

instance Storable CSigAtomic where
  sizeOf _ = sizeOf (undefined :: Int32)
  alignment _ = alignment (undefined :: Int32)
  peek address = CSigAtomic <$> peek (castPtr address)
  poke address (CSigAtomic a) = poke (castPtr address) a

instance Integral CSigAtomic where
  quot (CSigAtomic a) (CSigAtomic b) = CSigAtomic (quot a b)
  rem (CSigAtomic a) (CSigAtomic b) = CSigAtomic (rem a b)
  div (CSigAtomic a) (CSigAtomic b) = CSigAtomic (div a b)
  mod (CSigAtomic a) (CSigAtomic b) = CSigAtomic (mod a b)
  quotRem (CSigAtomic a) (CSigAtomic b) = case quotRem a b of
    (q, r) -> (CSigAtomic q, CSigAtomic r)
  divMod (CSigAtomic a) (CSigAtomic b) = case divMod a b of
    (q, r) -> (CSigAtomic q, CSigAtomic r)
  toInteger (CSigAtomic a) = toInteger a

instance Bounded CSigAtomic where
  minBound = CSigAtomic minBound
  maxBound = CSigAtomic maxBound

instance Bits CSigAtomic where
  CSigAtomic a .&. CSigAtomic b = CSigAtomic (a .&. b)
  CSigAtomic a .|. CSigAtomic b = CSigAtomic (a .|. b)
  xor (CSigAtomic a) (CSigAtomic b) = CSigAtomic (xor a b)
  complement (CSigAtomic a) = CSigAtomic (complement a)
  shift (CSigAtomic a) i = CSigAtomic (shift a i)
  rotate (CSigAtomic a) i = CSigAtomic (rotate a i)
  zeroBits = CSigAtomic zeroBits
  bit i = CSigAtomic (bit i)
  setBit (CSigAtomic a) i = CSigAtomic (setBit a i)
  clearBit (CSigAtomic a) i = CSigAtomic (clearBit a i)
  testBit (CSigAtomic a) i = testBit a i
  bitSizeMaybe (CSigAtomic a) = bitSizeMaybe a
  bitSize (CSigAtomic a) = bitSize a
  isSigned (CSigAtomic a) = isSigned a
  shiftL (CSigAtomic a) i = CSigAtomic (shiftL a i)
  shiftR (CSigAtomic a) i = CSigAtomic (shiftR a i)
  rotateL (CSigAtomic a) i = CSigAtomic (rotateL a i)
  rotateR (CSigAtomic a) i = CSigAtomic (rotateR a i)
  popCount (CSigAtomic a) = popCount a

instance FiniteBits CSigAtomic where
  finiteBitSize (CSigAtomic a) = finiteBitSize a

newtype CBool = CBool Word8

instance Eq CBool where
  CBool a == CBool b = a == b

instance Ord CBool where
  compare (CBool a) (CBool b) = compare a b

instance Show CBool where
  showsPrec d (CBool a) = showsPrec d a

instance Read CBool where
  readsPrec d text = [(CBool a, rest) | (a, rest) <- readsPrec d text]

instance Num CBool where
  CBool a + CBool b = CBool (a + b)
  CBool a - CBool b = CBool (a - b)
  CBool a * CBool b = CBool (a * b)
  negate (CBool a) = CBool (negate a)
  abs (CBool a) = CBool (abs a)
  signum (CBool a) = CBool (signum a)
  fromInteger n = CBool (fromInteger n)

instance Enum CBool where
  succ (CBool a) = CBool (succ a)
  pred (CBool a) = CBool (pred a)
  toEnum n = CBool (toEnum n)
  fromEnum (CBool a) = fromEnum a
  enumFrom (CBool a) = map CBool (enumFrom a)
  enumFromThen (CBool a) (CBool b) = map CBool (enumFromThen a b)
  enumFromTo (CBool a) (CBool b) = map CBool (enumFromTo a b)
  enumFromThenTo (CBool a) (CBool b) (CBool c) = map CBool (enumFromThenTo a b c)

instance Real CBool where
  toRational (CBool a) = toRational a

instance Storable CBool where
  sizeOf _ = sizeOf (undefined :: Word8)
  alignment _ = alignment (undefined :: Word8)
  peek address = CBool <$> peek (castPtr address)
  poke address (CBool a) = poke (castPtr address) a

instance Integral CBool where
  quot (CBool a) (CBool b) = CBool (quot a b)
  rem (CBool a) (CBool b) = CBool (rem a b)
  div (CBool a) (CBool b) = CBool (div a b)
  mod (CBool a) (CBool b) = CBool (mod a b)
  quotRem (CBool a) (CBool b) = case quotRem a b of
    (q, r) -> (CBool q, CBool r)
  divMod (CBool a) (CBool b) = case divMod a b of
    (q, r) -> (CBool q, CBool r)
  toInteger (CBool a) = toInteger a

instance Bounded CBool where
  minBound = CBool minBound
  maxBound = CBool maxBound

instance Bits CBool where
  CBool a .&. CBool b = CBool (a .&. b)
  CBool a .|. CBool b = CBool (a .|. b)
  xor (CBool a) (CBool b) = CBool (xor a b)
  complement (CBool a) = CBool (complement a)
  shift (CBool a) i = CBool (shift a i)
  rotate (CBool a) i = CBool (rotate a i)
  zeroBits = CBool zeroBits
  bit i = CBool (bit i)
  setBit (CBool a) i = CBool (setBit a i)
  clearBit (CBool a) i = CBool (clearBit a i)
  testBit (CBool a) i = testBit a i
  bitSizeMaybe (CBool a) = bitSizeMaybe a
  bitSize (CBool a) = bitSize a
  isSigned (CBool a) = isSigned a
  shiftL (CBool a) i = CBool (shiftL a i)
  shiftR (CBool a) i = CBool (shiftR a i)
  rotateL (CBool a) i = CBool (rotateL a i)
  rotateR (CBool a) i = CBool (rotateR a i)
  popCount (CBool a) = popCount a

instance FiniteBits CBool where
  finiteBitSize (CBool a) = finiteBitSize a

newtype CIntPtr = CIntPtr Int64

instance Eq CIntPtr where
  CIntPtr a == CIntPtr b = a == b

instance Ord CIntPtr where
  compare (CIntPtr a) (CIntPtr b) = compare a b

instance Show CIntPtr where
  showsPrec d (CIntPtr a) = showsPrec d a

instance Read CIntPtr where
  readsPrec d text = [(CIntPtr a, rest) | (a, rest) <- readsPrec d text]

instance Num CIntPtr where
  CIntPtr a + CIntPtr b = CIntPtr (a + b)
  CIntPtr a - CIntPtr b = CIntPtr (a - b)
  CIntPtr a * CIntPtr b = CIntPtr (a * b)
  negate (CIntPtr a) = CIntPtr (negate a)
  abs (CIntPtr a) = CIntPtr (abs a)
  signum (CIntPtr a) = CIntPtr (signum a)
  fromInteger n = CIntPtr (fromInteger n)

instance Enum CIntPtr where
  succ (CIntPtr a) = CIntPtr (succ a)
  pred (CIntPtr a) = CIntPtr (pred a)
  toEnum n = CIntPtr (toEnum n)
  fromEnum (CIntPtr a) = fromEnum a
  enumFrom (CIntPtr a) = map CIntPtr (enumFrom a)
  enumFromThen (CIntPtr a) (CIntPtr b) = map CIntPtr (enumFromThen a b)
  enumFromTo (CIntPtr a) (CIntPtr b) = map CIntPtr (enumFromTo a b)
  enumFromThenTo (CIntPtr a) (CIntPtr b) (CIntPtr c) = map CIntPtr (enumFromThenTo a b c)

instance Real CIntPtr where
  toRational (CIntPtr a) = toRational a

instance Storable CIntPtr where
  sizeOf _ = sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek address = CIntPtr <$> peek (castPtr address)
  poke address (CIntPtr a) = poke (castPtr address) a

instance Integral CIntPtr where
  quot (CIntPtr a) (CIntPtr b) = CIntPtr (quot a b)
  rem (CIntPtr a) (CIntPtr b) = CIntPtr (rem a b)
  div (CIntPtr a) (CIntPtr b) = CIntPtr (div a b)
  mod (CIntPtr a) (CIntPtr b) = CIntPtr (mod a b)
  quotRem (CIntPtr a) (CIntPtr b) = case quotRem a b of
    (q, r) -> (CIntPtr q, CIntPtr r)
  divMod (CIntPtr a) (CIntPtr b) = case divMod a b of
    (q, r) -> (CIntPtr q, CIntPtr r)
  toInteger (CIntPtr a) = toInteger a

instance Bounded CIntPtr where
  minBound = CIntPtr minBound
  maxBound = CIntPtr maxBound

instance Bits CIntPtr where
  CIntPtr a .&. CIntPtr b = CIntPtr (a .&. b)
  CIntPtr a .|. CIntPtr b = CIntPtr (a .|. b)
  xor (CIntPtr a) (CIntPtr b) = CIntPtr (xor a b)
  complement (CIntPtr a) = CIntPtr (complement a)
  shift (CIntPtr a) i = CIntPtr (shift a i)
  rotate (CIntPtr a) i = CIntPtr (rotate a i)
  zeroBits = CIntPtr zeroBits
  bit i = CIntPtr (bit i)
  setBit (CIntPtr a) i = CIntPtr (setBit a i)
  clearBit (CIntPtr a) i = CIntPtr (clearBit a i)
  testBit (CIntPtr a) i = testBit a i
  bitSizeMaybe (CIntPtr a) = bitSizeMaybe a
  bitSize (CIntPtr a) = bitSize a
  isSigned (CIntPtr a) = isSigned a
  shiftL (CIntPtr a) i = CIntPtr (shiftL a i)
  shiftR (CIntPtr a) i = CIntPtr (shiftR a i)
  rotateL (CIntPtr a) i = CIntPtr (rotateL a i)
  rotateR (CIntPtr a) i = CIntPtr (rotateR a i)
  popCount (CIntPtr a) = popCount a

instance FiniteBits CIntPtr where
  finiteBitSize (CIntPtr a) = finiteBitSize a

newtype CUIntPtr = CUIntPtr Word64

instance Eq CUIntPtr where
  CUIntPtr a == CUIntPtr b = a == b

instance Ord CUIntPtr where
  compare (CUIntPtr a) (CUIntPtr b) = compare a b

instance Show CUIntPtr where
  showsPrec d (CUIntPtr a) = showsPrec d a

instance Read CUIntPtr where
  readsPrec d text = [(CUIntPtr a, rest) | (a, rest) <- readsPrec d text]

instance Num CUIntPtr where
  CUIntPtr a + CUIntPtr b = CUIntPtr (a + b)
  CUIntPtr a - CUIntPtr b = CUIntPtr (a - b)
  CUIntPtr a * CUIntPtr b = CUIntPtr (a * b)
  negate (CUIntPtr a) = CUIntPtr (negate a)
  abs (CUIntPtr a) = CUIntPtr (abs a)
  signum (CUIntPtr a) = CUIntPtr (signum a)
  fromInteger n = CUIntPtr (fromInteger n)

instance Enum CUIntPtr where
  succ (CUIntPtr a) = CUIntPtr (succ a)
  pred (CUIntPtr a) = CUIntPtr (pred a)
  toEnum n = CUIntPtr (toEnum n)
  fromEnum (CUIntPtr a) = fromEnum a
  enumFrom (CUIntPtr a) = map CUIntPtr (enumFrom a)
  enumFromThen (CUIntPtr a) (CUIntPtr b) = map CUIntPtr (enumFromThen a b)
  enumFromTo (CUIntPtr a) (CUIntPtr b) = map CUIntPtr (enumFromTo a b)
  enumFromThenTo (CUIntPtr a) (CUIntPtr b) (CUIntPtr c) = map CUIntPtr (enumFromThenTo a b c)

instance Real CUIntPtr where
  toRational (CUIntPtr a) = toRational a

instance Storable CUIntPtr where
  sizeOf _ = sizeOf (undefined :: Word64)
  alignment _ = alignment (undefined :: Word64)
  peek address = CUIntPtr <$> peek (castPtr address)
  poke address (CUIntPtr a) = poke (castPtr address) a

instance Integral CUIntPtr where
  quot (CUIntPtr a) (CUIntPtr b) = CUIntPtr (quot a b)
  rem (CUIntPtr a) (CUIntPtr b) = CUIntPtr (rem a b)
  div (CUIntPtr a) (CUIntPtr b) = CUIntPtr (div a b)
  mod (CUIntPtr a) (CUIntPtr b) = CUIntPtr (mod a b)
  quotRem (CUIntPtr a) (CUIntPtr b) = case quotRem a b of
    (q, r) -> (CUIntPtr q, CUIntPtr r)
  divMod (CUIntPtr a) (CUIntPtr b) = case divMod a b of
    (q, r) -> (CUIntPtr q, CUIntPtr r)
  toInteger (CUIntPtr a) = toInteger a

instance Bounded CUIntPtr where
  minBound = CUIntPtr minBound
  maxBound = CUIntPtr maxBound

instance Bits CUIntPtr where
  CUIntPtr a .&. CUIntPtr b = CUIntPtr (a .&. b)
  CUIntPtr a .|. CUIntPtr b = CUIntPtr (a .|. b)
  xor (CUIntPtr a) (CUIntPtr b) = CUIntPtr (xor a b)
  complement (CUIntPtr a) = CUIntPtr (complement a)
  shift (CUIntPtr a) i = CUIntPtr (shift a i)
  rotate (CUIntPtr a) i = CUIntPtr (rotate a i)
  zeroBits = CUIntPtr zeroBits
  bit i = CUIntPtr (bit i)
  setBit (CUIntPtr a) i = CUIntPtr (setBit a i)
  clearBit (CUIntPtr a) i = CUIntPtr (clearBit a i)
  testBit (CUIntPtr a) i = testBit a i
  bitSizeMaybe (CUIntPtr a) = bitSizeMaybe a
  bitSize (CUIntPtr a) = bitSize a
  isSigned (CUIntPtr a) = isSigned a
  shiftL (CUIntPtr a) i = CUIntPtr (shiftL a i)
  shiftR (CUIntPtr a) i = CUIntPtr (shiftR a i)
  rotateL (CUIntPtr a) i = CUIntPtr (rotateL a i)
  rotateR (CUIntPtr a) i = CUIntPtr (rotateR a i)
  popCount (CUIntPtr a) = popCount a

instance FiniteBits CUIntPtr where
  finiteBitSize (CUIntPtr a) = finiteBitSize a

newtype CIntMax = CIntMax Int64

instance Eq CIntMax where
  CIntMax a == CIntMax b = a == b

instance Ord CIntMax where
  compare (CIntMax a) (CIntMax b) = compare a b

instance Show CIntMax where
  showsPrec d (CIntMax a) = showsPrec d a

instance Read CIntMax where
  readsPrec d text = [(CIntMax a, rest) | (a, rest) <- readsPrec d text]

instance Num CIntMax where
  CIntMax a + CIntMax b = CIntMax (a + b)
  CIntMax a - CIntMax b = CIntMax (a - b)
  CIntMax a * CIntMax b = CIntMax (a * b)
  negate (CIntMax a) = CIntMax (negate a)
  abs (CIntMax a) = CIntMax (abs a)
  signum (CIntMax a) = CIntMax (signum a)
  fromInteger n = CIntMax (fromInteger n)

instance Enum CIntMax where
  succ (CIntMax a) = CIntMax (succ a)
  pred (CIntMax a) = CIntMax (pred a)
  toEnum n = CIntMax (toEnum n)
  fromEnum (CIntMax a) = fromEnum a
  enumFrom (CIntMax a) = map CIntMax (enumFrom a)
  enumFromThen (CIntMax a) (CIntMax b) = map CIntMax (enumFromThen a b)
  enumFromTo (CIntMax a) (CIntMax b) = map CIntMax (enumFromTo a b)
  enumFromThenTo (CIntMax a) (CIntMax b) (CIntMax c) = map CIntMax (enumFromThenTo a b c)

instance Real CIntMax where
  toRational (CIntMax a) = toRational a

instance Storable CIntMax where
  sizeOf _ = sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek address = CIntMax <$> peek (castPtr address)
  poke address (CIntMax a) = poke (castPtr address) a

instance Integral CIntMax where
  quot (CIntMax a) (CIntMax b) = CIntMax (quot a b)
  rem (CIntMax a) (CIntMax b) = CIntMax (rem a b)
  div (CIntMax a) (CIntMax b) = CIntMax (div a b)
  mod (CIntMax a) (CIntMax b) = CIntMax (mod a b)
  quotRem (CIntMax a) (CIntMax b) = case quotRem a b of
    (q, r) -> (CIntMax q, CIntMax r)
  divMod (CIntMax a) (CIntMax b) = case divMod a b of
    (q, r) -> (CIntMax q, CIntMax r)
  toInteger (CIntMax a) = toInteger a

instance Bounded CIntMax where
  minBound = CIntMax minBound
  maxBound = CIntMax maxBound

instance Bits CIntMax where
  CIntMax a .&. CIntMax b = CIntMax (a .&. b)
  CIntMax a .|. CIntMax b = CIntMax (a .|. b)
  xor (CIntMax a) (CIntMax b) = CIntMax (xor a b)
  complement (CIntMax a) = CIntMax (complement a)
  shift (CIntMax a) i = CIntMax (shift a i)
  rotate (CIntMax a) i = CIntMax (rotate a i)
  zeroBits = CIntMax zeroBits
  bit i = CIntMax (bit i)
  setBit (CIntMax a) i = CIntMax (setBit a i)
  clearBit (CIntMax a) i = CIntMax (clearBit a i)
  testBit (CIntMax a) i = testBit a i
  bitSizeMaybe (CIntMax a) = bitSizeMaybe a
  bitSize (CIntMax a) = bitSize a
  isSigned (CIntMax a) = isSigned a
  shiftL (CIntMax a) i = CIntMax (shiftL a i)
  shiftR (CIntMax a) i = CIntMax (shiftR a i)
  rotateL (CIntMax a) i = CIntMax (rotateL a i)
  rotateR (CIntMax a) i = CIntMax (rotateR a i)
  popCount (CIntMax a) = popCount a

instance FiniteBits CIntMax where
  finiteBitSize (CIntMax a) = finiteBitSize a

newtype CUIntMax = CUIntMax Word64

instance Eq CUIntMax where
  CUIntMax a == CUIntMax b = a == b

instance Ord CUIntMax where
  compare (CUIntMax a) (CUIntMax b) = compare a b

instance Show CUIntMax where
  showsPrec d (CUIntMax a) = showsPrec d a

instance Read CUIntMax where
  readsPrec d text = [(CUIntMax a, rest) | (a, rest) <- readsPrec d text]

instance Num CUIntMax where
  CUIntMax a + CUIntMax b = CUIntMax (a + b)
  CUIntMax a - CUIntMax b = CUIntMax (a - b)
  CUIntMax a * CUIntMax b = CUIntMax (a * b)
  negate (CUIntMax a) = CUIntMax (negate a)
  abs (CUIntMax a) = CUIntMax (abs a)
  signum (CUIntMax a) = CUIntMax (signum a)
  fromInteger n = CUIntMax (fromInteger n)

instance Enum CUIntMax where
  succ (CUIntMax a) = CUIntMax (succ a)
  pred (CUIntMax a) = CUIntMax (pred a)
  toEnum n = CUIntMax (toEnum n)
  fromEnum (CUIntMax a) = fromEnum a
  enumFrom (CUIntMax a) = map CUIntMax (enumFrom a)
  enumFromThen (CUIntMax a) (CUIntMax b) = map CUIntMax (enumFromThen a b)
  enumFromTo (CUIntMax a) (CUIntMax b) = map CUIntMax (enumFromTo a b)
  enumFromThenTo (CUIntMax a) (CUIntMax b) (CUIntMax c) = map CUIntMax (enumFromThenTo a b c)

instance Real CUIntMax where
  toRational (CUIntMax a) = toRational a

instance Storable CUIntMax where
  sizeOf _ = sizeOf (undefined :: Word64)
  alignment _ = alignment (undefined :: Word64)
  peek address = CUIntMax <$> peek (castPtr address)
  poke address (CUIntMax a) = poke (castPtr address) a

instance Integral CUIntMax where
  quot (CUIntMax a) (CUIntMax b) = CUIntMax (quot a b)
  rem (CUIntMax a) (CUIntMax b) = CUIntMax (rem a b)
  div (CUIntMax a) (CUIntMax b) = CUIntMax (div a b)
  mod (CUIntMax a) (CUIntMax b) = CUIntMax (mod a b)
  quotRem (CUIntMax a) (CUIntMax b) = case quotRem a b of
    (q, r) -> (CUIntMax q, CUIntMax r)
  divMod (CUIntMax a) (CUIntMax b) = case divMod a b of
    (q, r) -> (CUIntMax q, CUIntMax r)
  toInteger (CUIntMax a) = toInteger a

instance Bounded CUIntMax where
  minBound = CUIntMax minBound
  maxBound = CUIntMax maxBound

instance Bits CUIntMax where
  CUIntMax a .&. CUIntMax b = CUIntMax (a .&. b)
  CUIntMax a .|. CUIntMax b = CUIntMax (a .|. b)
  xor (CUIntMax a) (CUIntMax b) = CUIntMax (xor a b)
  complement (CUIntMax a) = CUIntMax (complement a)
  shift (CUIntMax a) i = CUIntMax (shift a i)
  rotate (CUIntMax a) i = CUIntMax (rotate a i)
  zeroBits = CUIntMax zeroBits
  bit i = CUIntMax (bit i)
  setBit (CUIntMax a) i = CUIntMax (setBit a i)
  clearBit (CUIntMax a) i = CUIntMax (clearBit a i)
  testBit (CUIntMax a) i = testBit a i
  bitSizeMaybe (CUIntMax a) = bitSizeMaybe a
  bitSize (CUIntMax a) = bitSize a
  isSigned (CUIntMax a) = isSigned a
  shiftL (CUIntMax a) i = CUIntMax (shiftL a i)
  shiftR (CUIntMax a) i = CUIntMax (shiftR a i)
  rotateL (CUIntMax a) i = CUIntMax (rotateL a i)
  rotateR (CUIntMax a) i = CUIntMax (rotateR a i)
  popCount (CUIntMax a) = popCount a

instance FiniteBits CUIntMax where
  finiteBitSize (CUIntMax a) = finiteBitSize a

newtype CClock = CClock Int64

instance Eq CClock where
  CClock a == CClock b = a == b

instance Ord CClock where
  compare (CClock a) (CClock b) = compare a b

instance Show CClock where
  showsPrec d (CClock a) = showsPrec d a

instance Read CClock where
  readsPrec d text = [(CClock a, rest) | (a, rest) <- readsPrec d text]

instance Num CClock where
  CClock a + CClock b = CClock (a + b)
  CClock a - CClock b = CClock (a - b)
  CClock a * CClock b = CClock (a * b)
  negate (CClock a) = CClock (negate a)
  abs (CClock a) = CClock (abs a)
  signum (CClock a) = CClock (signum a)
  fromInteger n = CClock (fromInteger n)

instance Enum CClock where
  succ (CClock a) = CClock (succ a)
  pred (CClock a) = CClock (pred a)
  toEnum n = CClock (toEnum n)
  fromEnum (CClock a) = fromEnum a
  enumFrom (CClock a) = map CClock (enumFrom a)
  enumFromThen (CClock a) (CClock b) = map CClock (enumFromThen a b)
  enumFromTo (CClock a) (CClock b) = map CClock (enumFromTo a b)
  enumFromThenTo (CClock a) (CClock b) (CClock c) = map CClock (enumFromThenTo a b c)

instance Real CClock where
  toRational (CClock a) = toRational a

instance Storable CClock where
  sizeOf _ = sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek address = CClock <$> peek (castPtr address)
  poke address (CClock a) = poke (castPtr address) a

newtype CTime = CTime Int64

instance Eq CTime where
  CTime a == CTime b = a == b

instance Ord CTime where
  compare (CTime a) (CTime b) = compare a b

instance Show CTime where
  showsPrec d (CTime a) = showsPrec d a

instance Read CTime where
  readsPrec d text = [(CTime a, rest) | (a, rest) <- readsPrec d text]

instance Num CTime where
  CTime a + CTime b = CTime (a + b)
  CTime a - CTime b = CTime (a - b)
  CTime a * CTime b = CTime (a * b)
  negate (CTime a) = CTime (negate a)
  abs (CTime a) = CTime (abs a)
  signum (CTime a) = CTime (signum a)
  fromInteger n = CTime (fromInteger n)

instance Enum CTime where
  succ (CTime a) = CTime (succ a)
  pred (CTime a) = CTime (pred a)
  toEnum n = CTime (toEnum n)
  fromEnum (CTime a) = fromEnum a
  enumFrom (CTime a) = map CTime (enumFrom a)
  enumFromThen (CTime a) (CTime b) = map CTime (enumFromThen a b)
  enumFromTo (CTime a) (CTime b) = map CTime (enumFromTo a b)
  enumFromThenTo (CTime a) (CTime b) (CTime c) = map CTime (enumFromThenTo a b c)

instance Real CTime where
  toRational (CTime a) = toRational a

instance Storable CTime where
  sizeOf _ = sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek address = CTime <$> peek (castPtr address)
  poke address (CTime a) = poke (castPtr address) a

newtype CUSeconds = CUSeconds Word32

instance Eq CUSeconds where
  CUSeconds a == CUSeconds b = a == b

instance Ord CUSeconds where
  compare (CUSeconds a) (CUSeconds b) = compare a b

instance Show CUSeconds where
  showsPrec d (CUSeconds a) = showsPrec d a

instance Read CUSeconds where
  readsPrec d text = [(CUSeconds a, rest) | (a, rest) <- readsPrec d text]

instance Num CUSeconds where
  CUSeconds a + CUSeconds b = CUSeconds (a + b)
  CUSeconds a - CUSeconds b = CUSeconds (a - b)
  CUSeconds a * CUSeconds b = CUSeconds (a * b)
  negate (CUSeconds a) = CUSeconds (negate a)
  abs (CUSeconds a) = CUSeconds (abs a)
  signum (CUSeconds a) = CUSeconds (signum a)
  fromInteger n = CUSeconds (fromInteger n)

instance Enum CUSeconds where
  succ (CUSeconds a) = CUSeconds (succ a)
  pred (CUSeconds a) = CUSeconds (pred a)
  toEnum n = CUSeconds (toEnum n)
  fromEnum (CUSeconds a) = fromEnum a
  enumFrom (CUSeconds a) = map CUSeconds (enumFrom a)
  enumFromThen (CUSeconds a) (CUSeconds b) = map CUSeconds (enumFromThen a b)
  enumFromTo (CUSeconds a) (CUSeconds b) = map CUSeconds (enumFromTo a b)
  enumFromThenTo (CUSeconds a) (CUSeconds b) (CUSeconds c) = map CUSeconds (enumFromThenTo a b c)

instance Real CUSeconds where
  toRational (CUSeconds a) = toRational a

instance Storable CUSeconds where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek address = CUSeconds <$> peek (castPtr address)
  poke address (CUSeconds a) = poke (castPtr address) a

newtype CSUSeconds = CSUSeconds Int64

instance Eq CSUSeconds where
  CSUSeconds a == CSUSeconds b = a == b

instance Ord CSUSeconds where
  compare (CSUSeconds a) (CSUSeconds b) = compare a b

instance Show CSUSeconds where
  showsPrec d (CSUSeconds a) = showsPrec d a

instance Read CSUSeconds where
  readsPrec d text = [(CSUSeconds a, rest) | (a, rest) <- readsPrec d text]

instance Num CSUSeconds where
  CSUSeconds a + CSUSeconds b = CSUSeconds (a + b)
  CSUSeconds a - CSUSeconds b = CSUSeconds (a - b)
  CSUSeconds a * CSUSeconds b = CSUSeconds (a * b)
  negate (CSUSeconds a) = CSUSeconds (negate a)
  abs (CSUSeconds a) = CSUSeconds (abs a)
  signum (CSUSeconds a) = CSUSeconds (signum a)
  fromInteger n = CSUSeconds (fromInteger n)

instance Enum CSUSeconds where
  succ (CSUSeconds a) = CSUSeconds (succ a)
  pred (CSUSeconds a) = CSUSeconds (pred a)
  toEnum n = CSUSeconds (toEnum n)
  fromEnum (CSUSeconds a) = fromEnum a
  enumFrom (CSUSeconds a) = map CSUSeconds (enumFrom a)
  enumFromThen (CSUSeconds a) (CSUSeconds b) = map CSUSeconds (enumFromThen a b)
  enumFromTo (CSUSeconds a) (CSUSeconds b) = map CSUSeconds (enumFromTo a b)
  enumFromThenTo (CSUSeconds a) (CSUSeconds b) (CSUSeconds c) = map CSUSeconds (enumFromThenTo a b c)

instance Real CSUSeconds where
  toRational (CSUSeconds a) = toRational a

instance Storable CSUSeconds where
  sizeOf _ = sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek address = CSUSeconds <$> peek (castPtr address)
  poke address (CSUSeconds a) = poke (castPtr address) a

newtype CFloat = CFloat Float

instance Eq CFloat where
  CFloat a == CFloat b = a == b

instance Ord CFloat where
  compare (CFloat a) (CFloat b) = compare a b

instance Show CFloat where
  showsPrec d (CFloat a) = showsPrec d a

instance Read CFloat where
  readsPrec d text = [(CFloat a, rest) | (a, rest) <- readsPrec d text]

instance Num CFloat where
  CFloat a + CFloat b = CFloat (a + b)
  CFloat a - CFloat b = CFloat (a - b)
  CFloat a * CFloat b = CFloat (a * b)
  negate (CFloat a) = CFloat (negate a)
  abs (CFloat a) = CFloat (abs a)
  signum (CFloat a) = CFloat (signum a)
  fromInteger n = CFloat (fromInteger n)

instance Enum CFloat where
  succ (CFloat a) = CFloat (succ a)
  pred (CFloat a) = CFloat (pred a)
  toEnum n = CFloat (toEnum n)
  fromEnum (CFloat a) = fromEnum a
  enumFrom (CFloat a) = map CFloat (enumFrom a)
  enumFromThen (CFloat a) (CFloat b) = map CFloat (enumFromThen a b)
  enumFromTo (CFloat a) (CFloat b) = map CFloat (enumFromTo a b)
  enumFromThenTo (CFloat a) (CFloat b) (CFloat c) = map CFloat (enumFromThenTo a b c)

instance Real CFloat where
  toRational (CFloat a) = toRational a

instance Storable CFloat where
  sizeOf _ = sizeOf (undefined :: Float)
  alignment _ = alignment (undefined :: Float)
  peek address = CFloat <$> peek (castPtr address)
  poke address (CFloat a) = poke (castPtr address) a

instance Fractional CFloat where
  CFloat a / CFloat b = CFloat (a / b)
  recip (CFloat a) = CFloat (recip a)
  fromRational r = CFloat (fromRational r)

instance Floating CFloat where
  pi = CFloat pi
  exp (CFloat a) = CFloat (exp a)
  log (CFloat a) = CFloat (log a)
  sqrt (CFloat a) = CFloat (sqrt a)
  sin (CFloat a) = CFloat (sin a)
  cos (CFloat a) = CFloat (cos a)
  tan (CFloat a) = CFloat (tan a)
  asin (CFloat a) = CFloat (asin a)
  acos (CFloat a) = CFloat (acos a)
  atan (CFloat a) = CFloat (atan a)
  sinh (CFloat a) = CFloat (sinh a)
  cosh (CFloat a) = CFloat (cosh a)
  tanh (CFloat a) = CFloat (tanh a)
  asinh (CFloat a) = CFloat (asinh a)
  acosh (CFloat a) = CFloat (acosh a)
  atanh (CFloat a) = CFloat (atanh a)
  CFloat a ** CFloat b = CFloat (a ** b)
  logBase (CFloat a) (CFloat b) = CFloat (logBase a b)

instance RealFrac CFloat where
  properFraction (CFloat a) = case properFraction a of
    (n, f) -> (n, CFloat f)
  truncate (CFloat a) = truncate a
  round (CFloat a) = round a
  ceiling (CFloat a) = ceiling a
  floor (CFloat a) = floor a

instance RealFloat CFloat where
  floatRadix (CFloat a) = floatRadix a
  floatDigits (CFloat a) = floatDigits a
  floatRange (CFloat a) = floatRange a
  decodeFloat (CFloat a) = decodeFloat a
  encodeFloat m e = CFloat (encodeFloat m e)
  exponent (CFloat a) = exponent a
  significand (CFloat a) = CFloat (significand a)
  scaleFloat k (CFloat a) = CFloat (scaleFloat k a)
  isNaN (CFloat a) = isNaN a
  isInfinite (CFloat a) = isInfinite a
  isDenormalized (CFloat a) = isDenormalized a
  isNegativeZero (CFloat a) = isNegativeZero a
  isIEEE (CFloat a) = isIEEE a
  atan2 (CFloat a) (CFloat b) = CFloat (atan2 a b)

newtype CDouble = CDouble Double

instance Eq CDouble where
  CDouble a == CDouble b = a == b

instance Ord CDouble where
  compare (CDouble a) (CDouble b) = compare a b

instance Show CDouble where
  showsPrec d (CDouble a) = showsPrec d a

instance Read CDouble where
  readsPrec d text = [(CDouble a, rest) | (a, rest) <- readsPrec d text]

instance Num CDouble where
  CDouble a + CDouble b = CDouble (a + b)
  CDouble a - CDouble b = CDouble (a - b)
  CDouble a * CDouble b = CDouble (a * b)
  negate (CDouble a) = CDouble (negate a)
  abs (CDouble a) = CDouble (abs a)
  signum (CDouble a) = CDouble (signum a)
  fromInteger n = CDouble (fromInteger n)

instance Enum CDouble where
  succ (CDouble a) = CDouble (succ a)
  pred (CDouble a) = CDouble (pred a)
  toEnum n = CDouble (toEnum n)
  fromEnum (CDouble a) = fromEnum a
  enumFrom (CDouble a) = map CDouble (enumFrom a)
  enumFromThen (CDouble a) (CDouble b) = map CDouble (enumFromThen a b)
  enumFromTo (CDouble a) (CDouble b) = map CDouble (enumFromTo a b)
  enumFromThenTo (CDouble a) (CDouble b) (CDouble c) = map CDouble (enumFromThenTo a b c)

instance Real CDouble where
  toRational (CDouble a) = toRational a

instance Storable CDouble where
  sizeOf _ = sizeOf (undefined :: Double)
  alignment _ = alignment (undefined :: Double)
  peek address = CDouble <$> peek (castPtr address)
  poke address (CDouble a) = poke (castPtr address) a

instance Fractional CDouble where
  CDouble a / CDouble b = CDouble (a / b)
  recip (CDouble a) = CDouble (recip a)
  fromRational r = CDouble (fromRational r)

instance Floating CDouble where
  pi = CDouble pi
  exp (CDouble a) = CDouble (exp a)
  log (CDouble a) = CDouble (log a)
  sqrt (CDouble a) = CDouble (sqrt a)
  sin (CDouble a) = CDouble (sin a)
  cos (CDouble a) = CDouble (cos a)
  tan (CDouble a) = CDouble (tan a)
  asin (CDouble a) = CDouble (asin a)
  acos (CDouble a) = CDouble (acos a)
  atan (CDouble a) = CDouble (atan a)
  sinh (CDouble a) = CDouble (sinh a)
  cosh (CDouble a) = CDouble (cosh a)
  tanh (CDouble a) = CDouble (tanh a)
  asinh (CDouble a) = CDouble (asinh a)
  acosh (CDouble a) = CDouble (acosh a)
  atanh (CDouble a) = CDouble (atanh a)
  CDouble a ** CDouble b = CDouble (a ** b)
  logBase (CDouble a) (CDouble b) = CDouble (logBase a b)

instance RealFrac CDouble where
  properFraction (CDouble a) = case properFraction a of
    (n, f) -> (n, CDouble f)
  truncate (CDouble a) = truncate a
  round (CDouble a) = round a
  ceiling (CDouble a) = ceiling a
  floor (CDouble a) = floor a

instance RealFloat CDouble where
  floatRadix (CDouble a) = floatRadix a
  floatDigits (CDouble a) = floatDigits a
  floatRange (CDouble a) = floatRange a
  decodeFloat (CDouble a) = decodeFloat a
  encodeFloat m e = CDouble (encodeFloat m e)
  exponent (CDouble a) = exponent a
  significand (CDouble a) = CDouble (significand a)
  scaleFloat k (CDouble a) = CDouble (scaleFloat k a)
  isNaN (CDouble a) = isNaN a
  isInfinite (CDouble a) = isInfinite a
  isDenormalized (CDouble a) = isDenormalized a
  isNegativeZero (CDouble a) = isNegativeZero a
  isIEEE (CDouble a) = isIEEE a
  atan2 (CDouble a) (CDouble b) = CDouble (atan2 a b)
