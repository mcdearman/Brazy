-- |
-- Numbers as their bits, as GHC has them. An 'Integer' is as many bits as
-- it needs, as if a negative one went on with ones for ever; the runtime
-- works them out. The others are worked out on an 'Integer' and made their
-- own width again.
module Data.Bits
  ( Bits (..),
    FiniteBits (..),
    bitDefault,
    testBitDefault,
    popCountDefault,
    toIntegralSized,
    oneBits,
    (.^.),
    (.>>.),
    (.<<.),
    (!>>.),
    (!<<.),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Hazy (primIntegerBits, primIntegerPopCount, primIntegerShift)

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`, `unsafeShiftL`, `unsafeShiftR`

infixl 7 .&.

infixl 6 `xor`

infixl 5 .|.

class (Eq a) => Bits a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  xor :: a -> a -> a
  complement :: a -> a

  shift :: a -> Int -> a
  x `shift` i
    | i < 0 = x `shiftR` negate i
    | i > 0 = x `shiftL` i
    | otherwise = x

  rotate :: a -> Int -> a
  x `rotate` i
    | i < 0 = x `rotateR` negate i
    | i > 0 = x `rotateL` i
    | otherwise = x

  zeroBits :: a
  zeroBits = clearBit (bit 0) 0

  bit :: Int -> a

  setBit :: a -> Int -> a
  x `setBit` i = x .|. bit i

  clearBit :: a -> Int -> a
  x `clearBit` i = x .&. complement (bit i)

  complementBit :: a -> Int -> a
  x `complementBit` i = x `xor` bit i

  testBit :: a -> Int -> Bool

  bitSizeMaybe :: a -> Maybe Int

  bitSize :: a -> Int
  bitSize x = case bitSizeMaybe x of
    Just size -> size
    Nothing -> error "Data.Bits.bitSize: the number has no size"

  isSigned :: a -> Bool

  shiftL :: a -> Int -> a
  x `shiftL` i = x `shift` i

  unsafeShiftL :: a -> Int -> a
  unsafeShiftL = shiftL

  shiftR :: a -> Int -> a
  x `shiftR` i = x `shift` negate i

  unsafeShiftR :: a -> Int -> a
  unsafeShiftR = shiftR

  rotateL :: a -> Int -> a
  x `rotateL` i = x `rotate` i

  rotateR :: a -> Int -> a
  x `rotateR` i = x `rotate` negate i

  popCount :: a -> Int

class (Bits b) => FiniteBits b where
  finiteBitSize :: b -> Int

  countLeadingZeros :: b -> Int
  countLeadingZeros x = (width - 1) - go (width - 1)
    where
      width = finiteBitSize x
      go i
        | i < 0 = i
        | testBit x i = i
        | otherwise = go (i - 1)

  countTrailingZeros :: b -> Int
  countTrailingZeros x = go 0
    where
      width = finiteBitSize x
      go i
        | i >= width = i
        | testBit x i = i
        | otherwise = go (i + 1)

bitDefault :: (Bits a, Num a) => Int -> a
bitDefault i = 1 `shiftL` i

testBitDefault :: (Bits a, Num a) => a -> Int -> Bool
testBitDefault x i = (x .&. bit i) /= 0

popCountDefault :: (Bits a, Num a) => a -> Int
popCountDefault = go 0
  where
    go count 0 = count
    go count w = go (count + 1) (w .&. (w - 1))

toIntegralSized :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
toIntegralSized x
  | toInteger y == toInteger x = Just y
  | otherwise = Nothing
  where
    y = fromIntegral x

oneBits :: (Bits a) => a
oneBits = complement zeroBits

infixl 6 .^.

(.^.) :: (Bits a) => a -> a -> a
(.^.) = xor

infixl 8 .>>., .<<., !>>., !<<.

(.>>.), (.<<.), (!>>.), (!<<.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR
(.<<.) = shiftL
(!>>.) = unsafeShiftR
(!<<.) = unsafeShiftL

instance Bits Integer where
  x .&. y = primIntegerBits 0 x y
  x .|. y = primIntegerBits 1 x y
  xor x y = primIntegerBits 2 x y
  complement x = negate x - 1
  shift x i = primIntegerShift x i
  shiftL x i = primIntegerShift x i
  shiftR x i = primIntegerShift x (negate i)
  rotate = shift
  zeroBits = 0
  bit i = primIntegerShift 1 i
  testBit x i = primIntegerBits 0 (primIntegerShift x (negate i)) 1 /= 0
  bitSizeMaybe _ = Nothing
  isSigned _ = True
  popCount = primIntegerPopCount

instance Bits Bool where
  (.&.) = (&&)
  (.|.) = (||)
  xor = (/=)
  complement = not
  shift x 0 = x
  shift _ _ = False
  rotate x _ = x
  zeroBits = False
  bit 0 = True
  bit _ = False
  testBit x 0 = x
  testBit _ _ = False
  bitSizeMaybe _ = Just 1
  bitSize _ = 1
  isSigned _ = False
  popCount False = 0
  popCount True = 1

instance FiniteBits Bool where
  finiteBitSize _ = 1

-- What is of a width is worked out on an Integer, and then brought back
-- to the width: as the two's complement of its sign if it has one.
sized :: (Integral a) => Bool -> Int -> Integer -> a
sized signed width n
  | signed && testBit kept (width - 1) = fromInteger (kept - bit width)
  | otherwise = fromInteger kept
  where
    kept = n .&. (bit width - 1)

unsigned :: (Integral a) => Int -> a -> Integer
unsigned width x = toInteger x .&. (bit width - 1)

rotated :: (Integral a) => Bool -> Int -> a -> Int -> a
rotated signed width x i = sized signed width ((u `shiftL` n) .|. (u `shiftR` (width - n)))
  where
    n = i `mod` width
    u = unsigned width x

-- An Int is 63 bits.
instance Bits Int where
  x .&. y = sized True 63 (toInteger x .&. toInteger y)
  x .|. y = sized True 63 (toInteger x .|. toInteger y)
  xor x y = sized True 63 (toInteger x `xor` toInteger y)
  complement x = negate x - 1
  shift x i = sized True 63 (toInteger x `shift` i)
  rotate = rotated True 63
  zeroBits = 0
  bit i = sized True 63 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 63
  bitSize _ = 63
  isSigned _ = True
  popCount x = popCount (unsigned 63 x)

instance FiniteBits Int where
  finiteBitSize _ = 63

instance Bits Int8 where
  x .&. y = sized True 8 (toInteger x .&. toInteger y)
  x .|. y = sized True 8 (toInteger x .|. toInteger y)
  xor x y = sized True 8 (toInteger x `xor` toInteger y)
  complement x = sized True 8 (complement (toInteger x))
  shift x i = sized True 8 (toInteger x `shift` i)
  rotate = rotated True 8
  zeroBits = 0
  bit i = sized True 8 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 8
  bitSize _ = 8
  isSigned _ = True
  popCount x = popCount (unsigned 8 x)

instance FiniteBits Int8 where
  finiteBitSize _ = 8

instance Bits Int16 where
  x .&. y = sized True 16 (toInteger x .&. toInteger y)
  x .|. y = sized True 16 (toInteger x .|. toInteger y)
  xor x y = sized True 16 (toInteger x `xor` toInteger y)
  complement x = sized True 16 (complement (toInteger x))
  shift x i = sized True 16 (toInteger x `shift` i)
  rotate = rotated True 16
  zeroBits = 0
  bit i = sized True 16 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 16
  bitSize _ = 16
  isSigned _ = True
  popCount x = popCount (unsigned 16 x)

instance FiniteBits Int16 where
  finiteBitSize _ = 16

instance Bits Int32 where
  x .&. y = sized True 32 (toInteger x .&. toInteger y)
  x .|. y = sized True 32 (toInteger x .|. toInteger y)
  xor x y = sized True 32 (toInteger x `xor` toInteger y)
  complement x = sized True 32 (complement (toInteger x))
  shift x i = sized True 32 (toInteger x `shift` i)
  rotate = rotated True 32
  zeroBits = 0
  bit i = sized True 32 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 32
  bitSize _ = 32
  isSigned _ = True
  popCount x = popCount (unsigned 32 x)

instance FiniteBits Int32 where
  finiteBitSize _ = 32

instance Bits Int64 where
  x .&. y = sized True 64 (toInteger x .&. toInteger y)
  x .|. y = sized True 64 (toInteger x .|. toInteger y)
  xor x y = sized True 64 (toInteger x `xor` toInteger y)
  complement x = sized True 64 (complement (toInteger x))
  shift x i = sized True 64 (toInteger x `shift` i)
  rotate = rotated True 64
  zeroBits = 0
  bit i = sized True 64 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  isSigned _ = True
  popCount x = popCount (unsigned 64 x)

instance FiniteBits Int64 where
  finiteBitSize _ = 64

instance Bits Word where
  x .&. y = sized False 64 (toInteger x .&. toInteger y)
  x .|. y = sized False 64 (toInteger x .|. toInteger y)
  xor x y = sized False 64 (toInteger x `xor` toInteger y)
  complement x = sized False 64 (complement (toInteger x))
  shift x i = sized False 64 (toInteger x `shift` i)
  rotate = rotated False 64
  zeroBits = 0
  bit i = sized False 64 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  isSigned _ = False
  popCount x = popCount (toInteger x)

instance FiniteBits Word where
  finiteBitSize _ = 64

instance Bits Word8 where
  x .&. y = sized False 8 (toInteger x .&. toInteger y)
  x .|. y = sized False 8 (toInteger x .|. toInteger y)
  xor x y = sized False 8 (toInteger x `xor` toInteger y)
  complement x = sized False 8 (complement (toInteger x))
  shift x i = sized False 8 (toInteger x `shift` i)
  rotate = rotated False 8
  zeroBits = 0
  bit i = sized False 8 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 8
  bitSize _ = 8
  isSigned _ = False
  popCount x = popCount (toInteger x)

instance FiniteBits Word8 where
  finiteBitSize _ = 8

instance Bits Word16 where
  x .&. y = sized False 16 (toInteger x .&. toInteger y)
  x .|. y = sized False 16 (toInteger x .|. toInteger y)
  xor x y = sized False 16 (toInteger x `xor` toInteger y)
  complement x = sized False 16 (complement (toInteger x))
  shift x i = sized False 16 (toInteger x `shift` i)
  rotate = rotated False 16
  zeroBits = 0
  bit i = sized False 16 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 16
  bitSize _ = 16
  isSigned _ = False
  popCount x = popCount (toInteger x)

instance FiniteBits Word16 where
  finiteBitSize _ = 16

instance Bits Word32 where
  x .&. y = sized False 32 (toInteger x .&. toInteger y)
  x .|. y = sized False 32 (toInteger x .|. toInteger y)
  xor x y = sized False 32 (toInteger x `xor` toInteger y)
  complement x = sized False 32 (complement (toInteger x))
  shift x i = sized False 32 (toInteger x `shift` i)
  rotate = rotated False 32
  zeroBits = 0
  bit i = sized False 32 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 32
  bitSize _ = 32
  isSigned _ = False
  popCount x = popCount (toInteger x)

instance FiniteBits Word32 where
  finiteBitSize _ = 32

instance Bits Word64 where
  x .&. y = sized False 64 (toInteger x .&. toInteger y)
  x .|. y = sized False 64 (toInteger x .|. toInteger y)
  xor x y = sized False 64 (toInteger x `xor` toInteger y)
  complement x = sized False 64 (complement (toInteger x))
  shift x i = sized False 64 (toInteger x `shift` i)
  rotate = rotated False 64
  zeroBits = 0
  bit i = sized False 64 (bit i)
  testBit x i = testBit (toInteger x) i
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  isSigned _ = False
  popCount x = popCount (toInteger x)

instance FiniteBits Word64 where
  finiteBitSize _ = 64
