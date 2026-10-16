module Data.Int
  ( Int,
    Int8,
    Int16,
    Int32,
    Int64,
  )
where

import Hazy.Prelude (Ix (..))

-- An integer brought into the range of so many bits, as one that overflowed
-- would be.
wrap :: Int -> Int -> Int
wrap bits x
  | bits >= 63 = x
  | reduced >= half = reduced - 2 * half
  | otherwise = reduced
  where
    half = 2 ^ (bits - 1)
    reduced = x `mod` (2 * half)

newtype Int8 = Int8 Int
  deriving (Eq, Ord)

instance Show Int8 where
  showsPrec d (Int8 x) = showsPrec d x

instance Read Int8 where
  readsPrec d text = [(fromInteger x, rest) | (x, rest) <- readsPrec d text]

instance Num Int8 where
  Int8 a + Int8 b = Int8 (wrap 8 (a + b))
  Int8 a - Int8 b = Int8 (wrap 8 (a - b))
  Int8 a * Int8 b = Int8 (wrap 8 (a * b))
  negate (Int8 a) = Int8 (wrap 8 (negate a))
  abs (Int8 a) = Int8 (wrap 8 (abs a))
  signum (Int8 a) = Int8 (signum a)
  fromInteger x = Int8 (wrap 8 (fromInteger x))

instance Enum Int8 where
  toEnum x = Int8 (wrap 8 x)
  fromEnum (Int8 x) = x
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo (Int8 a) (Int8 b) = map Int8 (enumFromTo a b)
  enumFromThenTo (Int8 a) (Int8 b) (Int8 c) = map Int8 (enumFromThenTo a b c)

instance Real Int8 where
  toRational (Int8 x) = toRational x

instance Integral Int8 where
  quotRem (Int8 a) (Int8 b) = case quotRem a b of
    (q, r) -> (Int8 (wrap 8 q), Int8 r)
  divMod (Int8 a) (Int8 b) = case divMod a b of
    (q, r) -> (Int8 (wrap 8 q), Int8 r)
  toInteger (Int8 x) = toInteger x

instance Bounded Int8 where
  minBound = Int8 (negate (2 ^ (8 - 1 :: Int)))
  maxBound = Int8 (2 ^ (8 - 1 :: Int) - 1)

newtype Int16 = Int16 Int
  deriving (Eq, Ord)

instance Show Int16 where
  showsPrec d (Int16 x) = showsPrec d x

instance Read Int16 where
  readsPrec d text = [(fromInteger x, rest) | (x, rest) <- readsPrec d text]

instance Num Int16 where
  Int16 a + Int16 b = Int16 (wrap 16 (a + b))
  Int16 a - Int16 b = Int16 (wrap 16 (a - b))
  Int16 a * Int16 b = Int16 (wrap 16 (a * b))
  negate (Int16 a) = Int16 (wrap 16 (negate a))
  abs (Int16 a) = Int16 (wrap 16 (abs a))
  signum (Int16 a) = Int16 (signum a)
  fromInteger x = Int16 (wrap 16 (fromInteger x))

instance Enum Int16 where
  toEnum x = Int16 (wrap 16 x)
  fromEnum (Int16 x) = x
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo (Int16 a) (Int16 b) = map Int16 (enumFromTo a b)
  enumFromThenTo (Int16 a) (Int16 b) (Int16 c) = map Int16 (enumFromThenTo a b c)

instance Real Int16 where
  toRational (Int16 x) = toRational x

instance Integral Int16 where
  quotRem (Int16 a) (Int16 b) = case quotRem a b of
    (q, r) -> (Int16 (wrap 16 q), Int16 r)
  divMod (Int16 a) (Int16 b) = case divMod a b of
    (q, r) -> (Int16 (wrap 16 q), Int16 r)
  toInteger (Int16 x) = toInteger x

instance Bounded Int16 where
  minBound = Int16 (negate (2 ^ (16 - 1 :: Int)))
  maxBound = Int16 (2 ^ (16 - 1 :: Int) - 1)

newtype Int32 = Int32 Int
  deriving (Eq, Ord)

instance Show Int32 where
  showsPrec d (Int32 x) = showsPrec d x

instance Read Int32 where
  readsPrec d text = [(fromInteger x, rest) | (x, rest) <- readsPrec d text]

instance Num Int32 where
  Int32 a + Int32 b = Int32 (wrap 32 (a + b))
  Int32 a - Int32 b = Int32 (wrap 32 (a - b))
  Int32 a * Int32 b = Int32 (wrap 32 (a * b))
  negate (Int32 a) = Int32 (wrap 32 (negate a))
  abs (Int32 a) = Int32 (wrap 32 (abs a))
  signum (Int32 a) = Int32 (signum a)
  fromInteger x = Int32 (wrap 32 (fromInteger x))

instance Enum Int32 where
  toEnum x = Int32 (wrap 32 x)
  fromEnum (Int32 x) = x
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo (Int32 a) (Int32 b) = map Int32 (enumFromTo a b)
  enumFromThenTo (Int32 a) (Int32 b) (Int32 c) = map Int32 (enumFromThenTo a b c)

instance Real Int32 where
  toRational (Int32 x) = toRational x

instance Integral Int32 where
  quotRem (Int32 a) (Int32 b) = case quotRem a b of
    (q, r) -> (Int32 (wrap 32 q), Int32 r)
  divMod (Int32 a) (Int32 b) = case divMod a b of
    (q, r) -> (Int32 (wrap 32 q), Int32 r)
  toInteger (Int32 x) = toInteger x

instance Bounded Int32 where
  minBound = Int32 (negate (2 ^ (32 - 1 :: Int)))
  maxBound = Int32 (2 ^ (32 - 1 :: Int) - 1)

-- Wider than an Int, which is 63 bits, so kept as an Integer.
newtype Int64 = Int64 Integer
  deriving (Eq, Ord)

-- An Integer brought into the range of 64 bits.
wrap64 :: Integer -> Integer
wrap64 x
  | reduced >= half = reduced - 2 * half
  | otherwise = reduced
  where
    half = 2 ^ (63 :: Int)
    reduced = x `mod` (2 * half)

instance Show Int64 where
  showsPrec d (Int64 x) = showsPrec d x

instance Read Int64 where
  readsPrec d text = [(fromInteger x, rest) | (x, rest) <- readsPrec d text]

instance Num Int64 where
  Int64 a + Int64 b = Int64 (wrap64 (a + b))
  Int64 a - Int64 b = Int64 (wrap64 (a - b))
  Int64 a * Int64 b = Int64 (wrap64 (a * b))
  negate (Int64 a) = Int64 (wrap64 (negate a))
  abs (Int64 a) = Int64 (wrap64 (abs a))
  signum (Int64 a) = Int64 (signum a)
  fromInteger x = Int64 (wrap64 x)

instance Enum Int64 where
  toEnum x = Int64 (toInteger x)
  fromEnum (Int64 x) = fromInteger x
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo (Int64 a) (Int64 b) = map Int64 (enumFromTo a b)
  enumFromThenTo (Int64 a) (Int64 b) (Int64 c) = map Int64 (enumFromThenTo a b c)

instance Real Int64 where
  toRational (Int64 x) = toRational x

instance Integral Int64 where
  quotRem (Int64 a) (Int64 b) = case quotRem a b of
    (q, r) -> (Int64 (wrap64 q), Int64 r)
  divMod (Int64 a) (Int64 b) = case divMod a b of
    (q, r) -> (Int64 (wrap64 q), Int64 r)
  toInteger (Int64 x) = x

instance Bounded Int64 where
  minBound = Int64 (negate (2 ^ (63 :: Int)))
  maxBound = Int64 (2 ^ (63 :: Int) - 1)

instance Ix Int8 where
  range (low, high) = [low .. high]
  index bounds@(low, high) i
    | low <= i && i <= high = fromInteger (toInteger i - toInteger low)
    | otherwise = errorWithoutStackTrace ("Ix{Int8}.index: Index " ++ showsPrec 11 i "" ++ " out of range (" ++ showsPrec 11 bounds "" ++ ")")
  inRange (low, high) i = low <= i && i <= high

instance Ix Int16 where
  range (low, high) = [low .. high]
  index bounds@(low, high) i
    | low <= i && i <= high = fromInteger (toInteger i - toInteger low)
    | otherwise = errorWithoutStackTrace ("Ix{Int16}.index: Index " ++ showsPrec 11 i "" ++ " out of range (" ++ showsPrec 11 bounds "" ++ ")")
  inRange (low, high) i = low <= i && i <= high

instance Ix Int32 where
  range (low, high) = [low .. high]
  index bounds@(low, high) i
    | low <= i && i <= high = fromInteger (toInteger i - toInteger low)
    | otherwise = errorWithoutStackTrace ("Ix{Int32}.index: Index " ++ showsPrec 11 i "" ++ " out of range (" ++ showsPrec 11 bounds "" ++ ")")
  inRange (low, high) i = low <= i && i <= high

instance Ix Int64 where
  range (low, high) = [low .. high]
  index bounds@(low, high) i
    | low <= i && i <= high = fromInteger (toInteger i - toInteger low)
    | otherwise = errorWithoutStackTrace ("Ix{Int64}.index: Index " ++ showsPrec 11 i "" ++ " out of range (" ++ showsPrec 11 bounds "" ++ ")")
  inRange (low, high) i = low <= i && i <= high
