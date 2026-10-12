module Data.Int
  ( Int,
    Int8,
    Int16,
    Int32,
    Int64,
  )
where

-- An integer brought into the range of so many bits, as one that overflowed
-- would be.
wrap :: Int -> Int -> Int
wrap bits x
  | bits >= 32 = x
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
  minBound = Int32 minBound
  maxBound = Int32 maxBound

newtype Int64 = Int64 Int
  deriving (Eq, Ord)

instance Show Int64 where
  showsPrec d (Int64 x) = showsPrec d x

instance Read Int64 where
  readsPrec d text = [(fromInteger x, rest) | (x, rest) <- readsPrec d text]

instance Num Int64 where
  Int64 a + Int64 b = Int64 (wrap 64 (a + b))
  Int64 a - Int64 b = Int64 (wrap 64 (a - b))
  Int64 a * Int64 b = Int64 (wrap 64 (a * b))
  negate (Int64 a) = Int64 (wrap 64 (negate a))
  abs (Int64 a) = Int64 (wrap 64 (abs a))
  signum (Int64 a) = Int64 (signum a)
  fromInteger x = Int64 (wrap 64 (fromInteger x))

instance Enum Int64 where
  toEnum x = Int64 (wrap 64 x)
  fromEnum (Int64 x) = x
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo (Int64 a) (Int64 b) = map Int64 (enumFromTo a b)
  enumFromThenTo (Int64 a) (Int64 b) (Int64 c) = map Int64 (enumFromThenTo a b c)

instance Real Int64 where
  toRational (Int64 x) = toRational x

instance Integral Int64 where
  quotRem (Int64 a) (Int64 b) = case quotRem a b of
    (q, r) -> (Int64 (wrap 64 q), Int64 r)
  divMod (Int64 a) (Int64 b) = case divMod a b of
    (q, r) -> (Int64 (wrap 64 q), Int64 r)
  toInteger (Int64 x) = toInteger x

instance Bounded Int64 where
  minBound = Int64 minBound
  maxBound = Int64 maxBound
