module Data.Word
  ( Word,
    Word8,
    Word16,
    Word32,
    Word64,
  )
where

newtype Word8 = Word8 Integer
  deriving (Eq, Ord)

instance Show Word8 where
  showsPrec d (Word8 x) = showsPrec d x

instance Read Word8 where
  readsPrec d text = [(fromInteger x, rest) | (x, rest) <- readsPrec d text]

instance Num Word8 where
  Word8 a + Word8 b = fromInteger (a + b)
  Word8 a - Word8 b = fromInteger (a - b)
  Word8 a * Word8 b = fromInteger (a * b)
  negate (Word8 a) = fromInteger (negate a)
  abs word = word
  signum (Word8 a) = Word8 (signum a)
  fromInteger x = Word8 (x `mod` 2 ^ (8 :: Int))

instance Enum Word8 where
  toEnum x = fromInteger (toInteger x)
  fromEnum (Word8 x) = fromInteger x
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo (Word8 a) (Word8 b) = map Word8 (enumFromTo a b)
  enumFromThenTo (Word8 a) (Word8 b) (Word8 c) = map Word8 (enumFromThenTo a b c)

instance Real Word8 where
  toRational (Word8 x) = toRational x

instance Integral Word8 where
  quotRem (Word8 a) (Word8 b) = case quotRem a b of
    (q, r) -> (Word8 q, Word8 r)
  divMod (Word8 a) (Word8 b) = case divMod a b of
    (q, r) -> (Word8 q, Word8 r)
  toInteger (Word8 x) = x

instance Bounded Word8 where
  minBound = Word8 0
  maxBound = Word8 (2 ^ (8 :: Int) - 1)

newtype Word16 = Word16 Integer
  deriving (Eq, Ord)

instance Show Word16 where
  showsPrec d (Word16 x) = showsPrec d x

instance Read Word16 where
  readsPrec d text = [(fromInteger x, rest) | (x, rest) <- readsPrec d text]

instance Num Word16 where
  Word16 a + Word16 b = fromInteger (a + b)
  Word16 a - Word16 b = fromInteger (a - b)
  Word16 a * Word16 b = fromInteger (a * b)
  negate (Word16 a) = fromInteger (negate a)
  abs word = word
  signum (Word16 a) = Word16 (signum a)
  fromInteger x = Word16 (x `mod` 2 ^ (16 :: Int))

instance Enum Word16 where
  toEnum x = fromInteger (toInteger x)
  fromEnum (Word16 x) = fromInteger x
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo (Word16 a) (Word16 b) = map Word16 (enumFromTo a b)
  enumFromThenTo (Word16 a) (Word16 b) (Word16 c) = map Word16 (enumFromThenTo a b c)

instance Real Word16 where
  toRational (Word16 x) = toRational x

instance Integral Word16 where
  quotRem (Word16 a) (Word16 b) = case quotRem a b of
    (q, r) -> (Word16 q, Word16 r)
  divMod (Word16 a) (Word16 b) = case divMod a b of
    (q, r) -> (Word16 q, Word16 r)
  toInteger (Word16 x) = x

instance Bounded Word16 where
  minBound = Word16 0
  maxBound = Word16 (2 ^ (16 :: Int) - 1)

newtype Word32 = Word32 Integer
  deriving (Eq, Ord)

instance Show Word32 where
  showsPrec d (Word32 x) = showsPrec d x

instance Read Word32 where
  readsPrec d text = [(fromInteger x, rest) | (x, rest) <- readsPrec d text]

instance Num Word32 where
  Word32 a + Word32 b = fromInteger (a + b)
  Word32 a - Word32 b = fromInteger (a - b)
  Word32 a * Word32 b = fromInteger (a * b)
  negate (Word32 a) = fromInteger (negate a)
  abs word = word
  signum (Word32 a) = Word32 (signum a)
  fromInteger x = Word32 (x `mod` 2 ^ (32 :: Int))

instance Enum Word32 where
  toEnum x = fromInteger (toInteger x)
  fromEnum (Word32 x) = fromInteger x
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo (Word32 a) (Word32 b) = map Word32 (enumFromTo a b)
  enumFromThenTo (Word32 a) (Word32 b) (Word32 c) = map Word32 (enumFromThenTo a b c)

instance Real Word32 where
  toRational (Word32 x) = toRational x

instance Integral Word32 where
  quotRem (Word32 a) (Word32 b) = case quotRem a b of
    (q, r) -> (Word32 q, Word32 r)
  divMod (Word32 a) (Word32 b) = case divMod a b of
    (q, r) -> (Word32 q, Word32 r)
  toInteger (Word32 x) = x

instance Bounded Word32 where
  minBound = Word32 0
  maxBound = Word32 (2 ^ (32 :: Int) - 1)

newtype Word64 = Word64 Integer
  deriving (Eq, Ord)

instance Show Word64 where
  showsPrec d (Word64 x) = showsPrec d x

instance Read Word64 where
  readsPrec d text = [(fromInteger x, rest) | (x, rest) <- readsPrec d text]

instance Num Word64 where
  Word64 a + Word64 b = fromInteger (a + b)
  Word64 a - Word64 b = fromInteger (a - b)
  Word64 a * Word64 b = fromInteger (a * b)
  negate (Word64 a) = fromInteger (negate a)
  abs word = word
  signum (Word64 a) = Word64 (signum a)
  fromInteger x = Word64 (x `mod` 2 ^ (64 :: Int))

instance Enum Word64 where
  toEnum x = fromInteger (toInteger x)
  fromEnum (Word64 x) = fromInteger x
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo (Word64 a) (Word64 b) = map Word64 (enumFromTo a b)
  enumFromThenTo (Word64 a) (Word64 b) (Word64 c) = map Word64 (enumFromThenTo a b c)

instance Real Word64 where
  toRational (Word64 x) = toRational x

instance Integral Word64 where
  quotRem (Word64 a) (Word64 b) = case quotRem a b of
    (q, r) -> (Word64 q, Word64 r)
  divMod (Word64 a) (Word64 b) = case divMod a b of
    (q, r) -> (Word64 q, Word64 r)
  toInteger (Word64 x) = x

instance Bounded Word64 where
  minBound = Word64 0
  maxBound = Word64 (2 ^ (64 :: Int) - 1)
