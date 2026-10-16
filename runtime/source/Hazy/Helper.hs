-- |
-- This module contains helpers that the runtime call into. This is mainly
-- for type class defaults and instances that are necessarily part of the
-- runtime by transative closure.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Helper where

import Hazy
import Hazy.Prelude

minilex :: ReadS Char
minilex (c : s) = case c of
  ' ' -> minilex s
  '\t' -> minilex s
  '\n' -> minilex s
  '\r' -> minilex s
  '\f' -> minilex s
  '\v' -> minilex s
  _ -> [(c, s)]
minilex "" = []

minilexSingle :: Char -> ReadS ()
minilexSingle c' s = [((), t) | (c, t) <- minilex s, c == c']

bindRead :: [(a, String)] -> (a -> String -> [(b, String)]) -> [(b, String)]
bindRead as f = concatMap (\(a, s) -> f a s) as

data Strict a = Strict !a

enumEqual :: (Enum a) => a -> a -> Bool
enumEqual l r = fromEnum l == fromEnum r

enumCompare :: (Enum a) => a -> a -> Ordering
enumCompare l r = compare (fromEnum l) (fromEnum r)

reduce :: (Integral a) => a -> a -> HelperRatio a
reduce _ 0 = error "Data.Ratio.% : zero denominator"
reduce x y = Ratio $ (x `quot` d) :% (y `quot` d)
  where
    d = gcd x y

defaultNotEqual :: (Eq a) => a -> a -> Bool
defaultNotEqual x y = not (x == y)

defaultLessThen,
  defaultLessThenEqual,
  defaultGreaterThen,
  defaultGreaterThenEqual ::
    (Ord a) => a -> a -> Bool
defaultLessThen x y = compare x y == LT
defaultLessThenEqual x y = compare x y /= GT
defaultGreaterThen x y = compare x y == GT
defaultGreaterThenEqual x y = compare x y /= LT

defaultMax, defaultMin :: (Ord a) => a -> a -> a
defaultMax x y
  | x <= y = y
  | otherwise = x
defaultMin x y
  | x <= y = x
  | otherwise = y

defaultSconcat :: (Semigroup a) => NonEmpty a -> a
defaultSconcat = \case
  x :| [] -> x
  x :| x' : xs -> x <> sconcat (x' :| xs)

defaultStimes :: (Semigroup a, Integral b) => b -> a -> a
defaultStimes n a | n >= 1 = go n a
  where
    go 1 a = a
    go n a = a <> go (n - 1) a

defaultMappend :: (Monoid a) => a -> a -> a
defaultMappend = (<>)

defaultMconcat :: (Monoid a) => [a] -> a
defaultMconcat = \case
  [] -> mempty
  (x : xs) -> x <> mconcat xs

defaultShow :: (Show a) => a -> String
defaultShow x = showsPrec 0 x ""

defaultShowList :: (Show a) => [a] -> ShowS
defaultShowList [] = showString "[]"
defaultShowList (x : xs) = showChar '[' . shows x . showl xs
  where
    showl [] = showChar ']'
    showl (x : xs) =
      showChar ','
        . shows x
        . showl xs

defaultReadList :: (Read a) => ReadS [a]
defaultReadList = run
  where
    run r =
      do
        (l, s) <- minilex r
        case l of
          '[' -> case minilex s of
            [(']', t)] -> [([], t)]
            _ -> readElement s
          '(' -> do
            (x, t) <- run s
            (')', u) <- minilex t
            [(x, u)]
          _ -> []
    readElement t = do
      (x, u) <- readsPrec 0 t
      (xs, v) <- readTail u
      [(x : xs, v)]
    readTail s = do
      (l, t) <- minilex s
      case l of
        ']' -> [([], t)]
        ',' -> readElement t
        _ -> []

boundedEnumFrom :: (Enum a, Bounded a) => a -> [a]
boundedEnumFrom x = [x .. maxBound]

boundedEnumFromThen :: (Enum a, Bounded a, Ord a) => a -> a -> [a]
boundedEnumFromThen x y
  | x <= y = [x, y .. maxBound]
  | otherwise = [x, y .. minBound]

newtype HelperArrow a b = Arrow (a -> b)

instance (Semigroup b) => Semigroup (HelperArrow a b) where
  Arrow a <> Arrow b = Arrow (\x -> a x <> b x)

instance (Monoid b) => Monoid (HelperArrow a b) where
  mempty = Arrow (\_ -> mempty)

-- Functions from one type are a monad: each is given the same thing.
instance Functor (HelperArrow a) where
  fmap f (Arrow g) = Arrow (\x -> f (g x))

instance Applicative (HelperArrow a) where
  pure b = Arrow (\_ -> b)
  Arrow f <*> Arrow g = Arrow (\x -> f x (g x))

instance Monad (HelperArrow a) where
  Arrow g >>= f = Arrow (\x -> case f (g x) of Arrow h -> h x)

newtype HelperBool = Bool Bool

instance Eq HelperBool where
  (==) = enumEqual

instance Ord HelperBool where
  compare = enumCompare

instance Enum HelperBool where
  toEnum = \case
    0 -> Bool False
    1 -> Bool True
  fromEnum = \case
    Bool False -> 0
    Bool True -> 1
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded HelperBool where
  minBound = Bool False
  maxBound = Bool True

instance Show HelperBool where
  showsPrec _ (Bool bool) = case bool of
    False -> showString "False"
    True -> showString "True"

instance Read HelperBool where
  readsPrec _ string = do
    (token, string) <- lex string
    case token of
      "False" -> [(Bool False, string)]
      "True" -> [(Bool True, string)]
      _ -> []

newtype HelperChar = Char Char

instance Eq HelperChar where
  (==) = enumEqual

instance Ord HelperChar where
  compare = enumCompare

instance Enum HelperChar where
  toEnum x = Char (primIntToChar x)
  fromEnum (Char x) = primCharToInt x
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded HelperChar where
  minBound = Char '\0'
  maxBound = Char '\1114111'

instance Show HelperChar where
  showsPrec p (Char '\'') = showString "'\\''"
  showsPrec p (Char c) = showChar '\'' . showLitChar c . showChar '\''

  showList cs = showChar '"' . showl cs
    where
      showl [] = showChar '"'
      showl (Char '"' : cs) = showString "\\\"" . showl cs
      showl (Char c : cs) = showLitChar c . showl cs

instance Read HelperChar where
  readsPrec p =
    readParen
      False
      ( \r ->
          [ (Char c, t)
          | ('\'' : s, t) <- lex r,
            (c, "\'") <- readLitChar s
          ]
      )

  readList =
    readParen
      False
      ( \r ->
          [ (l, t)
          | ('"' : s, t) <- lex r,
            (l, _) <- readl s
          ]
      )
    where
      readl ('"' : s) = [([], s)]
      readl ('\\' : '&' : s) = readl s
      readl s =
        [ (Char c : cs, u)
        | (c, t) <- readLitChar s,
          (cs, u) <- readl t
        ]

newtype HelperInt = Int Int

instance Eq HelperInt where
  Int x == Int y = primEqualInt x y

-- Every comparison is written out, so that none goes the long way round
-- through the defaults of the class.
instance Ord HelperInt where
  Int x <= Int y = primLessThenEqualInt x y
  Int x < Int y = not (primLessThenEqualInt y x)
  Int x >= Int y = primLessThenEqualInt y x
  Int x > Int y = not (primLessThenEqualInt x y)
  compare (Int x) (Int y)
    | primEqualInt x y = EQ
    | primLessThenEqualInt x y = LT
    | otherwise = GT

instance Enum HelperInt where
  succ (Int x) = Int (x + 1)
  pred (Int x) = Int (x - 1)
  toEnum = Int
  fromEnum (Int x) = x
  enumFrom (Int x) = enumFromTo (Int x) maxBound
  enumFromTo (Int x) (Int y) = go x
    where
      go x
        | x > y = []
        | x == y = [Int x]
        | otherwise = Int x : go (x + 1)
  enumFromThen (Int x) (Int y) = enumFromThenTo (Int x) (Int y) (if y >= x then maxBound else minBound)
  -- As GHC has it: up or down as the second is, and never past the bounds
  -- of an Int, where adding the step would come round to the other end.
  enumFromThenTo (Int x1) (Int x2) (Int y)
    | x2 >= x1 =
        if y < x2
          then (if y < x1 then [] else [Int x1])
          else
            let delta = x2 - x1
                y' = y - delta
                up x
                  | x > y' = [Int x]
                  | otherwise = Int x : up (x + delta)
             in Int x1 : up x2
    | otherwise =
        if y > x2
          then (if y > x1 then [] else [Int x1])
          else
            let delta = x2 - x1
                y' = y - delta
                down x
                  | x < y' = [Int x]
                  | otherwise = Int x : down (x + delta)
             in Int x1 : down x2

instance Bounded HelperInt where
  minBound = Int primIntMinBound
  maxBound = Int primIntMaxBound

instance Num HelperInt where
  Int x + Int y = Int (primIntAdd x y)
  Int x - Int y = Int (primIntMinus x y)
  Int x * Int y = Int (primIntMultiply x y)
  negate (Int x) = Int (primIntNegate x)
  abs (Int x) = Int (primIntAbs x)
  signum (Int x) = Int (primIntSignum x)
  fromInteger x = Int (primIntegerTruncateToInt x)

instance Real HelperInt where
  toRational (Int x) = primIntToInteger x :% 1

-- Division that rounds down is written out too. The defaults of the class
-- go by way of a pair and of the class's own methods for the signs, and a
-- compiler made by Hazy divided that way millions of times: a vector is a
-- tree, and every step down it halves a number.
instance Integral HelperInt where
  Int x `quot` Int y = Int (primIntQuot x y)
  Int x `rem` Int y = Int (primIntRem x y)
  Int x `div` Int y = Int (intDiv x y)
  Int x `mod` Int y = Int (intMod x y)
  quotRem x y = (x `quot` y, x `rem` y)
  divMod x y = (x `div` y, x `mod` y)
  toInteger (Int x) = primIntToInteger x

-- Whether a number is below nothing.
intNegative :: Int -> Bool
intNegative x = not (primLessThenEqualInt 0 x)

-- What is left over and what the divisor is have different signs: the
-- quotient rounded toward nothing is one more than rounded down.
intDiv :: Int -> Int -> Int
intDiv x y =
  let q = primIntQuot x y
      r = primIntRem x y
   in if primEqualInt r 0
        then q
        else case intNegative r of
          True -> if intNegative y then q else primIntMinus q 1
          False -> if intNegative y then primIntMinus q 1 else q

intMod :: Int -> Int -> Int
intMod x y =
  let r = primIntRem x y
   in if primEqualInt r 0
        then r
        else case intNegative r of
          True -> if intNegative y then r else primIntAdd r y
          False -> if intNegative y then primIntAdd r y else r

instance Show HelperInt where
  showsPrec n (Int int) = showsPrec n (toInteger int)

instance Read HelperInt where
  readsPrec p r = [(Int (fromInteger i), t) | (i, t) <- readsPrec p r]

newtype HelperInteger = Integer Integer

instance Eq HelperInteger where
  Integer x == Integer y = primEqualInteger x y

instance Ord HelperInteger where
  Integer x <= Integer y = primLessThenEqualInteger x y
  Integer x < Integer y = not (primLessThenEqualInteger y x)
  Integer x >= Integer y = primLessThenEqualInteger y x
  Integer x > Integer y = not (primLessThenEqualInteger x y)
  compare (Integer x) (Integer y)
    | primEqualInteger x y = EQ
    | primLessThenEqualInteger x y = LT
    | otherwise = GT

instance Enum HelperInteger where
  succ (Integer x) = Integer (x + 1)
  pred (Integer x) = Integer (x - 1)
  toEnum x = Integer (primIntToInteger x)
  fromEnum (Integer x) = primIntegerCastToInt x
  enumFrom (Integer x) = [Integer x, Integer (x + 1) ..]
  enumFromTo (Integer x) (Integer y) = [Integer x, Integer (x + 1) .. Integer y]
  enumFromThen (Integer from) (Integer thenx) = run from
    where
      run from = Integer from : run (from + step)
      step = thenx - from
  enumFromThenTo (Integer from) (Integer thenx) (Integer to)
    | thenx >= from = up from
    | otherwise = down from
    where
      up from
        | from > to = []
        | otherwise = Integer from : up (from + step)
      down from
        | from < to = []
        | otherwise = Integer from : down (from + step)
      step = thenx - from

instance Num HelperInteger where
  Integer x + Integer y = Integer (primIntegerAdd x y)
  Integer x - Integer y = Integer (primIntegerMinus x y)
  Integer x * Integer y = Integer (primIntegerMultiply x y)
  negate (Integer x) = Integer (primIntegerNegate x)
  abs (Integer x) = Integer (primIntegerAbs x)
  signum (Integer x) = Integer (primIntegerSignum x)
  fromInteger x = Integer x

instance Real HelperInteger where
  toRational (Integer x) = x :% 1

instance Integral HelperInteger where
  Integer x `quot` Integer y = Integer (primIntegerQuot x y)
  Integer x `rem` Integer y = Integer (primIntegerRem x y)
  quotRem x y = (x `quot` y, x `rem` y)
  toInteger (Integer x) = x

instance Show HelperInteger where
  showsPrec n (Integer integer) = showSigned showInt n integer

instance Read HelperInteger where
  readsPrec p s = [(Integer i, t) | (i, t) <- readWhole s]

newtype HelperOrdering = Ordering Ordering

instance Eq HelperOrdering where
  (==) = enumEqual

instance Ord HelperOrdering where
  compare = enumCompare

instance Enum HelperOrdering where
  toEnum = \case
    0 -> Ordering LT
    1 -> Ordering EQ
    2 -> Ordering GT
  fromEnum = \case
    Ordering LT -> 0
    Ordering EQ -> 1
    Ordering GT -> 2
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded HelperOrdering where
  minBound = Ordering LT
  maxBound = Ordering GT

instance Semigroup HelperOrdering where
  Ordering EQ <> y = y
  x <> _ = x

instance Monoid HelperOrdering where
  mempty = Ordering EQ

instance Show HelperOrdering where
  showsPrec _ (Ordering order) = case order of
    LT -> showString "LT"
    EQ -> showString "EQ"
    GT -> showString "GT"

instance Read HelperOrdering where
  readsPrec _ string = do
    (token, string) <- lex string
    case token of
      "LT" -> [(Ordering LT, string)]
      "EQ" -> [(Ordering EQ, string)]
      "GT" -> [(Ordering GT, string)]
      _ -> []

newtype HelperList a = List {list :: [a]}

instance (Eq a) => Eq (HelperList a) where
  List [] == List [] = True
  List (x : xs) == List (x' : xs') = x == x' && xs == xs'
  _ == _ = False

instance (Ord a) => Ord (HelperList a) where
  List [] `compare` List [] = EQ
  List [] `compare` List (_ : _) = LT
  List (_ : _) `compare` List [] = GT
  List (x : xs) `compare` List (x' : xs') = compare x x' <> compare xs xs'

instance (Show a) => Show (HelperList a) where
  showsPrec p (List xs) = showList xs

instance (Read a) => Read (HelperList a) where
  readsPrec p s = [(List l, t) | (l, t) <- readList s]

instance Functor HelperList where
  fmap f (List xs) = List (map f xs)

instance Applicative HelperList where
  pure x = List [x]
  (<*>) = ap

instance Monad HelperList where
  List m >>= k = List $ concat $ map (list . k) m

instance MonadFail HelperList where
  fail _ = List []

instance Semigroup (HelperList a) where
  List xs <> List ys = List (xs ++ ys)

instance Monoid (HelperList a) where
  mempty = List []

-- What a list holds is gone through as it is, without the class's way
-- round through a list.
instance Foldable HelperList where
  foldr f z (List xs) = go xs
    where
      go [] = z
      go (y : ys) = f y (go ys)
  toList (List xs) = xs
  null (List []) = True
  null (List _) = False

-- A list made by an action is a HelperList as it is, so it is not gone
-- through again to be made one.
instance Traversable HelperList where
  traverse f (List xs) = primUnsafeCoerce (go xs)
    where
      go [] = pure []
      go (y : ys) = (:) <$> f y <*> go ys
  mapM f (List xs) = primUnsafeCoerce (go xs)
    where
      go [] = return []
      go (y : ys) = f y >>= \z -> go ys >>= \zs -> return (z : zs)
  sequence (List xs) = primUnsafeCoerce (go xs)
    where
      go [] = return []
      go (y : ys) = y >>= \z -> go ys >>= \zs -> return (z : zs)

newtype HelperNonEmpty a = NonEmpty {nonEmpty :: NonEmpty a}

instance (Eq a) => Eq (HelperNonEmpty a) where
  NonEmpty (x :| xs) == NonEmpty (x' :| xs') = x == x' && xs == xs'

instance (Ord a) => Ord (HelperNonEmpty a) where
  NonEmpty (x :| xs) `compare` NonEmpty (x' :| xs') = compare x x' <> compare xs xs'

instance (Show a) => Show (HelperNonEmpty a) where
  showsPrec d (NonEmpty (x :| xs)) =
    showParen (d > 5) $
      showsPrec 6 x . showString " :| " . showsPrec 6 xs

instance (Read a) => Read (HelperNonEmpty a) where
  readsPrec d = readParen (d > 5) $ \s -> do
    (x, t) <- readsPrec 6 s
    (":|", u) <- lex t
    (xs, v) <- readsPrec 6 u
    [(NonEmpty (x :| xs), v)]

instance Foldable HelperNonEmpty where
  foldr f z (NonEmpty (x :| xs)) = f x (foldr f z xs)
  toList (NonEmpty (x :| xs)) = x : xs
  null _ = False

instance Traversable HelperNonEmpty where
  traverse f (NonEmpty (x :| xs)) = primUnsafeCoerce ((:|) <$> f x <*> traverse f xs)

instance Functor HelperNonEmpty where
  fmap f (NonEmpty (x :| xs)) = NonEmpty (f x :| fmap f xs)

instance Applicative HelperNonEmpty where
  pure x = NonEmpty (x :| [])
  (<*>) = ap

instance Monad HelperNonEmpty where
  NonEmpty (x :| xs) >>= k = NonEmpty $ case k x of
    NonEmpty (y :| ys) -> y :| ys ++ (xs >>= toList . k)
      where
        toList (NonEmpty (x :| xs)) = x : xs

instance Semigroup (HelperNonEmpty a) where
  NonEmpty (x :| xs) <> NonEmpty (y :| ys) = NonEmpty (x :| xs ++ y : ys)

newtype HelperST s a = STx {st :: ST s a}

instance Functor (HelperST s) where
  fmap = liftM

instance Applicative (HelperST s) where
  pure x = STx (primSTPure x)
  (<*>) = ap

instance Monad (HelperST s) where
  STx m >>= f = STx (primSTBind m (st . f))

instance (Semigroup a) => Semigroup (HelperST s a) where
  STx a <> STx b = STx (liftA2 (<>) a b)

instance (Monoid a) => Monoid (HelperST s a) where
  mempty = pure mempty

newtype HelperUnit = Unit ()

instance Enum HelperUnit where
  fromEnum (Unit ()) = 0
  toEnum 0 = Unit ()
  enumFrom (Unit ()) = [Unit ()]
  enumFromThen (Unit ()) (Unit ()) = repeat (Unit ())

newtype HelperRatio a = Ratio {ratio :: Ratio a}

instance (Eq a) => Eq (HelperRatio a) where
  Ratio (n :% d) == Ratio (n' :% d') =
    n == n' && d == d'

instance (Integral a) => Ord (HelperRatio a) where
  Ratio (x :% y) <= Ratio (x' :% y') = x * y' <= x' * y
  Ratio (x :% y) < Ratio (x' :% y') = x * y' < x' * y

instance (Integral a) => Num (HelperRatio a) where
  Ratio (x :% y) + Ratio (x' :% y') = reduce (x * y' + x' * y) (y * y')
  Ratio (x :% y) * Ratio (x' :% y') = reduce (x * x') (y * y')
  negate (Ratio (x :% y)) = Ratio ((negate x) :% y)
  abs (Ratio (x :% y)) = Ratio (abs x :% y)
  signum (Ratio (x :% y)) = Ratio (signum x :% 1)
  fromInteger x = Ratio (fromInteger x :% 1)

instance (Integral a) => Real (HelperRatio a) where
  toRational (Ratio (x :% y)) = toInteger x :% toInteger y

instance (Integral a) => Fractional (HelperRatio a) where
  Ratio (x :% y) / Ratio (x' :% y') = Ratio $ (x * y') % (y * x')
  recip (Ratio (x :% y)) = Ratio (y % x)
  fromRational (x :% y) = Ratio (fromInteger x :% fromInteger y)

instance (Integral a) => Enum (HelperRatio a) where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate . ratio
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

instance (Integral a) => RealFrac (HelperRatio a) where
  properFraction (Ratio (x :% y)) = (fromIntegral q, Ratio (r :% y))
    where
      (q, r) = quotRem x y

instance (Show a) => Show (HelperRatio a) where
  showsPrec p (Ratio (x :% y)) =
    showParen (p > 7) $
      showsPrec (7 + 1) x
        . showString " % "
        . showsPrec (7 + 1) y

instance (Integral a, Read a) => Read (HelperRatio a) where
  readsPrec p =
    readParen
      (p > 7)
      ( \r ->
          [ (Ratio (x % y), u)
          | (x, s) <- readsPrec (7 + 1) r,
            ("%", t) <- lex s,
            (y, u) <- readsPrec (7 + 1) t
          ]
      )

newtype HelperFloat = HelperFloat Float

-- What the runtime has for a Float: see "Hazy". The functions and
-- operators are known by number.
instance Eq HelperFloat where
  x == y = primEqualInt (primFloatCompare x y) 1
  x /= y = not (x == y)

-- Every comparison is written out: one of a number that is not one is
-- false, and so not the other way round of another.
instance Ord HelperFloat where
  x < y = primEqualInt (primFloatCompare x y) 0
  x <= y = let order = primFloatCompare x y in primEqualInt order 0 || primEqualInt order 1
  x > y = primEqualInt (primFloatCompare x y) 2
  x >= y = let order = primFloatCompare x y in primEqualInt order 2 || primEqualInt order 1
  compare x y = case primFloatCompare x y of
    0 -> LT
    1 -> EQ
    _ -> GT

instance Num HelperFloat where
  x + y = primFloatBinary 32 0 x y
  x - y = primFloatBinary 32 1 x y
  x * y = primFloatBinary 32 2 x y
  negate = primFloatUnary 32 0
  abs = primFloatUnary 32 1
  signum = primFloatUnary 32 2
  fromInteger = primFloatFromInteger 32

instance Real HelperFloat where
  toRational x = (m % 1) * (2 % 1) ^^ e
    where
      (m, e) = decodeFloat x

instance Fractional HelperFloat where
  x / y = primFloatBinary 32 3 x y
  fromRational = fromRat

instance Floating HelperFloat where
  pi = 3.141592653589793238
  exp = primFloatUnary 32 3
  log = primFloatUnary 32 4
  sqrt = primFloatUnary 32 5
  sin = primFloatUnary 32 6
  cos = primFloatUnary 32 7
  tan = primFloatUnary 32 8
  asin = primFloatUnary 32 9
  acos = primFloatUnary 32 10
  atan = primFloatUnary 32 11
  sinh = primFloatUnary 32 12
  cosh = primFloatUnary 32 13
  tanh = primFloatUnary 32 14
  asinh = primFloatUnary 32 15
  acosh = primFloatUnary 32 16
  atanh = primFloatUnary 32 17
  x ** y = primFloatBinary 32 4 x y

instance RealFrac HelperFloat where
  properFraction x = (fromInteger whole, x - fromInteger whole)
    where
      whole = primFloatTruncate x
  truncate x = fromInteger (primFloatTruncate x)

instance RealFloat HelperFloat where
  floatRadix _ = 2
  floatDigits _ = 24
  floatRange _ = (-125, 128)
  decodeFloat x = (primFloatMantissa 32 x, primFloatExponent 32 x)
  encodeFloat = primFloatEncode 32
  isNaN = primFloatTest 32 0
  isInfinite = primFloatTest 32 1
  isDenormalized = primFloatTest 32 2
  isNegativeZero = primFloatTest 32 3
  isIEEE _ = True

-- A number below nothing is shown with its sign, in brackets if it is
-- an argument; so is nothing below nothing.
instance Show HelperFloat where
  showsPrec p x
    | x < 0 || isNegativeZero x = showParen (p > 6) (showChar '-' . showFloat (negate x))
    | otherwise = showFloat x

instance Read HelperFloat where
  readsPrec p = readSigned readFloat

newtype HelperDouble = HelperDouble Double

-- What the runtime has for a Double: see "Hazy". The functions and
-- operators are known by number.
instance Eq HelperDouble where
  x == y = primEqualInt (primFloatCompare x y) 1
  x /= y = not (x == y)

-- Every comparison is written out: one of a number that is not one is
-- false, and so not the other way round of another.
instance Ord HelperDouble where
  x < y = primEqualInt (primFloatCompare x y) 0
  x <= y = let order = primFloatCompare x y in primEqualInt order 0 || primEqualInt order 1
  x > y = primEqualInt (primFloatCompare x y) 2
  x >= y = let order = primFloatCompare x y in primEqualInt order 2 || primEqualInt order 1
  compare x y = case primFloatCompare x y of
    0 -> LT
    1 -> EQ
    _ -> GT

instance Num HelperDouble where
  x + y = primFloatBinary 64 0 x y
  x - y = primFloatBinary 64 1 x y
  x * y = primFloatBinary 64 2 x y
  negate = primFloatUnary 64 0
  abs = primFloatUnary 64 1
  signum = primFloatUnary 64 2
  fromInteger = primFloatFromInteger 64

instance Real HelperDouble where
  toRational x = (m % 1) * (2 % 1) ^^ e
    where
      (m, e) = decodeFloat x

instance Fractional HelperDouble where
  x / y = primFloatBinary 64 3 x y
  fromRational = fromRat

instance Floating HelperDouble where
  pi = 3.141592653589793238
  exp = primFloatUnary 64 3
  log = primFloatUnary 64 4
  sqrt = primFloatUnary 64 5
  sin = primFloatUnary 64 6
  cos = primFloatUnary 64 7
  tan = primFloatUnary 64 8
  asin = primFloatUnary 64 9
  acos = primFloatUnary 64 10
  atan = primFloatUnary 64 11
  sinh = primFloatUnary 64 12
  cosh = primFloatUnary 64 13
  tanh = primFloatUnary 64 14
  asinh = primFloatUnary 64 15
  acosh = primFloatUnary 64 16
  atanh = primFloatUnary 64 17
  x ** y = primFloatBinary 64 4 x y

instance RealFrac HelperDouble where
  properFraction x = (fromInteger whole, x - fromInteger whole)
    where
      whole = primFloatTruncate x
  truncate x = fromInteger (primFloatTruncate x)

instance RealFloat HelperDouble where
  floatRadix _ = 2
  floatDigits _ = 53
  floatRange _ = (-1021, 1024)
  decodeFloat x = (primFloatMantissa 64 x, primFloatExponent 64 x)
  encodeFloat = primFloatEncode 64
  isNaN = primFloatTest 64 0
  isInfinite = primFloatTest 64 1
  isDenormalized = primFloatTest 64 2
  isNegativeZero = primFloatTest 64 3
  isIEEE _ = True

instance Enum HelperFloat where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

instance Enum HelperDouble where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

-- A number below nothing is shown with its sign, in brackets if it is
-- an argument; so is nothing below nothing.
instance Show HelperDouble where
  showsPrec p x
    | x < 0 || isNegativeZero x = showParen (p > 6) (showChar '-' . showFloat (negate x))
    | otherwise = showFloat x

instance Read HelperDouble where
  readsPrec p = readSigned readFloat

-- A pair, as what it holds second: the instances of the builtin classes
-- for a pair that are not written out for every size of tuple.
newtype HelperTuple2 a b = Tuple2 (a, b)

instance Functor (HelperTuple2 a) where
  fmap f (Tuple2 (a, b)) = Tuple2 (a, f b)

instance Foldable (HelperTuple2 a) where
  foldr f z (Tuple2 (_, b)) = f b z
  toList (Tuple2 (_, b)) = [b]
  null _ = False
  length _ = 1

instance Traversable (HelperTuple2 a) where
  traverse f (Tuple2 (a, b)) = fmap (\c -> Tuple2 (a, c)) (f b)

-- What GHC says of an index that is not in its range.
indexError :: (Show a) => String -> (a, a) -> a -> b
indexError name bounds i =
  errorWithoutStackTrace
    ( "Ix{"
        ++ name
        ++ "}.index: Index "
        ++ showsPrec 11 i ""
        ++ " out of range ("
        ++ showsPrec 11 bounds ""
        ++ ")"
    )

-- What is numbered by something that counts, with that number.
countedIndex :: (Ord a, Show a) => String -> (a -> Int) -> (a, a) -> a -> Int
countedIndex name number bounds@(low, high) i
  | low <= i && i <= high = number i - number low
  | otherwise = indexError name bounds i

instance Ix HelperChar where
  range (Char low, Char high) = map Char [low .. high]
  index (Char low, Char high) (Char i) = countedIndex "Char" fromEnum (low, high) i
  inRange (Char low, Char high) (Char i) = low <= i && i <= high

instance Ix HelperInt where
  range (Int low, Int high) = map Int [low .. high]
  index (Int low, Int high) (Int i) = countedIndex "Int" id (low, high) i
  inRange (Int low, Int high) (Int i) = low <= i && i <= high

instance Ix HelperInteger where
  range (Integer low, Integer high) = map Integer [low .. high]
  index (Integer low, Integer high) (Integer i) = countedIndex "Integer" fromInteger (low, high) i
  inRange (Integer low, Integer high) (Integer i) = low <= i && i <= high

instance Ix HelperBool where
  range (Bool low, Bool high) = map Bool [low .. high]
  index (Bool low, Bool high) (Bool i) = countedIndex "Bool" fromEnum (low, high) i
  inRange (Bool low, Bool high) (Bool i) = low <= i && i <= high

instance Ix HelperOrdering where
  range (Ordering low, Ordering high) = map Ordering [low .. high]
  index (Ordering low, Ordering high) (Ordering i) = countedIndex "Ordering" fromEnum (low, high) i
  inRange (Ordering low, Ordering high) (Ordering i) = low <= i && i <= high

instance Eq HelperUnit where
  _ == _ = True

instance Ord HelperUnit where
  compare _ _ = EQ

instance Ix HelperUnit where
  range _ = [Unit ()]
  index _ _ = 0
  inRange _ _ = True

-- Tuples are numbered with the last part changing fastest.
instance (Eq a, Eq b) => Eq (HelperTuple2 a b) where
  Tuple2 x == Tuple2 y = x == y

instance (Ord a, Ord b) => Ord (HelperTuple2 a b) where
  compare (Tuple2 x) (Tuple2 y) = compare x y

instance (Ix a, Ix b) => Ix (HelperTuple2 a b) where
  range (Tuple2 (l1, l2), Tuple2 (u1, u2)) = [Tuple2 (i1, i2) | i1 <- range (l1, u1), i2 <- range (l2, u2)]
  index (Tuple2 (l1, l2), Tuple2 (u1, u2)) (Tuple2 (i1, i2))
    | inRange (l1, u1) i1 && inRange (l2, u2) i2 =
        index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2
    | otherwise = errorWithoutStackTrace "Error in array index"
  inRange (Tuple2 (l1, l2), Tuple2 (u1, u2)) (Tuple2 (i1, i2)) = inRange (l1, u1) i1 && inRange (l2, u2) i2

newtype HelperTuple3 a b c = Tuple3 (a, b, c)

instance (Eq a, Eq b, Eq c) => Eq (HelperTuple3 a b c) where
  Tuple3 x == Tuple3 y = x == y

instance (Ord a, Ord b, Ord c) => Ord (HelperTuple3 a b c) where
  compare (Tuple3 x) (Tuple3 y) = compare x y

instance (Ix a, Ix b, Ix c) => Ix (HelperTuple3 a b c) where
  range (Tuple3 (l1, l2, l3), Tuple3 (u1, u2, u3)) =
    [Tuple3 (i1, i2, i3) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3)]
  index (Tuple3 (l1, l2, l3), Tuple3 (u1, u2, u3)) (Tuple3 (i1, i2, i3))
    | inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 =
        (index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3
    | otherwise = errorWithoutStackTrace "Error in array index"
  inRange (Tuple3 (l1, l2, l3), Tuple3 (u1, u2, u3)) (Tuple3 (i1, i2, i3)) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3

newtype HelperTuple4 a b c d = Tuple4 (a, b, c, d)

instance (Eq a, Eq b, Eq c, Eq d) => Eq (HelperTuple4 a b c d) where
  Tuple4 x == Tuple4 y = x == y

instance (Ord a, Ord b, Ord c, Ord d) => Ord (HelperTuple4 a b c d) where
  compare (Tuple4 x) (Tuple4 y) = compare x y

instance (Ix a, Ix b, Ix c, Ix d) => Ix (HelperTuple4 a b c d) where
  range (Tuple4 (l1, l2, l3, l4), Tuple4 (u1, u2, u3, u4)) =
    [ Tuple4 (i1, i2, i3, i4)
    | i1 <- range (l1, u1),
      i2 <- range (l2, u2),
      i3 <- range (l3, u3),
      i4 <- range (l4, u4)
    ]
  index (Tuple4 (l1, l2, l3, l4), Tuple4 (u1, u2, u3, u4)) (Tuple4 (i1, i2, i3, i4))
    | inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 =
        ((index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3)
          * rangeSize (l4, u4)
          + index (l4, u4) i4
    | otherwise = errorWithoutStackTrace "Error in array index"
  inRange (Tuple4 (l1, l2, l3, l4), Tuple4 (u1, u2, u3, u4)) (Tuple4 (i1, i2, i3, i4)) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4

newtype HelperTuple5 a b c d e = Tuple5 (a, b, c, d, e)

instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (HelperTuple5 a b c d e) where
  Tuple5 x == Tuple5 y = x == y

instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (HelperTuple5 a b c d e) where
  compare (Tuple5 x) (Tuple5 y) = compare x y

instance (Ix a, Ix b, Ix c, Ix d, Ix e) => Ix (HelperTuple5 a b c d e) where
  range (Tuple5 (l1, l2, l3, l4, l5), Tuple5 (u1, u2, u3, u4, u5)) =
    [ Tuple5 (i1, i2, i3, i4, i5)
    | i1 <- range (l1, u1),
      i2 <- range (l2, u2),
      i3 <- range (l3, u3),
      i4 <- range (l4, u4),
      i5 <- range (l5, u5)
    ]
  index (Tuple5 (l1, l2, l3, l4, l5), Tuple5 (u1, u2, u3, u4, u5)) (Tuple5 (i1, i2, i3, i4, i5))
    | inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 =
        ( ((index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3)
            * rangeSize (l4, u4)
            + index (l4, u4) i4
        )
          * rangeSize (l5, u5)
          + index (l5, u5) i5
    | otherwise = errorWithoutStackTrace "Error in array index"
  inRange (Tuple5 (l1, l2, l3, l4, l5), Tuple5 (u1, u2, u3, u4, u5)) (Tuple5 (i1, i2, i3, i4, i5)) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5

