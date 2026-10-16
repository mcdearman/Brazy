module Data.Semigroup
  ( Semigroup (..),
    stimesMonoid,
    stimesIdempotent,
    stimesIdempotentMonoid,
    mtimesDefault,
    Min (..),
    Max (..),
    First (..),
    Last (..),
    WrappedMonoid (..),
    Dual (..),
    Endo (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
    diff,
    cycle1,
    Arg (..),
    ArgMin,
    ArgMax,
  )
where

import Data.Monoid
  ( All (..),
    Any (..),
    Dual (..),
    Endo (..),
    Product (..),
    Sum (..),
  )
import Hazy.Prelude (Semigroup (..))

stimesMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesMonoid n x = case compare n 0 of
  LT -> error "stimesMonoid: negative multiplier"
  EQ -> mempty
  GT -> mconcat (replicate (fromIntegral n) x)

stimesIdempotent :: (Integral b) => b -> a -> a
stimesIdempotent n x
  | n <= 0 = error "stimesIdempotent: positive multiplier expected"
  | otherwise = x

stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesIdempotentMonoid n x = case compare n 0 of
  LT -> error "stimesIdempotentMonoid: negative multiplier"
  EQ -> mempty
  GT -> x

mtimesDefault :: (Integral b, Monoid a) => b -> a -> a
mtimesDefault n x
  | n == 0 = mempty
  | otherwise = stimes n x

-- | The least.
newtype Min a = Min
  { getMin :: a
  }
  deriving (Eq, Ord, Show, Read, Bounded)

instance (Ord a) => Semigroup (Min a) where
  Min a <> Min b = Min (min a b)

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = maxBound

instance Functor Min where
  fmap f (Min a) = Min (f a)

-- | The greatest.
newtype Max a = Max
  { getMax :: a
  }
  deriving (Eq, Ord, Show, Read, Bounded)

instance (Ord a) => Semigroup (Max a) where
  Max a <> Max b = Max (max a b)

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = minBound

instance Functor Max where
  fmap f (Max a) = Max (f a)

-- | The first.
newtype First a = First
  { getFirst :: a
  }
  deriving (Eq, Ord, Show, Read, Bounded)

instance Semigroup (First a) where
  a <> _ = a

instance Functor First where
  fmap f (First a) = First (f a)

-- | The last.
newtype Last a = Last
  { getLast :: a
  }
  deriving (Eq, Ord, Show, Read, Bounded)

instance Semigroup (Last a) where
  _ <> b = b

instance Functor Last where
  fmap f (Last a) = Last (f a)

-- | A monoid, as a semigroup.
newtype WrappedMonoid m = WrapMonoid
  { unwrapMonoid :: m
  }
  deriving (Eq, Ord, Show, Read, Bounded)

instance (Monoid m) => Semigroup (WrappedMonoid m) where
  WrapMonoid a <> WrapMonoid b = WrapMonoid (a `mappend` b)

instance (Monoid m) => Monoid (WrappedMonoid m) where
  mempty = WrapMonoid mempty

diff :: (Semigroup m) => m -> Endo m
diff = Endo . (<>)

cycle1 :: (Semigroup m) => m -> m
cycle1 xs = xs'
  where
    xs' = xs <> xs'

-- | A value with something carried along, which is compared by the value
-- alone.
data Arg a b = Arg a b
  deriving (Show, Read)

instance (Eq a) => Eq (Arg a b) where
  Arg a _ == Arg b _ = a == b

instance (Ord a) => Ord (Arg a b) where
  compare (Arg a _) (Arg b _) = compare a b
  min x@(Arg a _) y@(Arg b _)
    | a <= b = x
    | otherwise = y
  max x@(Arg a _) y@(Arg b _)
    | a >= b = x
    | otherwise = y

instance Functor (Arg a) where
  fmap f (Arg x a) = Arg x (f a)

type ArgMin a b = Min (Arg a b)

type ArgMax a b = Max (Arg a b)
