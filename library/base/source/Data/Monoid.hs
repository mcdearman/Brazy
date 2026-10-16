module Data.Monoid
  ( Monoid (..),
    (<>),
    Dual (..),
    Endo (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
    First (..),
    Last (..),
    Alt (..),
    Ap (..),
  )
where

import Control.Applicative (Alternative (..), liftA2)
import Hazy.Prelude (Monoid (..), Semigroup (..))

-- | The opposite way round.
newtype Dual a = Dual
  { getDual :: a
  }
  deriving (Eq, Ord, Show, Read)

instance (Semigroup a) => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)

instance (Monoid a) => Monoid (Dual a) where
  mempty = Dual mempty

instance Functor Dual where
  fmap f (Dual a) = Dual (f a)

instance Bounded a => Bounded (Dual a) where
  minBound = Dual minBound
  maxBound = Dual maxBound

-- | Functions of a thing to itself, one after another.
newtype Endo a = Endo
  { appEndo :: a -> a
  }

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

-- | Whether all are true.
newtype All = All
  { getAll :: Bool
  }
  deriving (Eq, Ord, Show, Read, Bounded)

instance Semigroup All where
  All a <> All b = All (a && b)

instance Monoid All where
  mempty = All True

-- | Whether any is true.
newtype Any = Any
  { getAny :: Bool
  }
  deriving (Eq, Ord, Show, Read, Bounded)

instance Semigroup Any where
  Any a <> Any b = Any (a || b)

instance Monoid Any where
  mempty = Any False

newtype Sum a = Sum
  { getSum :: a
  }
  deriving (Eq, Ord, Show, Read, Bounded)

instance (Num a) => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

instance Functor Sum where
  fmap f (Sum a) = Sum (f a)

instance (Num a) => Num (Sum a) where
  Sum a + Sum b = Sum (a + b)
  Sum a - Sum b = Sum (a - b)
  Sum a * Sum b = Sum (a * b)
  negate (Sum a) = Sum (negate a)
  abs (Sum a) = Sum (abs a)
  signum (Sum a) = Sum (signum a)
  fromInteger n = Sum (fromInteger n)

newtype Product a = Product
  { getProduct :: a
  }
  deriving (Eq, Ord, Show, Read, Bounded)

instance (Num a) => Semigroup (Product a) where
  Product a <> Product b = Product (a * b)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

instance Functor Product where
  fmap f (Product a) = Product (f a)

instance (Num a) => Num (Product a) where
  Product a + Product b = Product (a + b)
  Product a - Product b = Product (a - b)
  Product a * Product b = Product (a * b)
  negate (Product a) = Product (negate a)
  abs (Product a) = Product (abs a)
  signum (Product a) = Product (signum a)
  fromInteger n = Product (fromInteger n)

-- | The first that is there.
newtype First a = First
  { getFirst :: Maybe a
  }
  deriving (Eq, Ord, Show, Read)

instance Semigroup (First a) where
  First Nothing <> b = b
  a <> _ = a

instance Monoid (First a) where
  mempty = First Nothing

instance Functor First where
  fmap f (First a) = First (fmap f a)

-- | The last that is there.
newtype Last a = Last
  { getLast :: Maybe a
  }
  deriving (Eq, Ord, Show, Read)

instance Semigroup (Last a) where
  a <> Last Nothing = a
  _ <> b = b

instance Monoid (Last a) where
  mempty = Last Nothing

instance Functor Last where
  fmap f (Last a) = Last (fmap f a)

-- | One or the other, as an 'Alternative' has it.
newtype Alt f a = Alt
  { getAlt :: f a
  }

instance (Eq (f a)) => Eq (Alt f a) where
  Alt a == Alt b = a == b

instance (Ord (f a)) => Ord (Alt f a) where
  compare (Alt a) (Alt b) = compare a b

instance (Show (f a)) => Show (Alt f a) where
  showsPrec d (Alt a) =
    showParen (d >= 11) (showString "Alt {getAlt = " . showsPrec 0 a . showString "}")

instance (Alternative f) => Semigroup (Alt f a) where
  Alt a <> Alt b = Alt (a <|> b)

instance (Alternative f) => Monoid (Alt f a) where
  mempty = Alt empty

instance (Functor f) => Functor (Alt f) where
  fmap f (Alt a) = Alt (fmap f a)

-- | What an 'Applicative' gives, put together.
newtype Ap f a = Ap
  { getAp :: f a
  }

instance (Eq (f a)) => Eq (Ap f a) where
  Ap a == Ap b = a == b

instance (Ord (f a)) => Ord (Ap f a) where
  compare (Ap a) (Ap b) = compare a b

instance (Show (f a)) => Show (Ap f a) where
  showsPrec d (Ap a) =
    showParen (d >= 11) (showString "Ap {getAp = " . showsPrec 0 a . showString "}")

instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
  Ap a <> Ap b = Ap (liftA2 (<>) a b)

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  mempty = Ap (pure mempty)

instance (Functor f) => Functor (Ap f) where
  fmap f (Ap a) = Ap (fmap f a)
