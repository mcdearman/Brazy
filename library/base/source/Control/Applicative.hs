module Control.Applicative
  ( Applicative (..),
    Alternative (..),
    Const (..),
    WrappedMonad (..),
    WrappedArrow (..),
    ZipList (..),
    (<$>),
    (<$),
    (<**>),
    liftA,
    liftA3,
    optional,
    asum,
  )
where

import Data.Foldable (asum)

class (Applicative f) => Alternative f where
  empty :: f a
  infixl 3 <|>
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  some v = (:) <$> v <*> many v
  many :: f a -> f [a]
  many v = some v <|> pure []

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> other = other
  found <|> _ = found

instance Alternative [] where
  empty = []
  (<|>) = (++)

newtype Const a b = Const
  { getConst :: a
  }

instance Functor (Const a) where
  fmap _ (Const a) = Const a

instance (Monoid a) => Applicative (Const a) where
  pure _ = Const mempty
  Const a <*> Const b = Const (a <> b)

newtype WrappedMonad m a = WrapMonad
  { unwrapMonad :: m a
  }

newtype WrappedArrow a b c = WrapArrow
  { unwrapArrow :: a b c
  }

newtype ZipList a = ZipList
  { getZipList :: [a]
  }

infixl 4 <**>

(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
a <**> f = (\x g -> g x) <$> a <*> f

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

optional :: (Alternative f) => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing
