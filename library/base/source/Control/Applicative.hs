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
import qualified Hazy.PreludeIO as IO (catch, ioError, userError)

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

-- An action that fails with an error of input or output gives way to the
-- other, as GHC's does; nothing is an error itself.
instance Alternative IO where
  empty = IO.ioError (IO.userError "mzero")
  action <|> other = action `IO.catch` \_ -> other

newtype Const a b = Const
  { getConst :: a
  }

instance Functor (Const a) where
  fmap _ (Const a) = Const a

instance (Monoid a) => Applicative (Const a) where
  pure _ = Const mempty
  Const a <*> Const b = Const (a <> b)

instance (Eq a) => Eq (Const a b) where
  Const a == Const b = a == b

instance (Ord a) => Ord (Const a b) where
  compare (Const a) (Const b) = compare a b

instance (Show a) => Show (Const a b) where
  showsPrec d (Const a) = showParen (d > 10) (showString "Const " . showsPrec 11 a)

instance Foldable (Const a) where
  foldr _ z _ = z

instance Traversable (Const a) where
  traverse _ (Const a) = pure (Const a)

newtype WrappedMonad m a = WrapMonad
  { unwrapMonad :: m a
  }

instance (Monad m) => Functor (WrappedMonad m) where
  fmap f (WrapMonad v) = WrapMonad (v >>= \x -> return (f x))

instance (Monad m) => Applicative (WrappedMonad m) where
  pure = WrapMonad . return
  WrapMonad f <*> WrapMonad v = WrapMonad (f >>= \g -> v >>= \x -> return (g x))

instance (Monad m) => Monad (WrappedMonad m) where
  WrapMonad a >>= f = WrapMonad (a >>= unwrapMonad . f)

newtype WrappedArrow a b c = WrapArrow
  { unwrapArrow :: a b c
  }

newtype ZipList a = ZipList
  { getZipList :: [a]
  }
  deriving (Eq, Ord, Show)

-- Lists put together place by place, as long as the shortest.
instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)

instance Alternative ZipList where
  empty = ZipList []
  ZipList xs <|> ZipList ys = ZipList (xs ++ drop (length xs) ys)

instance Foldable ZipList where
  foldr f z (ZipList xs) = foldr f z xs

instance Traversable ZipList where
  traverse f (ZipList xs) = fmap ZipList (traverse f xs)

infixl 4 <**>

(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
a <**> f = (\x g -> g x) <$> a <*> f

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

optional :: (Alternative f) => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing
