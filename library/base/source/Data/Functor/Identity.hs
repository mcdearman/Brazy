module Data.Functor.Identity (Identity (..)) where

newtype Identity a = Identity
  { runIdentity :: a
  }
  deriving (Eq, Ord)

-- Shown as what it holds is, as GHC has it, and not as a record.
instance (Show a) => Show (Identity a) where
  showsPrec d (Identity a) = showParen (d > 10) $ showString "Identity " . showsPrec 11 a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a

instance Foldable Identity where
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)
