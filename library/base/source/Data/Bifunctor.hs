module Data.Bifunctor (Bifunctor (..)) where

import Control.Applicative (Const (..))

-- |
-- A type of two parameters that can be mapped over in both. An instance
-- gives 'bimap', or both 'first' and 'second'.
class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

instance Bifunctor (,) where
  bimap f g (a, b) = (f a, g b)

instance Bifunctor ((,,) x1) where
  bimap f g (x1, a, b) = (x1, f a, g b)

instance Bifunctor ((,,,) x1 x2) where
  bimap f g (x1, x2, a, b) = (x1, x2, f a, g b)

instance Bifunctor ((,,,,) x1 x2 x3) where
  bimap f g (x1, x2, x3, a, b) = (x1, x2, x3, f a, g b)

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
