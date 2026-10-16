module Data.Traversable
  ( Traversable (..),
    for,
    forM,
    mapAccumL,
    mapAccumR,
    fmapDefault,
    foldMapDefault,
  )
where

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for xs f = traverse f xs

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM xs f = mapM f xs

-- What is carried along from the left, or from the right.
newtype StateL s a = StateL (s -> (s, a))

newtype StateR s a = StateR (s -> (s, a))

instance Functor (StateL s) where
  fmap f (StateL k) = StateL $ \s -> let (s', v) = k s in (s', f v)

instance Applicative (StateL s) where
  pure x = StateL (\s -> (s, x))
  StateL kf <*> StateL kv = StateL $ \s ->
    let (s', f) = kf s
        (s'', v) = kv s'
     in (s'', f v)

instance Functor (StateR s) where
  fmap f (StateR k) = StateR $ \s -> let (s', v) = k s in (s', f v)

instance Applicative (StateR s) where
  pure x = StateR (\s -> (s, x))
  StateR kf <*> StateR kv = StateR $ \s ->
    let (s', v) = kv s
        (s'', f) = kf s'
     in (s'', f v)

mapAccumL :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumL f s t = run (traverse (\a -> StateL (\s' -> f s' a)) t) s
  where
    run (StateL k) = k

mapAccumR :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumR f s t = run (traverse (\a -> StateR (\s' -> f s' a)) t) s
  where
    run (StateR k) = k

fmapDefault :: (Traversable t) => (a -> b) -> t a -> t b
fmapDefault f = runIdentity . traverse (Identity . f)

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . traverse (Const . f)
