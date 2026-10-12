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
import Hazy.Prelude (placeholder)

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for xs f = traverse f xs

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM xs f = mapM f xs

mapAccumL :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumL = placeholder

mapAccumR :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumR = placeholder

fmapDefault :: (Traversable t) => (a -> b) -> t a -> t b
fmapDefault f = runIdentity . traverse (Identity . f)

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . traverse (Const . f)
