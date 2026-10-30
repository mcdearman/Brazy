module Data.Foldable
  ( Foldable (..),
    foldrM,
    foldlM,
    traverse_,
    for_,
    sequenceA_,
    asum,
    mapM_,
    forM_,
    sequence_,
    msum,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    maximumBy,
    minimumBy,
    notElem,
    find,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import Hazy.Prelude (Foldable (..))

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z0 xs = foldl (\k x z -> f x z >>= k) pure xs z0

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldr (\x k z -> f z x >>= k) pure xs z0

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr (\x rest -> f x *> rest) (pure ())

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ xs f = traverse_ f xs

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (\x rest -> x *> rest) (pure ())

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ xs f = traverse_ f xs

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = asum

maximumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumBy order = foldr1 (\a b -> if order a b == LT then b else a)

minimumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
minimumBy order = foldr1 (\a b -> if order a b == GT then b else a)

find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find p = foldr (\x rest -> if p x then Just x else rest) Nothing
