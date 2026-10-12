module Control.Monad
  ( Functor (..),
    Monad (..),
    MonadFail (..),
    MonadPlus (..),
    mapM,
    mapM_,
    forM,
    forM_,
    sequence,
    sequence_,
    (=<<),
    (>=>),
    (<=<),
    forever,
    void,
    join,
    msum,
    mfilter,
    filterM,
    mapAndUnzipM,
    zipWithM,
    zipWithM_,
    foldM,
    foldM_,
    replicateM,
    replicateM_,
    guard,
    when,
    unless,
    liftM,
    liftM2,
    liftM3,
    liftM4,
    liftM5,
    ap,
    (<$!>),
  )
where

import Control.Applicative (Alternative (empty))
import Data.Foldable (msum)
import Hazy.Prelude (ap, join, liftM, placeholder)

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM xs f = mapM f xs

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ xs f = mapM_ f xs

infixr 1 >=>

(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
(f >=> g) a = f a >>= g

infixr 1 <=<

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(g <=< f) a = f a >>= g

forever :: (Applicative f) => f a -> f b
forever a = a *> forever a

void :: (Functor f) => f a -> f ()
void = fmap (const ())

mfilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a
mfilter = placeholder

filterM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr (\x rest -> (\keep -> if keep then (x :) else id) <$> p x <*> rest) (pure [])

mapAndUnzipM :: (Applicative m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = unzip <$> traverse f xs

zipWithM :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequenceA (zipWith f xs ys)

zipWithM_ :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = foldr (\a rest -> a *> rest) (pure ()) (zipWith f xs ys)

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM f z0 xs = foldr (\x k z -> f z x >>= k) pure xs z0

foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
foldM_ f z xs = foldM f z xs >> pure ()

replicateM :: (Applicative m) => Int -> m a -> m [a]
replicateM n a = if n <= 0 then pure [] else (:) <$> a <*> replicateM (n - 1) a

replicateM_ :: (Applicative m) => Int -> m a -> m ()
replicateM_ n a = if n <= 0 then pure () else a *> replicateM_ (n - 1) a

guard :: (Alternative f) => Bool -> f ()
guard condition = if condition then pure () else empty

when :: (Applicative f) => Bool -> f () -> f ()
when condition action = if condition then action else pure ()

unless :: (Applicative f) => Bool -> f () -> f ()
unless condition action = if condition then pure () else action

liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f a b = do
  x <- a
  y <- b
  pure (f x y)

liftM3 :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f a b c = do
  x <- a
  y <- b
  z <- c
  pure (f x y z)

liftM4 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 = placeholder

liftM5 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 = placeholder

infixl 4 <$!>

(<$!>) :: (Monad m) => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` pure z
