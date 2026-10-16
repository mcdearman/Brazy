module Data.Bifoldable
  ( Bifoldable (..),
    bifoldr',
    bifoldr1,
    bifoldrM,
    bifoldl',
    bifoldl1,
    bifoldlM,
    bitraverse_,
    bifor_,
    bimapM_,
    biforM_,
    bimsum,
    bisequenceA_,
    bisequence_,
    biasum,
    biList,
    binull,
    bilength,
    bielem,
    bimaximum,
    biminimum,
    bisum,
    biproduct,
    biconcat,
    biconcatMap,
    biand,
    bior,
    biany,
    biall,
    bimaximumBy,
    biminimumBy,
    binotElem,
    bifind,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), Const (..))
import Data.Monoid (All (..), Any (..), Endo (..), First (..), Product (..), Sum (..))

-- |
-- A type of two parameters whose values can be folded over in both. An
-- instance gives 'bifoldMap' or 'bifoldr'.
class Bifoldable p where
  bifold :: (Monoid m) => p m m -> m
  bifold = bifoldMap id id

  bifoldMap :: (Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
  bifoldMap f g = bifoldr (mappend . f) (mappend . g) mempty

  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldr f g z t = appEndo (bifoldMap (Endo . f) (Endo . g) t) z

  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldl f g z t = bifoldr (\a k c -> k (f c a)) (\b k c -> k (g c b)) id t z

instance Bifoldable (,) where
  bifoldMap f g (a, b) = f a `mappend` g b

instance Bifoldable ((,,) x1) where
  bifoldMap f g (_, a, b) = f a `mappend` g b

instance Bifoldable ((,,,) x1 x2) where
  bifoldMap f g (_, _, a, b) = f a `mappend` g b

instance Bifoldable ((,,,,) x1 x2 x3) where
  bifoldMap f g (_, _, _, a, b) = f a `mappend` g b

instance Bifoldable Either where
  bifoldMap f _ (Left a) = f a
  bifoldMap _ g (Right b) = g b

instance Bifoldable Const where
  bifoldMap f _ (Const a) = f a

bifoldr' :: (Bifoldable t) => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' f g z0 xs = bifoldl (\k a z -> k $! f a z) (\k b z -> k $! g b z) id xs z0

bifoldr1 :: (Bifoldable t) => (a -> a -> a) -> t a a -> a
bifoldr1 f xs = case bifoldr step step Nothing xs of
  Just x -> x
  Nothing -> errorWithoutStackTrace "bifoldr1: empty structure"
  where
    step x Nothing = Just x
    step x (Just y) = Just (f x y)

bifoldrM :: (Bifoldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM f g z0 xs = bifoldl (\k a z -> f a z >>= k) (\k b z -> g b z >>= k) return xs z0

bifoldl' :: (Bifoldable t) => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' f g z0 xs = bifoldr (\a k z -> k $! f z a) (\b k z -> k $! g z b) id xs z0

bifoldl1 :: (Bifoldable t) => (a -> a -> a) -> t a a -> a
bifoldl1 f xs = case bifoldl step step Nothing xs of
  Just x -> x
  Nothing -> errorWithoutStackTrace "bifoldl1: empty structure"
  where
    step Nothing y = Just y
    step (Just x) y = Just (f x y)

bifoldlM :: (Bifoldable t, Monad m) => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a
bifoldlM f g z0 xs = bifoldr (\a k z -> f z a >>= k) (\b k z -> g z b >>= k) return xs z0

bitraverse_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr ((*>) . f) ((*>) . g) (pure ())

bifor_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ t f g = bitraverse_ f g t

bimapM_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bimapM_ = bitraverse_

biforM_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
biforM_ = bifor_

bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = bifoldr (*>) (*>) (pure ())

bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequence_ = bisequenceA_

biasum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
biasum = bifoldr (<|>) (<|>) empty

bimsum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
bimsum = biasum

biList :: (Bifoldable t) => t a a -> [a]
biList = bifoldr (:) (:) []

binull :: (Bifoldable t) => t a b -> Bool
binull = bifoldr (\_ _ -> False) (\_ _ -> False) True

bilength :: (Bifoldable t) => t a b -> Int
bilength = bifoldl' (\c _ -> c + 1) (\c _ -> c + 1) 0

bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem x = biany (== x) (== x)

binotElem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
binotElem x = not . bielem x

bimaximum :: (Bifoldable t, Ord a) => t a a -> a
bimaximum xs = case bifoldr step step Nothing xs of
  Just x -> x
  Nothing -> errorWithoutStackTrace "bimaximum: empty structure"
  where
    step x Nothing = Just x
    step x (Just y) = Just (max x y)

biminimum :: (Bifoldable t, Ord a) => t a a -> a
biminimum xs = case bifoldr step step Nothing xs of
  Just x -> x
  Nothing -> errorWithoutStackTrace "biminimum: empty structure"
  where
    step x Nothing = Just x
    step x (Just y) = Just (min x y)

bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = getSum . bifoldMap Sum Sum

biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = getProduct . bifoldMap Product Product

biconcat :: (Bifoldable t) => t [a] [a] -> [a]
biconcat = bifold

biconcatMap :: (Bifoldable t) => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap

biand :: (Bifoldable t) => t Bool Bool -> Bool
biand = getAll . bifoldMap All All

bior :: (Bifoldable t) => t Bool Bool -> Bool
bior = getAny . bifoldMap Any Any

biany :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany p q = getAny . bifoldMap (Any . p) (Any . q)

biall :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall p q = getAll . bifoldMap (All . p) (All . q)

bimaximumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
bimaximumBy compare' = bifoldr1 larger
  where
    larger x y = case compare' x y of
      GT -> x
      _ -> y

biminimumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
biminimumBy compare' = bifoldr1 smaller
  where
    smaller x y = case compare' x y of
      GT -> y
      _ -> x

bifind :: (Bifoldable t) => (a -> Bool) -> t a a -> Maybe a
bifind p = getFirst . bifoldMap found found
  where
    found x = First (if p x then Just x else Nothing)
