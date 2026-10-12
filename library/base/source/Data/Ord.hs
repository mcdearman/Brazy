module Data.Ord
  ( Ord (..),
    Ordering (..),
    Down (..),
    comparing,
    clamp,
  )
where

newtype Down a = Down
  { getDown :: a
  }
  deriving (Eq, Show)

-- The order of what is inside, the other way round.
instance (Ord a) => Ord (Down a) where
  compare (Down a) (Down b) = compare b a

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing f a b = compare (f a) (f b)

clamp :: (Ord a) => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)
