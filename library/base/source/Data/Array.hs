-- |
-- Arrays as the Haskell 2010 report has them: numbered by any 'Ix', and
-- kept as one of the runtime's arrays, so that a place is reached at once.
module Data.Array
  ( module Data.Ix,
    Array,
    array,
    listArray,
    accumArray,
    (!),
    bounds,
    indices,
    elems,
    assocs,
    (//),
    accum,
    ixmap,
  )
where

import Data.Ix (Ix (..))
import qualified Data.Vector as Vector

infixl 9 !, //

-- | Its bounds, how many places it has, and what is in them.
data Array i e = Array !(i, i) !Int (Vector.Vector e)

-- A place that was never given anything.
missing :: a
missing = errorWithoutStackTrace "(Array.!): undefined array element"

array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array bounds' pairs =
  Array bounds' size (Vector.replicate size missing Vector.// [(index bounds' i, e) | (i, e) <- pairs])
  where
    size = rangeSize bounds'

listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray bounds' es = Array bounds' size (Vector.fromListN size (take size (es ++ repeat missing)))
  where
    size = rangeSize bounds'

accumArray :: (Ix i) => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
accumArray f initial bounds' pairs =
  Array bounds' size (Vector.accum f (Vector.replicate size initial) [(index bounds' i, a) | (i, a) <- pairs])
  where
    size = rangeSize bounds'

(!) :: (Ix i) => Array i e -> i -> e
Array bounds' _ places ! i = places Vector.! index bounds' i

bounds :: Array i e -> (i, i)
bounds (Array bounds' _ _) = bounds'

indices :: (Ix i) => Array i e -> [i]
indices (Array bounds' _ _) = range bounds'

elems :: Array i e -> [e]
elems (Array _ _ places) = Vector.toList places

assocs :: (Ix i) => Array i e -> [(i, e)]
assocs a = zip (indices a) (elems a)

(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
Array bounds' size places // pairs =
  Array bounds' size (places Vector.// [(index bounds' i, e) | (i, e) <- pairs])

accum :: (Ix i) => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
accum f (Array bounds' size places) pairs =
  Array bounds' size (Vector.accum f places [(index bounds' i, a) | (i, a) <- pairs])

ixmap :: (Ix i, Ix j) => (i, i) -> (i -> j) -> Array j e -> Array i e
ixmap bounds' f a = array bounds' [(i, a ! f i) | i <- range bounds']

instance Functor (Array i) where
  fmap f (Array bounds' size places) = Array bounds' size (fmap f places)

instance (Ix i, Eq e) => Eq (Array i e) where
  a == b = bounds a == bounds b && elems a == elems b

instance (Ix i, Ord e) => Ord (Array i e) where
  compare a b = compare (assocs a) (assocs b)

instance (Ix i, Show i, Show e) => Show (Array i e) where
  showsPrec p a =
    showParen (p > 9) $
      showString "array " . showsPrec 10 (bounds a) . showChar ' ' . showsPrec 10 (assocs a)

instance (Ix i, Read i, Read e) => Read (Array i e) where
  readsPrec p =
    readParen
      (p > 9)
      ( \r ->
          [ (array b as, u)
          | ("array", s) <- lex r,
            (b, t) <- readsPrec 10 s,
            (as, u) <- readsPrec 10 t
          ]
      )

instance Foldable (Array i) where
  foldr f z a = foldr f z (elems a)
  length (Array _ size _) = size
  null (Array _ size _) = size == 0

instance (Ix i) => Traversable (Array i) where
  traverse f (Array bounds' size places) = Array bounds' size <$> traverse f places
