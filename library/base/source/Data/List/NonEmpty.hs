module Data.List.NonEmpty
  ( NonEmpty (..),
    map,
    intersperse,
    scanl,
    scanr,
    scanl1,
    scanr1,
    transpose,
    sortBy,
    sortWith,
    length,
    head,
    tail,
    last,
    init,
    singleton,
    (<|),
    cons,
    uncons,
    unfoldr,
    sort,
    sortOn,
    reverse,
    inits,
    tails,
    append,
    appendList,
    prependList,
    iterate,
    repeat,
    cycle,
    unfold,
    insert,
    some1,
    take,
    drop,
    splitAt,
    takeWhile,
    dropWhile,
    span,
    break,
    filter,
    partition,
    group,
    groupBy,
    groupWith,
    groupAllWith,
    group1,
    groupBy1,
    groupWith1,
    groupAllWith1,
    isPrefixOf,
    nub,
    nubBy,
    (!!),
    zip,
    zipWith,
    unzip,
    fromList,
    toList,
    nonEmpty,
    xor,
  )
where

import qualified Data.Foldable as Foldable (toList)
import Data.Function (on)
import qualified Data.List as List (cycle, drop, dropWhile, filter, inits, insert, intersperse, isPrefixOf, iterate, nubBy, partition, repeat, scanl, scanr, scanr1, sort, sortBy, sortOn, span, splitAt, tails, take, takeWhile, transpose)
import Data.Ord (comparing)
import Prelude hiding
  ( break,
    cycle,
    drop,
    dropWhile,
    filter,
    head,
    init,
    iterate,
    last,
    length,
    map,
    repeat,
    reverse,
    scanl,
    scanl1,
    scanr,
    scanr1,
    span,
    splitAt,
    tail,
    take,
    takeWhile,
    unzip,
    zip,
    zipWith,
    (!!),
  )
import qualified Prelude
import Control.Applicative (Alternative (..), liftA2)
import Hazy.Prelude (NonEmpty (..))

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f (a :| as) = f a :| Prelude.map f as

intersperse :: a -> NonEmpty a -> NonEmpty a
intersperse a (b :| bs) = b :| case bs of
  [] -> []
  _ : _ -> a : List.intersperse a bs

scanl :: (Foldable f) => (b -> a -> b) -> b -> f a -> NonEmpty b
scanl f z = fromList . List.scanl f z . Foldable.toList

scanr :: (Foldable f) => (a -> b -> b) -> b -> f a -> NonEmpty b
scanr f z = fromList . List.scanr f z . Foldable.toList

scanl1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanl1 f (a :| as) = fromList (List.scanl f a as)

scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanr1 f (a :| as) = fromList (List.scanr1 f (a : as))

transpose :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
transpose = fmap fromList . fromList . List.transpose . toList . fmap toList

sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy order (a :| as) = case List.sortBy order (a : as) of
  b : bs -> b :| bs
  [] -> a :| []

sortWith :: (Ord o) => (a -> o) -> NonEmpty a -> NonEmpty a
sortWith = sortBy . comparing

length :: NonEmpty a -> Int
length (_ :| as) = 1 Prelude.+ Prelude.length as

head :: NonEmpty a -> a
head (a :| _) = a

tail :: NonEmpty a -> [a]
tail (_ :| as) = as

last :: NonEmpty a -> a
last (a :| as) = Prelude.foldl (\_ x -> x) a as

init :: NonEmpty a -> [a]
init (a :| as) = case as of
  [] -> []
  _ -> a : Prelude.init as

singleton :: a -> NonEmpty a
singleton a = a :| []

infixr 5 <|

(<|) :: a -> NonEmpty a -> NonEmpty a
a <| (b :| bs) = a :| b : bs

cons :: a -> NonEmpty a -> NonEmpty a
cons a (b :| bs) = a :| b : bs

uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
uncons (a :| as) = (a, nonEmpty as)

unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfoldr f a = case f a of
  (b, next) -> b :| maybe [] go next
  where
    go c = case f c of
      (d, next) -> d : maybe [] go next

sort :: (Ord a) => NonEmpty a -> NonEmpty a
sort (a :| as) = case List.sort (a : as) of
  b : bs -> b :| bs
  [] -> a :| []

sortOn :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f (a :| as) = case List.sortOn f (a : as) of
  b : bs -> b :| bs
  [] -> a :| []

reverse :: NonEmpty a -> NonEmpty a
reverse (a :| as) = case Prelude.reverse (a : as) of
  b : bs -> b :| bs
  [] -> a :| []

inits :: (Foldable f) => f a -> NonEmpty [a]
inits = fromList . List.inits . Foldable.toList

tails :: (Foldable f) => f a -> NonEmpty [a]
tails = fromList . List.tails . Foldable.toList

append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append (a :| as) (b :| bs) = a :| (as ++ b : bs)

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (a :| as) bs = a :| (as ++ bs)

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  x : xs -> x :| xs ++ toList ne

iterate :: (a -> a) -> a -> NonEmpty a
iterate f a = a :| List.iterate f (f a)

repeat :: a -> NonEmpty a
repeat a = a :| List.repeat a

cycle :: NonEmpty a -> NonEmpty a
cycle = fromList . List.cycle . toList

unfold :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfold f a = case f a of
  (b, Nothing) -> b :| []
  (b, Just c) -> b <| unfold f c

insert :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
insert a = fromList . List.insert a . Foldable.toList

some1 :: (Alternative f) => f a -> f (NonEmpty a)
some1 x = liftA2 (:|) x (many x)

take :: Int -> NonEmpty a -> [a]
take n = List.take n . toList

drop :: Int -> NonEmpty a -> [a]
drop n = List.drop n . toList

splitAt :: Int -> NonEmpty a -> ([a], [a])
splitAt n = List.splitAt n . toList

takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile p = List.takeWhile p . toList

dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
dropWhile p = List.dropWhile p . toList

span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
span p = List.span p . toList

break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
break p = span (not . p)

filter :: (a -> Bool) -> NonEmpty a -> [a]
filter p = List.filter p . toList

partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
partition p = List.partition p . toList

group :: (Foldable f, Eq a) => f a -> [NonEmpty a]
group = groupBy (==)

groupBy :: (Foldable f) => (a -> a -> Bool) -> f a -> [NonEmpty a]
groupBy same = go . Foldable.toList
  where
    go [] = []
    go (x : xs) = case List.span (same x) xs of
      (ys, zs) -> (x :| ys) : go zs

groupWith :: (Foldable f, Eq b) => (a -> b) -> f a -> [NonEmpty a]
groupWith f = groupBy ((==) `on` f)

groupAllWith :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
groupAllWith f = groupWith f . List.sortBy (compare `on` f)

group1 :: (Eq a) => NonEmpty a -> NonEmpty (NonEmpty a)
group1 = groupBy1 (==)

groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1 same (x :| xs) = case List.span (same x) xs of
  (ys, zs) -> (x :| ys) :| groupBy same zs

groupWith1 :: (Eq b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupWith1 f = groupBy1 ((==) `on` f)

groupAllWith1 :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupAllWith1 f = groupWith1 f . sortWith f

isPrefixOf :: (Eq a) => [a] -> NonEmpty a -> Bool
isPrefixOf [] _ = True
isPrefixOf (y : ys) (x :| xs) = y == x && List.isPrefixOf ys xs

nub :: (Eq a) => NonEmpty a -> NonEmpty a
nub = nubBy (==)

nubBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
nubBy same (a :| as) = a :| List.nubBy same (List.filter (\b -> not (same a b)) as)

(!!) :: NonEmpty a -> Int -> a
(x :| xs) !! n
  | n == 0 = x
  | n > 0 = xs Prelude.!! (n - 1)
  | otherwise = errorWithoutStackTrace "NonEmpty.!! negative index"

zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zip (a :| as) (b :| bs) = (a, b) :| Prelude.zip as bs

zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith f (a :| as) (b :| bs) = f a b :| Prelude.zipWith f as bs

unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)

fromList :: [a] -> NonEmpty a
fromList = \case
  a : as -> a :| as
  [] -> error "NonEmpty.fromList: empty list"

toList :: NonEmpty a -> [a]
toList (a :| as) = a : as

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty = \case
  a : as -> Just (a :| as)
  [] -> Nothing

xor :: NonEmpty Bool -> Bool
xor (x :| xs) = foldr flip' x xs
  where
    flip' True y = not y
    flip' False y = y
