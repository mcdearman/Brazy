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

import qualified Data.List as List (sort, sortBy, sortOn)
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
import Control.Applicative (Alternative (..))
import Hazy.Prelude (NonEmpty (..), placeholder)

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f (a :| as) = f a :| Prelude.map f as

intersperse :: a -> NonEmpty a -> NonEmpty a
intersperse = placeholder

scanl :: (Foldable f) => (b -> a -> b) -> b -> f a -> NonEmpty b
scanl = placeholder

scanr :: (Foldable f) => (a -> b -> b) -> b -> f a -> NonEmpty b
scanr = placeholder

scanl1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanl1 = placeholder

scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanr1 = placeholder

transpose :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
transpose = placeholder

sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy order (a :| as) = case List.sortBy order (a : as) of
  b : bs -> b :| bs
  [] -> a :| []

sortWith :: (Ord o) => (a -> o) -> NonEmpty a -> NonEmpty a
sortWith = placeholder

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
unfoldr = placeholder

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
inits = placeholder

tails :: (Foldable f) => f a -> NonEmpty [a]
tails = placeholder

append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append (a :| as) (b :| bs) = a :| (as ++ b : bs)

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (a :| as) bs = a :| (as ++ bs)

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList = placeholder

iterate :: (a -> a) -> a -> NonEmpty a
iterate = placeholder

repeat :: a -> NonEmpty a
repeat = placeholder

cycle :: NonEmpty a -> NonEmpty a
cycle = placeholder

unfold :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfold = placeholder

insert :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
insert = placeholder

some1 :: (Alternative f) => f a -> f (NonEmpty a)
some1 = placeholder

take :: Int -> NonEmpty a -> [a]
take = placeholder

drop :: Int -> NonEmpty a -> [a]
drop = placeholder

splitAt :: Int -> NonEmpty a -> ([a], [a])
splitAt = placeholder

takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile = placeholder

dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
dropWhile = placeholder

span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
span = placeholder

break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
break = placeholder

filter :: (a -> Bool) -> NonEmpty a -> [a]
filter = placeholder

partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
partition = placeholder

group :: (Foldable f, Eq a) => f a -> [NonEmpty a]
group = placeholder

groupBy :: (Foldable f) => (a -> a -> Bool) -> f a -> [NonEmpty a]
groupBy = placeholder

groupWith :: (Foldable f, Eq b) => (a -> b) -> f a -> [NonEmpty a]
groupWith = placeholder

groupAllWith :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
groupAllWith = placeholder

group1 :: (Eq a) => NonEmpty a -> NonEmpty (NonEmpty a)
group1 = placeholder

groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1 = placeholder

groupWith1 :: (Eq b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupWith1 = placeholder

groupAllWith1 :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupAllWith1 = placeholder

isPrefixOf :: (Eq a) => [a] -> NonEmpty a -> Bool
isPrefixOf = placeholder

nub :: (Eq a) => NonEmpty a -> NonEmpty a
nub = placeholder

nubBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
nubBy = placeholder

(!!) :: NonEmpty a -> Int -> a
(!!) = placeholder

zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zip (a :| as) (b :| bs) = (a, b) :| Prelude.zip as bs

zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith f (a :| as) (b :| bs) = f a b :| Prelude.zipWith f as bs

unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip = placeholder

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
xor = placeholder
