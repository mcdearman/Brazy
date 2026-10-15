module Data.Vector
  ( Vector,
    length,
    null,
    (!),
    (!?),
    head,
    last,
    unsafeIndex,
    unsafeHead,
    unsafeLast,
    indexM,
    headM,
    lastM,
    unsafeIndexM,
    unsafeHeadM,
    unsafeLastM,
    slice,
    init,
    tail,
    take,
    drop,
    splitAt,
    uncons,
    unsnoc,
    unsafeSlice,
    unsafeInit,
    unsafeTail,
    unsafeTake,
    unsafeDrop,
    empty,
    singleton,
    replicate,
    generate,
    iterateN,
    replicateM,
    generateM,
    iterateNM,
    unfoldr,
    unfoldrN,
    unfoldrExactN,
    unfoldrM,
    unfoldrNM,
    unfoldrExactNM,
    constructN,
    constructrN,
    enumFromN,
    enumFromStepN,
    enumFromTo,
    enumFromThenTo,
    cons,
    snoc,
    (++),
    concat,
    force,
    (//),
    update,
    update_,
    unsafeUpd,
    unsafeUpdate,
    unsafeUpdate_,
    accum,
    accumulate,
    accumulate_,
    unsafeAccum,
    unsafeAccumulate,
    unsafeAccumulate_,
    reverse,
    backpermute,
    unsafeBackpermute,
    indexed,
    map,
    imap,
    concatMap,
    mapM,
    imapM,
    mapM_,
    imapM_,
    forM,
    forM_,
    iforM,
    iforM_,
    zipWith,
    zipWith3,
    zipWith4,
    zipWith5,
    zipWith6,
    izipWith,
    izipWith3,
    izipWith4,
    izipWith5,
    izipWith6,
    zip,
    zip3,
    zip4,
    zip5,
    zip6,
    zipWithM,
    izipWithM,
    zipWithM_,
    izipWithM_,
    unzip,
    unzip3,
    unzip4,
    unzip5,
    unzip6,
    filter,
    ifilter,
    filterM,
    uniq,
    mapMaybe,
    imapMaybe,
    mapMaybeM,
    imapMaybeM,
    catMaybes,
    takeWhile,
    dropWhile,
    partition,
    unstablePartition,
    partitionWith,
    span,
    break,
    groupBy,
    group,
    elem,
    notElem,
    find,
    findIndex,
    findIndexR,
    findIndices,
    elemIndex,
    elemIndices,
    foldl,
    foldl1,
    foldl,
    foldl1,
    foldr,
    foldr1,
    foldr,
    foldr1,
    ifoldl,
    ifoldl,
    ifoldr,
    ifoldr,
    foldMap,
    foldMap,
    all,
    any,
    and,
    or,
    sum,
    product,
    maximum,
    maximumBy,
    maximumOn,
    minimum,
    minimumBy,
    minimumOn,
    minIndex,
    minIndexBy,
    maxIndex,
    maxIndexBy,
    foldM,
    ifoldM,
    foldM,
    ifoldM,
    fold1M,
    fold1M,
    foldM_,
    ifoldM_,
    foldM,
    ifoldM,
    fold1M_,
    fold1M,
    sequence,
    sequence_,
    prescanl,
    prescanl,
    postscanl,
    postscanl,
    scanl,
    scanl,
    scanl1,
    scanl1,
    iscanl,
    iscanl,
    prescanr,
    prescanr,
    postscanr,
    postscanr,
    scanr,
    scanr,
    scanr1,
    scanr1,
    iscanr,
    iscanr,
    eqBy,
    cmpBy,
    toList,
    fromList,
    fromListN,
  )
where

import Prelude hiding ((++), all, and, any, break, concat, concatMap, drop, dropWhile, elem, filter, foldMap, foldl, foldl1, foldr, foldr1, head, init, last, length, map, mapM, mapM_, maximum, minimum, notElem, null, or, product, replicate, reverse, scanl, scanl1, scanr, scanr1, sequence, sequence_, span, splitAt, sum, tail, take, takeWhile, unzip, unzip3, zip, zip3, zipWith, zipWith3)
import qualified Prelude
import Hazy.Prelude (placeholder)
import Control.Monad.ST (runST)
import Hazy (primArrayIndex, primArrayNew, primArrayRead, primArrayWrite, primSTBind, primSTPure, primUnsafeCoerce)

-- A vector is an array of the runtime's: its places one after another, so
-- that an element is found by its index at once. It was a tree, and every
-- step down it halved a number. Elements are not evaluated by being put
-- in.
data Vector a = Vector !Int !(Array a)

-- The runtime's array, which has nothing that can be matched; and one that
-- is being filled in.
data Array a

data Places s a

instance Functor Vector where
  fmap = map

instance Foldable Vector where
  foldr = foldr
  foldl = foldl
  length = length
  null = null

instance Traversable Vector where
  traverse f vector = fromListN (length vector) <$> Prelude.traverse f (toList vector)

instance Applicative Vector where
  pure = singleton
  fs <*> xs = fromList [f x | f <- toList fs, x <- toList xs]

instance Monad Vector where
  vector >>= f = concatMap f vector

instance (Eq a) => Eq (Vector a) where
  a == b = eqBy (==) a b

instance (Ord a) => Ord (Vector a) where
  compare = cmpBy compare

instance (Show a) => Show (Vector a) where
  showsPrec d vector = showsPrec d (toList vector)

instance Semigroup (Vector a) where
  (<>) = (++)

instance Monoid (Vector a) where
  mempty = empty

length :: Vector a -> Int
length (Vector count _) = count

null :: Vector a -> Bool
null (Vector count _) = count == 0

(!) :: Vector a -> Int -> a
(!) (Vector count array) index
  | index < 0 || index >= count = error ("Vector.!: index out of bounds " Prelude.++ show (index, count))
  | otherwise = at array index

(!?) :: Vector a -> Int -> Maybe a
(!?) (Vector count array) index
  | index < 0 || index >= count = Nothing
  | otherwise = Just (at array index)

head :: Vector a -> a
head vector = vector ! 0

last :: Vector a -> a
last vector = vector ! (length vector - 1)

unsafeIndex :: Vector a -> Int -> a
unsafeIndex = (!)

unsafeHead :: Vector a -> a
unsafeHead = head

unsafeLast :: Vector a -> a
unsafeLast = last

indexM :: (Monad m) => Vector a -> Int -> m a
indexM vector index = pure (vector ! index)

headM :: (Monad m) => Vector a -> m a
headM vector = pure (head vector)

lastM :: (Monad m) => Vector a -> m a
lastM vector = pure (last vector)

unsafeIndexM :: (Monad m) => Vector a -> Int -> m a
unsafeIndexM = indexM

unsafeHeadM :: (Monad m) => Vector a -> m a
unsafeHeadM = headM

unsafeLastM :: (Monad m) => Vector a -> m a
unsafeLastM = lastM

slice :: Int -> Int -> Vector a -> Vector a
slice start count vector = fromListN count (Prelude.take count (Prelude.drop start (toList vector)))

init :: Vector a -> Vector a
init vector = take (length vector - 1) vector

tail :: Vector a -> Vector a
tail = drop 1

take :: Int -> Vector a -> Vector a
take count vector
  | count >= length vector = vector
  | otherwise = fromList (Prelude.take count (toList vector))

drop :: Int -> Vector a -> Vector a
drop count vector
  | count <= 0 = vector
  | otherwise = fromList (Prelude.drop count (toList vector))

splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt count vector = (take count vector, drop count vector)

uncons :: Vector a -> Maybe (a, Vector a)
uncons vector
  | null vector = Nothing
  | otherwise = Just (head vector, tail vector)

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc vector
  | null vector = Nothing
  | otherwise = Just (init vector, last vector)

unsafeSlice :: Int -> Int -> Vector a -> Vector a
unsafeSlice = slice

unsafeInit :: Vector a -> Vector a
unsafeInit = init

unsafeTail :: Vector a -> Vector a
unsafeTail = tail

unsafeTake :: Int -> Vector a -> Vector a
unsafeTake = take

unsafeDrop :: Int -> Vector a -> Vector a
unsafeDrop = drop

empty :: Vector a
empty = Vector 0 (filled 0 unfilled)

singleton :: a -> Vector a
singleton x = Vector 1 (filled 1 (\_ -> x))

replicate :: Int -> a -> Vector a
replicate count x = generate count (\_ -> x)

generate :: Int -> (Int -> a) -> Vector a
generate count f = Vector size (filled size f)
  where
    size = max 0 count

iterateN :: Int -> (a -> a) -> a -> Vector a
iterateN count f x = fromList (Prelude.take count (Prelude.iterate f x))

replicateM :: (Monad m) => Int -> m a -> m (Vector a)
replicateM count action = fromListN (max 0 count) <$> Prelude.sequence (Prelude.replicate count action)

generateM :: (Monad m) => Int -> (Int -> m a) -> m (Vector a)
generateM count f = fromListN (max 0 count) <$> Prelude.mapM f [0 .. count - 1]

iterateNM :: (Monad m) => Int -> (a -> m a) -> a -> m (Vector a)
iterateNM = placeholder

unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr f seed = fromList (go seed)
  where
    go state = case f state of
      Just (x, next) -> x : go next
      Nothing -> []

unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
unfoldrN count f seed = fromList (go count seed)
  where
    go left state
      | left <= 0 = []
      | otherwise = case f state of
          Just (x, next) -> x : go (left - 1) next
          Nothing -> []

unfoldrExactN :: Int -> (b -> (a, b)) -> b -> Vector a
unfoldrExactN = placeholder

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrM = placeholder

unfoldrNM :: (Monad m) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrNM = placeholder

unfoldrExactNM :: (Monad m) => Int -> (b -> m (a, b)) -> b -> m (Vector a)
unfoldrExactNM = placeholder

constructN :: Int -> (Vector a -> a) -> Vector a
constructN = placeholder

constructrN :: Int -> (Vector a -> a) -> Vector a
constructrN = placeholder

enumFromN :: (Num a) => a -> Int -> Vector a
enumFromN start count = fromList (Prelude.take count (Prelude.iterate (+ 1) start))

enumFromStepN :: (Num a) => a -> a -> Int -> Vector a
enumFromStepN start step count = fromList (Prelude.take count (Prelude.iterate (+ step) start))

enumFromTo :: (Enum a) => a -> a -> Vector a
enumFromTo from to = fromList (Prelude.enumFromTo from to)

enumFromThenTo :: (Enum a) => a -> a -> a -> Vector a
enumFromThenTo from next to = fromList (Prelude.enumFromThenTo from next to)

cons :: a -> Vector a -> Vector a
cons x vector = fromListN (length vector + 1) (x : toList vector)

snoc :: Vector a -> a -> Vector a
snoc vector x = fromListN (length vector + 1) (toList vector Prelude.++ [x])

infixr 5 ++

(++) :: Vector a -> Vector a -> Vector a
(++) a b = fromListN (length a + length b) (toList a Prelude.++ toList b)

concat :: [Vector a] -> Vector a
concat vectors = fromList (Prelude.concatMap toList vectors)

force :: Vector a -> Vector a
force vector = foldl' (\() x -> x `seq` ()) () vector `seq` vector

(//) :: Vector a -> [(Int, a)] -> Vector a
(//) vector changes = changed vector (\_ x -> x) changes

update :: Vector a -> Vector (Int, a) -> Vector a
update vector changes = vector // toList changes

update_ :: Vector a -> Vector Int -> Vector a -> Vector a
update_ = placeholder

unsafeUpd :: Vector a -> [(Int, a)] -> Vector a
unsafeUpd = (//)

unsafeUpdate :: Vector a -> Vector (Int, a) -> Vector a
unsafeUpdate = update

unsafeUpdate_ :: Vector a -> Vector Int -> Vector a -> Vector a
unsafeUpdate_ = placeholder

accum :: (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
accum f vector changes = changed vector f changes

accumulate :: (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
accumulate f vector changes = accum f vector (toList changes)

accumulate_ :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
accumulate_ = placeholder

unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
unsafeAccum = accum

unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
unsafeAccumulate = accumulate

unsafeAccumulate_ :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
unsafeAccumulate_ = placeholder

reverse :: Vector a -> Vector a
reverse vector = fromListN (length vector) (Prelude.reverse (toList vector))

backpermute :: Vector a -> Vector Int -> Vector a
backpermute vector indexes = map (vector !) indexes

unsafeBackpermute :: Vector a -> Vector Int -> Vector a
unsafeBackpermute = backpermute

indexed :: Vector a -> Vector (Int, a)
indexed = imap (\index x -> (index, x))

map :: (a -> b) -> Vector a -> Vector b
map f (Vector count array) = Vector count (filled count (\index -> f (at array index)))

imap :: (Int -> a -> b) -> Vector a -> Vector b
imap f (Vector count array) = Vector count (filled count (\index -> f index (at array index)))

concatMap :: (a -> Vector b) -> Vector a -> Vector b
concatMap f vector = concat (Prelude.map f (toList vector))

mapM :: (Monad m) => (a -> m b) -> Vector a -> m (Vector b)
mapM f vector = fromListN (length vector) <$> Prelude.mapM f (toList vector)

imapM :: (Monad m) => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM f vector = fromListN (length vector) <$> Prelude.mapM (\(index, x) -> f index x) (Prelude.zip [0 ..] (toList vector))

mapM_ :: (Monad m) => (a -> m b) -> Vector a -> m ()
mapM_ f vector = Prelude.mapM_ f (toList vector)

imapM_ :: (Monad m) => (Int -> a -> m b) -> Vector a -> m ()
imapM_ f vector = Prelude.mapM_ (\(index, x) -> f index x) (Prelude.zip [0 ..] (toList vector))

forM :: (Monad m) => Vector a -> (a -> m b) -> m (Vector b)
forM vector f = mapM f vector

forM_ :: (Monad m) => Vector a -> (a -> m b) -> m ()
forM_ vector f = mapM_ f vector

iforM :: (Monad m) => Vector a -> (Int -> a -> m b) -> m (Vector b)
iforM vector f = imapM f vector

iforM_ :: (Monad m) => Vector a -> (Int -> a -> m b) -> m ()
iforM_ vector f = imapM_ f vector

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f a b = fromList (Prelude.zipWith f (toList a) (toList b))

zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 f a b c = fromList (Prelude.zipWith3 f (toList a) (toList b) (toList c))

zipWith4 :: (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
zipWith4 f a b c d = fromList (zipLists4 f (toList a) (toList b) (toList c) (toList d))

zipWith5 :: (a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
zipWith5 f a b c d e =fromList (zipLists5 f (toList a) (toList b) (toList c) (toList d) (toList e))

zipWith6 ::
  (a -> b -> c -> d -> e -> f -> g) ->
  Vector a ->
  Vector b ->
  Vector c ->
  Vector d ->
  Vector e ->
  Vector f ->
  Vector g
zipWith6 f a b c d e g =
  fromList (zipLists6 f (toList a) (toList b) (toList c) (toList d) (toList e) (toList g))

izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
izipWith f a b = fromList (Prelude.zipWith3 f [0 ..] (toList a) (toList b))

izipWith3 :: (Int -> a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
izipWith3 f a b c = fromList (zipLists4 f [0 ..] (toList a) (toList b) (toList c))

izipWith4 :: (Int -> a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
izipWith4 = placeholder

izipWith5 :: (Int -> a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
izipWith5 = placeholder

izipWith6 ::
  (Int -> a -> b -> c -> d -> e -> f -> g) ->
  Vector a ->
  Vector b ->
  Vector c ->
  Vector d ->
  Vector e ->
  Vector f ->
  Vector g
izipWith6 = placeholder

zip :: Vector a -> Vector b -> Vector (a, b)
zip = zipWith (\a b -> (a, b))

zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
zip3 = zipWith3 (\a b c -> (a, b, c))

zip4 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector (a, b, c, d)
zip4 = zipWith4 (\a b c d -> (a, b, c, d))

zip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a, b, c, d, e)
zip5 = placeholder

zip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a, b, c, d, e, f)
zip6 = placeholder

zipWithM :: (Monad m) => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
zipWithM f a b = fromList <$> Prelude.sequence (Prelude.zipWith f (toList a) (toList b))

izipWithM :: (Monad m) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
izipWithM f a b = fromList <$> Prelude.sequence (Prelude.zipWith3 f [0 ..] (toList a) (toList b))

zipWithM_ :: (Monad m) => (a -> b -> m c) -> Vector a -> Vector b -> m ()
zipWithM_ f a b = Prelude.sequence_ (Prelude.zipWith f (toList a) (toList b))

izipWithM_ :: (Monad m) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
izipWithM_ f a b = Prelude.sequence_ (Prelude.zipWith3 f [0 ..] (toList a) (toList b))

unzip :: Vector (a, b) -> (Vector a, Vector b)
unzip vector = (map Prelude.fst vector, map Prelude.snd vector)

unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
unzip3 vector = (map (\(a, _, _) -> a) vector, map (\(_, b, _) -> b) vector, map (\(_, _, c) -> c) vector)

unzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
unzip4 = placeholder

unzip5 :: Vector (a, b, c, d, e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)
unzip5 = placeholder

unzip6 :: Vector (a, b, c, d, e, f) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
unzip6 = placeholder

filter :: (a -> Bool) -> Vector a -> Vector a
filter p vector = fromList (Prelude.filter p (toList vector))

ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
ifilter p vector = fromList [x | (index, x) <- Prelude.zip [0 ..] (toList vector), p index x]

filterM :: (Monad m) => (a -> m Bool) -> Vector a -> m (Vector a)
filterM p vector = fromList <$> go (toList vector)
  where
    go [] = pure []
    go (x : xs) = (\keep rest -> if keep then x : rest else rest) <$> p x <*> go xs

uniq :: (Eq a) => Vector a -> Vector a
uniq vector = fromList (go (toList vector))
  where
    go (x : y : rest)
      | x == y = go (y : rest)
      | otherwise = x : go (y : rest)
    go rest = rest

mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe f vector = fromList [y | Just y <- Prelude.map f (toList vector)]

imapMaybe :: (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe f vector = fromList [y | Just y <- Prelude.zipWith f [0 ..] (toList vector)]

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM = placeholder

imapMaybeM :: (Monad m) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM = placeholder

catMaybes :: Vector (Maybe a) -> Vector a
catMaybes vector = fromList [x | Just x <- toList vector]

takeWhile :: (a -> Bool) -> Vector a -> Vector a
takeWhile p vector = fromList (Prelude.takeWhile p (toList vector))

dropWhile :: (a -> Bool) -> Vector a -> Vector a
dropWhile p vector = fromList (Prelude.dropWhile p (toList vector))

partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition p vector = (filter p vector, filter (not . p) vector)

unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
unstablePartition = partition

partitionWith :: (a -> Either b c) -> Vector a -> (Vector b, Vector c)
partitionWith = placeholder

span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
span p vector = case Prelude.span p (toList vector) of
  (before, after) -> (fromList before, fromList after)

break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
break p = span (not . p)

groupBy :: (a -> a -> Bool) -> Vector a -> [Vector a]
groupBy = placeholder

group :: (Eq a) => Vector a -> [Vector a]
group = placeholder

infix 4 `elem`

elem :: (Eq a) => a -> Vector a -> Bool
elem x vector = Prelude.elem x (toList vector)

infix 4 `notElem`

notElem :: (Eq a) => a -> Vector a -> Bool
notElem x vector = not (elem x vector)

find :: (a -> Bool) -> Vector a -> Maybe a
find p vector = case Prelude.filter p (toList vector) of
  x : _ -> Just x
  [] -> Nothing

findIndex :: (a -> Bool) -> Vector a -> Maybe Int
findIndex p vector = case [index | (index, x) <- Prelude.zip [0 ..] (toList vector), p x] of
  index : _ -> Just index
  [] -> Nothing

findIndexR :: (a -> Bool) -> Vector a -> Maybe Int
findIndexR p vector = case findIndex p (reverse vector) of
  Just index -> Just (length vector - 1 - index)
  Nothing -> Nothing

findIndices :: (a -> Bool) -> Vector a -> Vector Int
findIndices p vector = fromList [index | (index, x) <- Prelude.zip [0 ..] (toList vector), p x]

elemIndex :: (Eq a) => a -> Vector a -> Maybe Int
elemIndex x = findIndex (== x)

elemIndices :: (Eq a) => a -> Vector a -> Vector Int
elemIndices x = findIndices (== x)

foldl :: (a -> b -> a) -> a -> Vector b -> a
foldl f z vector = Prelude.foldl f z (toList vector)

foldl1 :: (a -> a -> a) -> Vector a -> a
foldl1 f vector = Prelude.foldl1 f (toList vector)

foldl' :: (a -> b -> a) -> a -> Vector b -> a
foldl' f z vector = go z (toList vector)
  where
    go state [] = state
    go state (x : xs) = let next = f state x in next `seq` go next xs

foldl1' :: (a -> a -> a) -> Vector a -> a
foldl1' f vector = case toList vector of
  x : xs -> foldl' f x (fromList xs)
  [] -> error "Vector.foldl1': empty vector"

foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr f z (Vector count array) = go 0
  where
    go index
      | index >= count = z
      | otherwise = f (at array index) (go (index + 1))

foldr1 :: (a -> a -> a) -> Vector a -> a
foldr1 f vector = Prelude.foldr1 f (toList vector)

foldr' :: (a -> b -> b) -> b -> Vector a -> b
foldr' f z vector = foldl' (\rest x -> f x rest) z (reverse vector)

foldr1' :: (a -> a -> a) -> Vector a -> a
foldr1' f vector = Prelude.foldr1 f (toList vector)

ifoldl :: (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl f z vector = Prelude.foldl (\state (index, x) -> f state index x) z (Prelude.zip [0 ..] (toList vector))

ifoldl' :: (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl' f z vector = foldl' (\state (index, x) -> f state index x) z (indexed vector)

ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr f z vector = Prelude.foldr (\(index, x) rest -> f index x rest) z (Prelude.zip [0 ..] (toList vector))

ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr' = ifoldr

foldMap :: (Monoid m) => (a -> m) -> Vector a -> m
foldMap f = foldr (\x rest -> f x <> rest) mempty

foldMap' :: (Monoid m) => (a -> m) -> Vector a -> m
foldMap' = foldMap

all :: (a -> Bool) -> Vector a -> Bool
all p vector = Prelude.all p (toList vector)

any :: (a -> Bool) -> Vector a -> Bool
any p vector = Prelude.any p (toList vector)

and :: Vector Bool -> Bool
and vector = Prelude.and (toList vector)

or :: Vector Bool -> Bool
or vector = Prelude.or (toList vector)

sum :: (Num a) => Vector a -> a
sum vector = foldl' (+) 0 vector

product :: (Num a) => Vector a -> a
product vector = foldl' (*) 1 vector

maximum :: (Ord a) => Vector a -> a
maximum vector = Prelude.maximum (toList vector)

maximumBy :: (a -> a -> Ordering) -> Vector a -> a
maximumBy order vector = Prelude.foldl1 (\a b -> if order a b == GT then a else b) (toList vector)

maximumOn :: (Ord b) => (a -> b) -> Vector a -> a
maximumOn = placeholder

minimum :: (Ord a) => Vector a -> a
minimum vector = Prelude.minimum (toList vector)

minimumBy :: (a -> a -> Ordering) -> Vector a -> a
minimumBy order vector = Prelude.foldl1 (\a b -> if order a b == GT then b else a) (toList vector)

minimumOn :: (Ord b) => (a -> b) -> Vector a -> a
minimumOn = placeholder

minIndex :: (Ord a) => Vector a -> Int
minIndex = placeholder

minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
minIndexBy = placeholder

maxIndex :: (Ord a) => Vector a -> Int
maxIndex = placeholder

maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
maxIndexBy = placeholder

foldM :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m a
foldM f z vector = go z (toList vector)
  where
    go state [] = pure state
    go state (x : xs) = f state x >>= \next -> go next xs

ifoldM :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM f z vector = foldM (\state (index, x) -> f state index x) z (indexed vector)

foldM' :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m a
foldM' = foldM

ifoldM' :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM' = ifoldM

fold1M :: (Monad m) => (a -> a -> m a) -> Vector a -> m a
fold1M = placeholder

fold1M' :: (Monad m) => (a -> a -> m a) -> Vector a -> m a
fold1M' = placeholder

foldM_ :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m ()
foldM_ f z vector = foldM f z vector >> pure ()

ifoldM_ :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM_ f z vector = ifoldM f z vector >> pure ()

foldM'_ :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m ()
foldM'_ = foldM_

ifoldM'_ :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM'_ = ifoldM_

fold1M_ :: (Monad m) => (a -> a -> m a) -> Vector a -> m ()
fold1M_ = placeholder

fold1M'_ :: (Monad m) => (a -> a -> m a) -> Vector a -> m ()
fold1M'_ = placeholder

sequence :: (Monad m) => Vector (m a) -> m (Vector a)
sequence vector = mapM id vector

sequence_ :: (Monad m) => Vector (m a) -> m ()
sequence_ vector = mapM_ id vector

prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
prescanl = placeholder

prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
prescanl' = placeholder

postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
postscanl = placeholder

postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
postscanl' = placeholder

scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl f z vector = fromList (Prelude.scanl f z (toList vector))

scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl' = scanl

scanl1 :: (a -> a -> a) -> Vector a -> Vector a
scanl1 f vector = fromList (Prelude.scanl1 f (toList vector))

scanl1' :: (a -> a -> a) -> Vector a -> Vector a
scanl1' = scanl1

iscanl :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl = placeholder

iscanl' :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl' = placeholder

prescanr :: (a -> b -> b) -> b -> Vector a -> Vector b
prescanr = placeholder

prescanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
prescanr' = placeholder

postscanr :: (a -> b -> b) -> b -> Vector a -> Vector b
postscanr = placeholder

postscanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
postscanr' = placeholder

scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr f z vector = fromList (Prelude.scanr f z (toList vector))

scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr' = scanr

scanr1 :: (a -> a -> a) -> Vector a -> Vector a
scanr1 f vector = fromList (Prelude.scanr1 f (toList vector))

scanr1' :: (a -> a -> a) -> Vector a -> Vector a
scanr1' = scanr1

iscanr :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr = placeholder

iscanr' :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr' = placeholder

eqBy :: (a -> b -> Bool) -> Vector a -> Vector b -> Bool
eqBy same a b = length a == length b && Prelude.and (Prelude.zipWith same (toList a) (toList b))

cmpBy :: (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
cmpBy order a b = go (toList a) (toList b)
  where
    go [] [] = EQ
    go [] _ = LT
    go _ [] = GT
    go (x : xs) (y : ys) = case order x y of
      EQ -> go xs ys
      other -> other

toList :: Vector a -> [a]
toList vector = foldr (:) [] vector

fromList :: [a] -> Vector a
fromList xs = fromListN (Prelude.length xs) xs

fromListN :: Int -> [a] -> Vector a
fromListN count xs = Vector size (listed size xs)
  where
    size = max 0 (min count (Prelude.length xs))

-- An array of so many places, each holding what a function gives of its
-- index.
--
-- These fill an array in by the primitives of 'ST' themselves, which are
-- put where they are called, and not by way of the class: each step was
-- some functions made and called through evidence.
filled :: Int -> (Int -> a) -> Array a
filled count f = runST (primSTBind (primArrayNew count unfilled) (\places -> fill places 0))
  where
    fill places index
      | index >= count = primSTPure (primUnsafeCoerce places)
      | otherwise =
          let x = f index
           in primSTBind (primArrayWrite places index x) (\_ -> fill places (index + 1))

-- An array of the first so many elements of a list.
listed :: Int -> [a] -> Array a
listed count xs = runST (primSTBind (primArrayNew count unfilled) (\places -> fill places 0 xs))
  where
    fill places index list
      | index >= count = primSTPure (primUnsafeCoerce places)
      | otherwise = case list of
          x : rest -> primSTBind (primArrayWrite places index x) (\_ -> fill places (index + 1) rest)
          [] -> primSTPure (primUnsafeCoerce places)

-- A vector with some of its elements changed, each by a function of what
-- was there and what it is changed by.
changed :: Vector a -> (a -> b -> a) -> [(Int, b)] -> Vector a
changed (Vector count array) f changes =
  Vector count (runST (primSTBind (primArrayNew count unfilled) (\places -> copy places 0)))
  where
    copy places index
      | index >= count = change places changes
      | otherwise =
          let x = at array index
           in primSTBind (primArrayWrite places index x) (\_ -> copy places (index + 1))
    change places = \case
      [] -> primSTPure (primUnsafeCoerce places)
      (index, y) : rest
        | index < 0 || index >= count -> error ("Vector: index out of bounds " Prelude.++ show (index, count))
        | otherwise ->
            primSTBind (primArrayRead places index) $ \old ->
              let x = f old y
               in primSTBind (primArrayWrite places index x) (\_ -> change places rest)

-- What a place holds before it is filled in. Nothing asks for it.
unfilled :: a
unfilled = error "Vector: a place that was never filled in"

-- The element at an index of an array.
at :: Array a -> Int -> a
at = primArrayIndex

zipLists4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipLists4 f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zipLists4 f as bs cs ds
zipLists4 _ _ _ _ _ = []

zipLists5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipLists5 f (a : as) (b : bs) (c : cs) (d : ds) (e : es) = f a b c d e : zipLists5 f as bs cs ds es
zipLists5 _ _ _ _ _ _ = []

zipLists6 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipLists6 f (a : as) (b : bs) (c : cs) (d : ds) (e : es) (g : gs) = f a b c d e g : zipLists6 f as bs cs ds es gs
zipLists6 _ _ _ _ _ _ _ = []
