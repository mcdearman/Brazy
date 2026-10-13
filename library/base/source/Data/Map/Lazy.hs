module Data.Map.Lazy
  ( Map,
    empty,
    singleton,
    fromSet,
    fromArgSet,
    fromList,
    fromListWith,
    fromListWithKey,
    fromAscList,
    fromAscListWith,
    fromAscListWithKey,
    fromDistinctAscList,
    fromDescList,
    fromDescListWith,
    fromDescListWithKey,
    fromDistinctDescList,
    insert,
    insertWith,
    insertWithKey,
    insertLookupWithKey,
    delete,
    adjust,
    adjustWithKey,
    update,
    updateWithKey,
    updateLookupWithKey,
    alter,
    alterF,
    lookup,
    (!?),
    (!),
    findWithDefault,
    member,
    notMember,
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,
    null,
    size,
    union,
    unionWith,
    unionWithKey,
    unions,
    unionsWith,
    difference,
    (\\),
    differenceWith,
    differenceWithKey,
    intersection,
    intersectionWith,
    intersectionWithKey,
    disjoint,
    compose,
    mergeWithKey,
    map,
    mapWithKey,
    traverseWithKey,
    traverseMaybeWithKey,
    mapAccum,
    mapAccumWithKey,
    mapAccumRWithKey,
    mapKeys,
    mapKeysWith,
    mapKeysMonotonic,
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,
    foldMapWithKey,
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,
    elems,
    keys,
    assocs,
    keysSet,
    argSet,
    toList,
    toAscList,
    toDescList,
    filter,
    filterWithKey,
    restrictKeys,
    withoutKeys,
    partition,
    partitionWithKey,
    takeWhileAntitone,
    dropWhileAntitone,
    spanAntitone,
    mapMaybe,
    mapMaybeWithKey,
    mapEither,
    mapEitherWithKey,
    split,
    splitLookup,
    splitRoot,
    isSubmapOf,
    isSubmapOfBy,
    isProperSubmapOf,
    isProperSubmapOfBy,
    lookupIndex,
    findIndex,
    elemAt,
    updateAt,
    deleteAt,
    take,
    drop,
    splitAt,
    lookupMin,
    lookupMax,
    findMin,
    findMax,
    deleteMin,
    deleteMax,
    deleteFindMin,
    deleteFindMax,
    updateMin,
    updateMax,
    updateMinWithKey,
    updateMaxWithKey,
    minView,
    maxView,
    minViewWithKey,
    maxViewWithKey,
    valid,
  )
where

import qualified Data.Set as Set (fromDistinctAscList, member, notMember, toAscList)
import Prelude hiding (drop, filter, foldl, foldr, lookup, map, null, splitAt, take)
import qualified Prelude
import Data.Semigroup (Arg)
import Data.Set (Set)
import Hazy.Prelude (placeholder)

-- A map is a tree balanced by weight: neither side of a node has more than
-- three times the other's entries. A value is not evaluated by being put in.
data Map k a
  = Bin !Int !k a !(Map k a) !(Map k a)
  | Tip

instance Functor (Map k) where
  fmap = map

instance Foldable (Map k) where
  foldr = foldr
  foldl = foldl
  length = size
  null = null

instance Traversable (Map k) where
  traverse f = traverseWithKey (\_ x -> f x)

instance (Eq k, Eq a) => Eq (Map k a) where
  a == b = size a == size b && toAscList a == toAscList b

instance (Ord k, Ord a) => Ord (Map k a) where
  compare a b = compare (toAscList a) (toAscList b)

instance (Show k, Show a) => Show (Map k a) where
  showsPrec d m = showParen (d > 10) (showString "fromList " . shows (toAscList m))

instance (Ord k) => Semigroup (Map k a) where
  (<>) = union

instance (Ord k) => Monoid (Map k a) where
  mempty = Tip

empty :: Map k a
empty = Tip

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip

fromSet :: (k -> a) -> Set k -> Map k a
fromSet f set = fromDistinctAscList [(k, f k) | k <- Set.toAscList set]

fromArgSet :: Set (Arg k a) -> Map k a
fromArgSet = placeholder

fromList :: (Ord k) => [(k, a)] -> Map k a
fromList = Prelude.foldl (\m (k, x) -> insert k x m) Tip

fromListWith :: (Ord k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith f = fromListWithKey (\_ new old -> f new old)

fromListWithKey :: (Ord k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey f = Prelude.foldl (\m (k, x) -> insertWithKey f k x m) Tip

fromAscList :: (Eq k) => [(k, a)] -> Map k a
fromAscList = fromAscListWithKey (\_ new _ -> new)

fromAscListWith :: (Eq k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWith f = fromAscListWithKey (\_ new old -> f new old)

fromAscListWithKey :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWithKey f = fromDistinctAscList . adjacent f

fromDistinctAscList :: [(k, a)] -> Map k a
fromDistinctAscList xs = Prelude.fst (build (Prelude.length xs) xs)

fromDescList :: (Eq k) => [(k, a)] -> Map k a
fromDescList = fromDescListWithKey (\_ new _ -> new)

fromDescListWith :: (Eq k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromDescListWith f = fromDescListWithKey (\_ new old -> f new old)

fromDescListWithKey :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromDescListWithKey f = fromDistinctAscList . Prelude.reverse . adjacent f

fromDistinctDescList :: [(k, a)] -> Map k a
fromDistinctDescList = fromDistinctAscList . Prelude.reverse

insert :: (Ord k) => k -> a -> Map k a -> Map k a
insert k x = \case
  Tip -> singleton k x
  Bin count ky y l r -> case compare k ky of
    LT -> balance ky y (insert k x l) r
    GT -> balance ky y l (insert k x r)
    EQ -> Bin count k x l r

insertWith :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f = insertWithKey (\_ new old -> f new old)

insertWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f k x = \case
  Tip -> singleton k x
  Bin count ky y l r -> case compare k ky of
    LT -> balance ky y (insertWithKey f k x l) r
    GT -> balance ky y l (insertWithKey f k x r)
    EQ -> Bin count k (f k x y) l r

insertLookupWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey f k x m = (lookup k m, insertWithKey f k x m)

delete :: (Ord k) => k -> Map k a -> Map k a
delete k = \case
  Tip -> Tip
  Bin _ ky y l r -> case compare k ky of
    LT -> balance ky y (delete k l) r
    GT -> balance ky y l (delete k r)
    EQ -> glue l r

adjust :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a
adjust f = adjustWithKey (\_ x -> f x)

adjustWithKey :: (Ord k) => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey f k = \case
  Tip -> Tip
  Bin count ky y l r -> case compare k ky of
    LT -> Bin count ky y (adjustWithKey f k l) r
    GT -> Bin count ky y l (adjustWithKey f k r)
    EQ -> Bin count ky (f ky y) l r

update :: (Ord k) => (a -> Maybe a) -> k -> Map k a -> Map k a
update f = updateWithKey (\_ x -> f x)

updateWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey f k = \case
  Tip -> Tip
  Bin count ky y l r -> case compare k ky of
    LT -> balance ky y (updateWithKey f k l) r
    GT -> balance ky y l (updateWithKey f k r)
    EQ -> case f ky y of
      Just changed -> Bin count ky changed l r
      Nothing -> glue l r

updateLookupWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
updateLookupWithKey f k m = case lookup k m of
  Nothing -> (Nothing, m)
  Just x -> case f k x of
    Just changed -> (Just changed, insert k changed m)
    Nothing -> (Just x, delete k m)

alter :: (Ord k) => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k m = case f (lookup k m) of
  Just x -> insert k x m
  Nothing -> delete k m

alterF :: (Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
alterF f k m = fmap change (f (lookup k m))
  where
    change = \case
      Just x -> insert k x m
      Nothing -> delete k m

lookup :: (Ord k) => k -> Map k a -> Maybe a
lookup k = \case
  Tip -> Nothing
  Bin _ ky y l r -> case compare k ky of
    LT -> lookup k l
    GT -> lookup k r
    EQ -> Just y

(!?) :: (Ord k) => Map k a -> k -> Maybe a
(!?) m k = lookup k m

(!) :: (Ord k) => Map k a -> k -> a
(!) m k = case lookup k m of
  Just x -> x
  Nothing -> error "Map.!: given key is not an element in the map"

findWithDefault :: (Ord k) => a -> k -> Map k a -> a
findWithDefault fallback k m = case lookup k m of
  Just x -> x
  Nothing -> fallback

member :: (Ord k) => k -> Map k a -> Bool
member k m = case lookup k m of
  Just _ -> True
  Nothing -> False

notMember :: (Ord k) => k -> Map k a -> Bool
notMember k m = not (member k m)

lookupLT :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupLT k = go Nothing
  where
    go best Tip = best
    go best (Bin _ ky y l r)
      | k <= ky = go best l
      | otherwise = go (Just (ky, y)) r

lookupGT :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupGT k = go Nothing
  where
    go best Tip = best
    go best (Bin _ ky y l r)
      | k < ky = go (Just (ky, y)) l
      | otherwise = go best r

lookupLE :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupLE k = go Nothing
  where
    go best Tip = best
    go best (Bin _ ky y l r) = case compare k ky of
      LT -> go best l
      EQ -> Just (ky, y)
      GT -> go (Just (ky, y)) r

lookupGE :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupGE k = go Nothing
  where
    go best Tip = best
    go best (Bin _ ky y l r) = case compare k ky of
      LT -> go (Just (ky, y)) l
      EQ -> Just (ky, y)
      GT -> go best r

null :: Map k a -> Bool
null = \case
  Tip -> True
  Bin {} -> False

size :: Map k a -> Int
size = \case
  Tip -> 0
  Bin count _ _ _ _ -> count

union :: (Ord k) => Map k a -> Map k a -> Map k a
union = unionWithKey (\_ x _ -> x)

unionWith :: (Ord k) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f = unionWithKey (\_ x y -> f x y)

unionWithKey :: (Ord k) => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey _ a Tip = a
unionWithKey _ Tip b = b
unionWithKey f (Bin _ k x l r) b = case splitLookup k b of
  (smaller, found, larger) -> link k value (unionWithKey f l smaller) (unionWithKey f r larger)
    where
      value = case found of
        Just y -> f k x y
        Nothing -> x

unions :: (Foldable f, Ord k) => f (Map k a) -> Map k a
unions = Prelude.foldl union Tip

unionsWith :: (Foldable f, Ord k) => (a -> a -> a) -> f (Map k a) -> Map k a
unionsWith f = Prelude.foldl (unionWith f) Tip

difference :: (Ord k) => Map k a -> Map k b -> Map k a
difference Tip _ = Tip
difference a Tip = a
difference a (Bin _ k _ l r) = case splitLookup k a of
  (smaller, _, larger) -> link2 (difference smaller l) (difference larger r)

(\\) :: (Ord k) => Map k a -> Map k b -> Map k a
(\\) = difference

differenceWith :: (Ord k) => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f = differenceWithKey (\_ x y -> f x y)

differenceWithKey :: (Ord k) => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey f a b = mapMaybeWithKey change a
  where
    change k x = case lookup k b of
      Just y -> f k x y
      Nothing -> Just x

intersection :: (Ord k) => Map k a -> Map k b -> Map k a
intersection = intersectionWithKey (\_ x _ -> x)

intersectionWith :: (Ord k) => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f = intersectionWithKey (\_ x y -> f x y)

intersectionWithKey :: (Ord k) => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey _ Tip _ = Tip
intersectionWithKey _ _ Tip = Tip
intersectionWithKey f (Bin _ k x l r) b = case splitLookup k b of
  (smaller, found, larger) -> case found of
    Just y -> link k (f k x y) (intersectionWithKey f l smaller) (intersectionWithKey f r larger)
    Nothing -> link2 (intersectionWithKey f l smaller) (intersectionWithKey f r larger)

disjoint :: (Ord k) => Map k a -> Map k b -> Bool
disjoint a b = Prelude.all (\k -> notMember k b) (keys a)

compose :: (Ord b) => Map b c -> Map a b -> Map a c
compose = placeholder

mergeWithKey ::
  (Ord k) =>
  (k -> a -> b -> Maybe c) ->
  (Map k a -> Map k c) ->
  (Map k b -> Map k c) ->
  Map k a ->
  Map k b ->
  Map k c
mergeWithKey = placeholder

map :: (a -> b) -> Map k a -> Map k b
map f = mapWithKey (\_ x -> f x)

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f = \case
  Tip -> Tip
  Bin count k x l r -> Bin count k (f k x) (mapWithKey f l) (mapWithKey f r)

traverseWithKey :: (Applicative t) => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey f = \case
  Tip -> pure Tip
  Bin count k x l r ->
    (\l' x' r' -> Bin count k x' l' r') <$> traverseWithKey f l <*> f k x <*> traverseWithKey f r

traverseMaybeWithKey :: (Applicative f) => (k -> a -> f (Maybe b)) -> Map k a -> f (Map k b)
traverseMaybeWithKey = placeholder

mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccum f = mapAccumWithKey (\state _ x -> f state x)

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey f state = \case
  Tip -> (state, Tip)
  Bin count k x l r -> case mapAccumWithKey f state l of
    (state1, l') -> case f state1 k x of
      (state2, x') -> case mapAccumWithKey f state2 r of
        (state3, r') -> (state3, Bin count k x' l' r')

mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumRWithKey f state = \case
  Tip -> (state, Tip)
  Bin count k x l r -> case mapAccumRWithKey f state r of
    (state1, r') -> case f state1 k x of
      (state2, x') -> case mapAccumRWithKey f state2 l of
        (state3, l') -> (state3, Bin count k x' l' r')

mapKeys :: (Ord k2) => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys f m = fromList [(f k, x) | (k, x) <- toAscList m]

mapKeysWith :: (Ord k2) => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith combine f m = fromListWith combine [(f k, x) | (k, x) <- toAscList m]

mapKeysMonotonic :: (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic f = \case
  Tip -> Tip
  Bin count k x l r -> Bin count (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr f = foldrWithKey (\_ x rest -> f x rest)

foldl :: (a -> b -> a) -> a -> Map k b -> a
foldl f = foldlWithKey (\rest _ x -> f rest x)

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z = \case
  Tip -> z
  Bin _ k x l r -> foldrWithKey f (f k x (foldrWithKey f z r)) l

foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey f z = \case
  Tip -> z
  Bin _ k x l r -> foldlWithKey f (f (foldlWithKey f z l) k x) r

foldMapWithKey :: (Monoid m) => (k -> a -> m) -> Map k a -> m
foldMapWithKey f = foldrWithKey (\k x rest -> f k x <> rest) mempty

foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldr' f = foldrWithKey' (\_ x rest -> f x rest)

foldl' :: (a -> b -> a) -> a -> Map k b -> a
foldl' f = foldlWithKey' (\rest _ x -> f rest x)

foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' f z = \case
  Tip -> z
  Bin _ k x l r ->
    let right = foldrWithKey' f z r
     in right `seq` (let here = f k x right in here `seq` foldrWithKey' f here l)

foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey' f z = \case
  Tip -> z
  Bin _ k x l r ->
    let left = foldlWithKey' f z l
     in left `seq` (let here = f left k x in here `seq` foldlWithKey' f here r)

elems :: Map k a -> [a]
elems = foldr (:) []

keys :: Map k a -> [k]
keys = foldrWithKey (\k _ rest -> k : rest) []

assocs :: Map k a -> [(k, a)]
assocs = toAscList

keysSet :: Map k a -> Set k
keysSet m = Set.fromDistinctAscList (keys m)

argSet :: Map k a -> Set (Arg k a)
argSet = placeholder

toList :: Map k a -> [(k, a)]
toList = toAscList

toAscList :: Map k a -> [(k, a)]
toAscList = foldrWithKey (\k x rest -> (k, x) : rest) []

toDescList :: Map k a -> [(k, a)]
toDescList = foldlWithKey (\rest k x -> (k, x) : rest) []

filter :: (a -> Bool) -> Map k a -> Map k a
filter p = filterWithKey (\_ x -> p x)

filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey p = \case
  Tip -> Tip
  Bin _ k x l r
    | p k x -> link k x (filterWithKey p l) (filterWithKey p r)
    | otherwise -> link2 (filterWithKey p l) (filterWithKey p r)

restrictKeys :: (Ord k) => Map k a -> Set k -> Map k a
restrictKeys m set = filterWithKey (\k _ -> Set.member k set) m

withoutKeys :: (Ord k) => Map k a -> Set k -> Map k a
withoutKeys m set = filterWithKey (\k _ -> Set.notMember k set) m

partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a)
partition p = partitionWithKey (\_ x -> p x)

partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey p m = (filterWithKey p m, filterWithKey (\k x -> not (p k x)) m)

takeWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
takeWhileAntitone = placeholder

dropWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
dropWhileAntitone = placeholder

spanAntitone :: (k -> Bool) -> Map k a -> (Map k a, Map k a)
spanAntitone = placeholder

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey f = \case
  Tip -> Tip
  Bin _ k x l r -> case f k x of
    Just y -> link k y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
    Nothing -> link2 (mapMaybeWithKey f l) (mapMaybeWithKey f r)

mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f = mapEitherWithKey (\_ x -> f x)

mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey f m = (mapMaybeWithKey left m, mapMaybeWithKey right m)
  where
    left k x = case f k x of
      Left y -> Just y
      Right _ -> Nothing
    right k x = case f k x of
      Left _ -> Nothing
      Right y -> Just y

split :: (Ord k) => k -> Map k a -> (Map k a, Map k a)
split k m = case splitLookup k m of
  (smaller, _, larger) -> (smaller, larger)

splitLookup :: (Ord k) => k -> Map k a -> (Map k a, Maybe a, Map k a)
splitLookup k = \case
  Tip -> (Tip, Nothing, Tip)
  Bin _ ky y l r -> case compare k ky of
    LT -> case splitLookup k l of
      (smaller, found, larger) -> (smaller, found, link ky y larger r)
    GT -> case splitLookup k r of
      (smaller, found, larger) -> (link ky y l smaller, found, larger)
    EQ -> (l, Just y, r)

splitRoot :: Map k b -> [Map k b]
splitRoot = placeholder

isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf = isSubmapOfBy (==)

isSubmapOfBy :: (Ord k) => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy same a b = size a <= size b && Prelude.all inside (toAscList a)
  where
    inside (k, x) = case lookup k b of
      Just y -> same x y
      Nothing -> False

isProperSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf = placeholder

isProperSubmapOfBy :: (Ord k) => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy = placeholder

lookupIndex :: (Ord k) => k -> Map k a -> Maybe Int
lookupIndex k = go 0
  where
    go _ Tip = Nothing
    go before (Bin _ ky _ l r) = case compare k ky of
      LT -> go before l
      GT -> go (before + size l + 1) r
      EQ -> Just (before + size l)

findIndex :: (Ord k) => k -> Map k a -> Int
findIndex k m = case lookupIndex k m of
  Just index -> index
  Nothing -> error "Map.findIndex: element is not in the map"

elemAt :: Int -> Map k a -> (k, a)
elemAt index = \case
  Tip -> error "Map.elemAt: index out of range"
  Bin _ k x l r -> case compare index (size l) of
    LT -> elemAt index l
    GT -> elemAt (index - size l - 1) r
    EQ -> (k, x)

updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateAt = placeholder

deleteAt :: Int -> Map k a -> Map k a
deleteAt index = \case
  Tip -> error "Map.deleteAt: index out of range"
  Bin _ k x l r -> case compare index (size l) of
    LT -> balance k x (deleteAt index l) r
    GT -> balance k x l (deleteAt (index - size l - 1) r)
    EQ -> glue l r

take :: Int -> Map k a -> Map k a
take count m = fromDistinctAscList (Prelude.take count (toAscList m))

drop :: Int -> Map k a -> Map k a
drop count m = fromDistinctAscList (Prelude.drop count (toAscList m))

splitAt :: Int -> Map k a -> (Map k a, Map k a)
splitAt count m = (take count m, drop count m)

lookupMin :: Map k a -> Maybe (k, a)
lookupMin = \case
  Tip -> Nothing
  Bin _ k x Tip _ -> Just (k, x)
  Bin _ _ _ l _ -> lookupMin l

lookupMax :: Map k a -> Maybe (k, a)
lookupMax = \case
  Tip -> Nothing
  Bin _ k x _ Tip -> Just (k, x)
  Bin _ _ _ _ r -> lookupMax r

findMin :: Map k a -> (k, a)
findMin m = case lookupMin m of
  Just found -> found
  Nothing -> error "Map.findMin: empty map has no minimal element"

findMax :: Map k a -> (k, a)
findMax m = case lookupMax m of
  Just found -> found
  Nothing -> error "Map.findMax: empty map has no maximal element"

deleteMin :: Map k a -> Map k a
deleteMin m = case minViewWithKey m of
  Just (_, rest) -> rest
  Nothing -> Tip

deleteMax :: Map k a -> Map k a
deleteMax m = case maxViewWithKey m of
  Just (_, rest) -> rest
  Nothing -> Tip

deleteFindMin :: Map k a -> ((k, a), Map k a)
deleteFindMin m = case minViewWithKey m of
  Just found -> found
  Nothing -> error "Map.deleteFindMin: can not return the minimal element of an empty map"

deleteFindMax :: Map k a -> ((k, a), Map k a)
deleteFindMax m = case maxViewWithKey m of
  Just found -> found
  Nothing -> error "Map.deleteFindMax: can not return the maximal element of an empty map"

updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin = placeholder

updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax = placeholder

updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey = placeholder

updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey = placeholder

minView :: Map k a -> Maybe (a, Map k a)
minView m = case minViewWithKey m of
  Just ((_, x), rest) -> Just (x, rest)
  Nothing -> Nothing

maxView :: Map k a -> Maybe (a, Map k a)
maxView m = case maxViewWithKey m of
  Just ((_, x), rest) -> Just (x, rest)
  Nothing -> Nothing

minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
minViewWithKey = \case
  Tip -> Nothing
  Bin _ k x Tip r -> Just ((k, x), r)
  Bin _ k x l r -> case minViewWithKey l of
    Just (least, rest) -> Just (least, balance k x rest r)
    Nothing -> Nothing

maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
maxViewWithKey = \case
  Tip -> Nothing
  Bin _ k x l Tip -> Just ((k, x), l)
  Bin _ k x l r -> case maxViewWithKey r of
    Just (most, rest) -> Just (most, balance k x l rest)
    Nothing -> Nothing

valid :: (Ord k) => Map k a -> Bool
valid = placeholder

-- One entry of each run of entries next to each other with equal keys,
-- its value the run's values combined.
adjacent :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> [(k, a)]
adjacent f ((k1, x1) : (k2, x2) : rest)
  | k1 == k2 = adjacent f ((k1, f k1 x2 x1) : rest)
  | otherwise = (k1, x1) : adjacent f ((k2, x2) : rest)
adjacent _ rest = rest

-- A tree of the first so many entries of a list in order, and the rest of
-- the list.
build :: Int -> [(k, a)] -> (Map k a, [(k, a)])
build 0 xs = (Tip, xs)
build count xs = case build (count `div` 2) xs of
  (l, (k, x) : rest) -> case build (count - count `div` 2 - 1) rest of
    (r, others) -> (bin k x l r, others)
  (l, []) -> (l, [])

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r = Bin (size l + size r + 1) k x l r

-- A tree of an entry and two trees, one of which may be one entry out of
-- balance: it had one put in or taken out.
balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | size l + size r <= 1 = bin k x l r
  | size r > 3 * size l = case r of
      Bin _ ky y ly ry
        | size ly < 2 * size ry -> bin ky y (bin k x l ly) ry
        | Bin _ kz z lz rz <- ly -> bin kz z (bin k x l lz) (bin ky y rz ry)
      _ -> bin k x l r
  | size l > 3 * size r = case l of
      Bin _ ky y ly ry
        | size ry < 2 * size ly -> bin ky y ly (bin k x ry r)
        | Bin _ kz z lz rz <- ry -> bin kz z (bin ky y ly lz) (bin k x rz r)
      _ -> bin k x l r
  | otherwise = bin k x l r

-- A tree of an entry and two trees of any sizes, all of one below the
-- entry and all of the other above.
link :: k -> a -> Map k a -> Map k a -> Map k a
link k x Tip r = insertMin k x r
link k x l Tip = insertMax k x l
link k x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | 3 * sizeL < sizeR = balance kz z (link k x l lz) rz
  | 3 * sizeR < sizeL = balance ky y ly (link k x ry r)
  | otherwise = bin k x l r

insertMin, insertMax :: k -> a -> Map k a -> Map k a
insertMin k x = \case
  Tip -> singleton k x
  Bin _ ky y l r -> balance ky y (insertMin k x l) r

insertMax k x = \case
  Tip -> singleton k x
  Bin _ ky y l r -> balance ky y l (insertMax k x r)

-- Two trees of any sizes as one, all of the first below the second.
link2 :: Map k a -> Map k a -> Map k a
link2 Tip r = r
link2 l Tip = l
link2 l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | 3 * sizeL < sizeR = balance ky y (link2 l ly) ry
  | 3 * sizeR < sizeL = balance kx x lx (link2 rx r)
  | otherwise = glue l r

-- Two trees in balance with each other as one.
glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = case maxViewWithKey l of
      Just ((k, x), rest) -> balance k x rest r
      Nothing -> r
  | otherwise = case minViewWithKey r of
      Just ((k, x), rest) -> balance k x l rest
      Nothing -> l
