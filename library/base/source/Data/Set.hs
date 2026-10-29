module Data.Set
  ( Set,
    empty,
    singleton,
    fromList,
    fromAscList,
    fromDescList,
    fromDistinctAscList,
    fromDistinctDescList,
    powerSet,
    insert,
    delete,
    alterF,
    member,
    notMember,
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,
    null,
    size,
    isSubsetOf,
    isProperSubsetOf,
    disjoint,
    union,
    unions,
    difference,
    (\\),
    intersection,
    cartesianProduct,
    disjointUnion,
    filter,
    takeWhileAntitone,
    dropWhileAntitone,
    spanAntitone,
    partition,
    split,
    splitMember,
    splitRoot,
    lookupIndex,
    findIndex,
    elemAt,
    deleteAt,
    take,
    drop,
    splitAt,
    map,
    mapMonotonic,
    foldr,
    foldl,
    foldr,
    foldl,
    fold,
    lookupMin,
    lookupMax,
    findMin,
    findMax,
    deleteMin,
    deleteMax,
    deleteFindMin,
    deleteFindMax,
    maxView,
    minView,
    elems,
    toList,
    toAscList,
    toDescList,
    showTree,
    showTreeWith,
    valid,
  )
where

import Prelude hiding (drop, filter, foldl, foldr, map, null, splitAt, take)
import qualified Data.Foldable as Foldable (foldl')
import qualified Prelude

-- A set is a tree balanced by weight: neither side of a node has more than
-- three times the other's elements.
data Set a
  = Bin !Int !a !(Set a) !(Set a)
  | Tip

instance Foldable Set where
  foldr = foldr
  foldl = foldl
  length = size
  null = null
  elem x = Prelude.elem x . toAscList

instance (Eq a) => Eq (Set a) where
  a == b = size a == size b && toAscList a == toAscList b

instance (Ord a) => Ord (Set a) where
  compare a b = compare (toAscList a) (toAscList b)

instance (Show a) => Show (Set a) where
  showsPrec d set = showParen (d > 10) (showString "fromList " . shows (toAscList set))

instance (Ord a) => Semigroup (Set a) where
  (<>) = union

instance (Ord a) => Monoid (Set a) where
  mempty = Tip

empty :: Set a
empty = Tip

singleton :: a -> Set a
singleton x = Bin 1 x Tip Tip

fromList :: (Ord a) => [a] -> Set a
fromList = Foldable.foldl' (\set x -> insert x set) Tip

fromAscList :: (Eq a) => [a] -> Set a
fromAscList = fromDistinctAscList . adjacent

fromDescList :: (Eq a) => [a] -> Set a
fromDescList = fromDistinctAscList . Prelude.reverse . adjacent

fromDistinctAscList :: [a] -> Set a
fromDistinctAscList xs = Prelude.fst (build (Prelude.length xs) xs)

fromDistinctDescList :: [a] -> Set a
fromDistinctDescList = fromDistinctAscList . Prelude.reverse

powerSet :: Set a -> Set (Set a)
-- Every subset, in order: those of the first element, with it and then
-- without it.
powerSet set = fromDistinctAscList (Prelude.map fromDistinctAscList (subsets (toAscList set)))
  where
    subsets [] = [[]]
    subsets (x : xs) = [] : Prelude.map (x :) (subsets xs) ++ Prelude.drop 1 (subsets xs)

insert :: (Ord a) => a -> Set a -> Set a
insert x = \case
  Tip -> singleton x
  Bin count y l r -> case compare x y of
    LT -> balance y (insert x l) r
    GT -> balance y l (insert x r)
    EQ -> Bin count x l r

delete :: (Ord a) => a -> Set a -> Set a
delete x = \case
  Tip -> Tip
  Bin _ y l r -> case compare x y of
    LT -> balance y (delete x l) r
    GT -> balance y l (delete x r)
    EQ -> glue l r

alterF :: (Ord a, Functor f) => (Bool -> f Bool) -> a -> Set a -> f (Set a)
alterF f x set = (\keep -> if keep then insert x set else delete x set) <$> f (member x set)

member :: (Ord a) => a -> Set a -> Bool
member x = \case
  Tip -> False
  Bin _ y l r -> case compare x y of
    LT -> member x l
    GT -> member x r
    EQ -> True

notMember :: (Ord a) => a -> Set a -> Bool
notMember x set = not (member x set)

lookupLT :: (Ord a) => a -> Set a -> Maybe a
lookupLT x = go Nothing
  where
    go best Tip = best
    go best (Bin _ y l r)
      | x <= y = go best l
      | otherwise = go (Just y) r

lookupGT :: (Ord a) => a -> Set a -> Maybe a
lookupGT x = go Nothing
  where
    go best Tip = best
    go best (Bin _ y l r)
      | x < y = go (Just y) l
      | otherwise = go best r

lookupLE :: (Ord a) => a -> Set a -> Maybe a
lookupLE x = go Nothing
  where
    go best Tip = best
    go best (Bin _ y l r) = case compare x y of
      LT -> go best l
      EQ -> Just y
      GT -> go (Just y) r

lookupGE :: (Ord a) => a -> Set a -> Maybe a
lookupGE x = go Nothing
  where
    go best Tip = best
    go best (Bin _ y l r) = case compare x y of
      LT -> go (Just y) l
      EQ -> Just y
      GT -> go best r

null :: Set a -> Bool
null = \case
  Tip -> True
  Bin {} -> False

size :: Set a -> Int
size = \case
  Tip -> 0
  Bin count _ _ _ -> count

isSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isSubsetOf a b = size a <= size b && Prelude.all (\x -> member x b) (toAscList a)

isProperSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isProperSubsetOf a b = size a < size b && isSubsetOf a b

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint a b = null (intersection a b)

union :: (Ord a) => Set a -> Set a -> Set a
union a Tip = a
union Tip b = b
union a@(Bin _ x l r) b = case splitMember x b of
  (smaller, _, larger) -> link x (union l smaller) (union r larger)

unions :: (Foldable f, Ord a) => f (Set a) -> Set a
unions = Foldable.foldl' union Tip

difference :: (Ord a) => Set a -> Set a -> Set a
difference Tip _ = Tip
difference a Tip = a
difference a (Bin _ x l r) = case splitMember x a of
  (smaller, _, larger) -> link2 (difference smaller l) (difference larger r)

(\\) :: (Ord a) => Set a -> Set a -> Set a
(\\) = difference

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection (Bin _ x l r) b = case splitMember x b of
  (smaller, found, larger)
    | found -> link x (intersection l smaller) (intersection r larger)
    | otherwise -> link2 (intersection l smaller) (intersection r larger)

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct a b = fromDistinctAscList [(x, y) | x <- toAscList a, y <- toAscList b]

disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion as bs = fromDistinctAscList (Prelude.map Left (toAscList as) ++ Prelude.map Right (toAscList bs))

filter :: (a -> Bool) -> Set a -> Set a
filter p = \case
  Tip -> Tip
  Bin _ x l r
    | p x -> link x (filter p l) (filter p r)
    | otherwise -> link2 (filter p l) (filter p r)

takeWhileAntitone :: (a -> Bool) -> Set a -> Set a
takeWhileAntitone _ Tip = Tip
takeWhileAntitone p (Bin _ x l r)
  | p x = link x l (takeWhileAntitone p r)
  | otherwise = takeWhileAntitone p l

dropWhileAntitone :: (a -> Bool) -> Set a -> Set a
dropWhileAntitone _ Tip = Tip
dropWhileAntitone p (Bin _ x l r)
  | p x = dropWhileAntitone p r
  | otherwise = link x (dropWhileAntitone p l) r

spanAntitone :: (a -> Bool) -> Set a -> (Set a, Set a)
spanAntitone p set = (takeWhileAntitone p set, dropWhileAntitone p set)

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition p set = (filter p set, filter (not . p) set)

split :: (Ord a) => a -> Set a -> (Set a, Set a)
split x set = case splitMember x set of
  (smaller, _, larger) -> (smaller, larger)

splitMember :: (Ord a) => a -> Set a -> (Set a, Bool, Set a)
splitMember x = \case
  Tip -> (Tip, False, Tip)
  Bin _ y l r -> case compare x y of
    LT -> case splitMember x l of
      (smaller, found, larger) -> (smaller, found, link y larger r)
    GT -> case splitMember x r of
      (smaller, found, larger) -> (link y l smaller, found, larger)
    EQ -> (l, True, r)

splitRoot :: Set a -> [Set a]
splitRoot Tip = []
splitRoot (Bin _ x l r) = [l, singleton x, r]

lookupIndex :: (Ord a) => a -> Set a -> Maybe Int
lookupIndex x = go 0
  where
    go _ Tip = Nothing
    go before (Bin _ y l r) = case compare x y of
      LT -> go before l
      GT -> go (before + size l + 1) r
      EQ -> Just (before + size l)

findIndex :: (Ord a) => a -> Set a -> Int
findIndex x set = case lookupIndex x set of
  Just index -> index
  Nothing -> error "Set.findIndex: element is not in the set"

elemAt :: Int -> Set a -> a
elemAt index = \case
  Tip -> error "Set.elemAt: index out of range"
  Bin _ x l r -> case compare index (size l) of
    LT -> elemAt index l
    GT -> elemAt (index - size l - 1) r
    EQ -> x

deleteAt :: Int -> Set a -> Set a
deleteAt index = \case
  Tip -> error "Set.deleteAt: index out of range"
  Bin _ x l r -> case compare index (size l) of
    LT -> balance x (deleteAt index l) r
    GT -> balance x l (deleteAt (index - size l - 1) r)
    EQ -> glue l r

take :: Int -> Set a -> Set a
take count set = fromDistinctAscList (Prelude.take count (toAscList set))

drop :: Int -> Set a -> Set a
drop count set = fromDistinctAscList (Prelude.drop count (toAscList set))

splitAt :: Int -> Set a -> (Set a, Set a)
splitAt count set = (take count set, drop count set)

map :: (Ord b) => (a -> b) -> Set a -> Set b
map f set = fromList (Prelude.map f (toAscList set))

mapMonotonic :: (a -> b) -> Set a -> Set b
mapMonotonic f = \case
  Tip -> Tip
  Bin count x l r -> Bin count (f x) (mapMonotonic f l) (mapMonotonic f r)

foldr :: (a -> b -> b) -> b -> Set a -> b
foldr f z = \case
  Tip -> z
  Bin _ x l r -> foldr f (f x (foldr f z r)) l

foldl :: (a -> b -> a) -> a -> Set b -> a
foldl f z = \case
  Tip -> z
  Bin _ x l r -> foldl f (f (foldl f z l) x) r

foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' f z = \case
  Tip -> z
  Bin _ x l r -> let right = foldr' f z r in right `seq` (let here = f x right in here `seq` foldr' f here l)

foldl' :: (a -> b -> a) -> a -> Set b -> a
foldl' f z = \case
  Tip -> z
  Bin _ x l r -> let left = foldl' f z l in left `seq` (let here = f left x in here `seq` foldl' f here r)

fold :: (a -> b -> b) -> b -> Set a -> b
fold = foldr

lookupMin :: Set a -> Maybe a
lookupMin = \case
  Tip -> Nothing
  Bin _ x Tip _ -> Just x
  Bin _ _ l _ -> lookupMin l

lookupMax :: Set a -> Maybe a
lookupMax = \case
  Tip -> Nothing
  Bin _ x _ Tip -> Just x
  Bin _ _ _ r -> lookupMax r

findMin :: Set a -> a
findMin set = case lookupMin set of
  Just x -> x
  Nothing -> error "Set.findMin: empty set has no minimal element"

findMax :: Set a -> a
findMax set = case lookupMax set of
  Just x -> x
  Nothing -> error "Set.findMax: empty set has no maximal element"

deleteMin :: Set a -> Set a
deleteMin set = case minView set of
  Just (_, rest) -> rest
  Nothing -> Tip

deleteMax :: Set a -> Set a
deleteMax set = case maxView set of
  Just (_, rest) -> rest
  Nothing -> Tip

deleteFindMin :: Set a -> (a, Set a)
deleteFindMin set = case minView set of
  Just found -> found
  Nothing -> error "Set.deleteFindMin: can not return the minimal element of an empty set"

deleteFindMax :: Set a -> (a, Set a)
deleteFindMax set = case maxView set of
  Just found -> found
  Nothing -> error "Set.deleteFindMax: can not return the maximal element of an empty set"

maxView :: Set a -> Maybe (a, Set a)
maxView = \case
  Tip -> Nothing
  Bin _ x l Tip -> Just (x, l)
  Bin _ x l r -> case maxView r of
    Just (most, rest) -> Just (most, balance x l rest)
    Nothing -> Nothing

minView :: Set a -> Maybe (a, Set a)
minView = \case
  Tip -> Nothing
  Bin _ x Tip r -> Just (x, r)
  Bin _ x l r -> case minView l of
    Just (least, rest) -> Just (least, balance x rest r)
    Nothing -> Nothing

elems :: Set a -> [a]
elems = toAscList

toList :: Set a -> [a]
toList = toAscList

toAscList :: Set a -> [a]
toAscList = foldr (:) []

toDescList :: Set a -> [a]
toDescList = foldl (\xs x -> x : xs) []

showTree :: (Show a) => Set a -> String
showTree = showTreeWith True False

showTreeWith :: (Show a) => Bool -> Bool -> Set a -> String
-- The tree drawn as containers draws it: hanging, with the root first, or
-- on its side, with the larger elements above.
showTreeWith hang wide set
  | hang = hanging [] set ""
  | otherwise = sideways [] [] set ""
  where
    sideways lbars rbars = \case
      Tip -> bars lbars . showString "|\n"
      Bin _ x Tip Tip -> bars lbars . shows x . showString "\n"
      Bin _ x l r ->
        sideways (withBar rbars) (withEmpty rbars) r
          . gap rbars
          . bars lbars
          . shows x
          . showString "\n"
          . gap lbars
          . sideways (withEmpty lbars) (withBar lbars) l
    hanging marks = \case
      Tip -> bars marks . showString "|\n"
      Bin _ x Tip Tip -> bars marks . shows x . showString "\n"
      Bin _ x l r ->
        bars marks
          . shows x
          . showString "\n"
          . gap marks
          . hanging (withBar marks) l
          . gap marks
          . hanging (withEmpty marks) r
    gap marks
      | wide = showString (concat (reverse marks)) . showString "|\n"
      | otherwise = id
    bars marks = case marks of
      [] -> id
      _ : rest -> showString (concat (reverse rest)) . showString "+--"
    withBar marks = "|  " : marks
    withEmpty marks = "   " : marks

valid :: (Ord a) => Set a -> Bool
-- | Whether the tree is as a set's must be: in order, of the sizes it says,
-- and balanced.
valid set = ordered (toAscList set) && sized set && balanced set
  where
    ordered xs = and (Prelude.zipWith (<) xs (Prelude.drop 1 xs))
    sized Tip = True
    sized (Bin count _ l r) = count == size l + size r + 1 && sized l && sized r
    balanced Tip = True
    balanced (Bin _ _ l r) =
      (size l + size r <= 1 || (size l <= 3 * size r && size r <= 3 * size l))
        && balanced l
        && balanced r

-- One of each run of equal elements that are next to each other.
adjacent :: (Eq a) => [a] -> [a]
adjacent (x : y : rest)
  | x == y = adjacent (y : rest)
  | otherwise = x : adjacent (y : rest)
adjacent rest = rest

-- A tree of the first so many elements of a list in order, and the rest of
-- the list.
build :: Int -> [a] -> (Set a, [a])
build 0 xs = (Tip, xs)
build count xs = case build (count `div` 2) xs of
  (l, x : rest) -> case build (count - count `div` 2 - 1) rest of
    (r, others) -> (bin x l r, others)
  (l, []) -> (l, [])

bin :: a -> Set a -> Set a -> Set a
bin x l r = Bin (size l + size r + 1) x l r

-- A tree of an element and two trees, one of which may be one element
-- out of balance: it had one put in or taken out.
balance :: a -> Set a -> Set a -> Set a
balance x l r
  | size l + size r <= 1 = bin x l r
  | size r > 3 * size l = case r of
      Bin _ y ly ry
        | size ly < 2 * size ry -> bin y (bin x l ly) ry
        | Bin _ z lz rz <- ly -> bin z (bin x l lz) (bin y rz ry)
      _ -> bin x l r
  | size l > 3 * size r = case l of
      Bin _ y ly ry
        | size ry < 2 * size ly -> bin y ly (bin x ry r)
        | Bin _ z lz rz <- ry -> bin z (bin y ly lz) (bin x rz r)
      _ -> bin x l r
  | otherwise = bin x l r

-- A tree of an element and two trees of any sizes, all of one below the
-- element and all of the other above.
link :: a -> Set a -> Set a -> Set a
link x Tip r = insertMin x r
link x l Tip = insertMax x l
link x l@(Bin sizeL y ly ry) r@(Bin sizeR z lz rz)
  | 3 * sizeL < sizeR = balance z (link x l lz) rz
  | 3 * sizeR < sizeL = balance y ly (link x ry r)
  | otherwise = bin x l r

insertMin, insertMax :: a -> Set a -> Set a
insertMin x = \case
  Tip -> singleton x
  Bin _ y l r -> balance y (insertMin x l) r

insertMax x = \case
  Tip -> singleton x
  Bin _ y l r -> balance y l (insertMax x r)

-- Two trees of any sizes as one, all of the first below the second.
link2 :: Set a -> Set a -> Set a
link2 Tip r = r
link2 l Tip = l
link2 l@(Bin sizeL x lx rx) r@(Bin sizeR y ly ry)
  | 3 * sizeL < sizeR = balance y (link2 l ly) ry
  | 3 * sizeR < sizeL = balance x lx (link2 rx r)
  | otherwise = glue l r

-- Two trees in balance with each other as one.
glue :: Set a -> Set a -> Set a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = case maxView l of
      Just (most, rest) -> balance most rest r
      Nothing -> r
  | otherwise = case minView r of
      Just (least, rest) -> balance least l rest
      Nothing -> l
