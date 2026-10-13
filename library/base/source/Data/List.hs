module Data.List
  ( (++),
    head,
    last,
    tail,
    init,
    uncons,
    singleton,
    null,
    length,
    map,
    reverse,
    intersperse,
    intercalate,
    transpose,
    subsequences,
    permutations,
    foldl,
    foldl,
    foldl1,
    foldl1,
    foldr,
    foldr1,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    sum,
    product,
    maximum,
    minimum,
    scanl,
    scanl,
    scanl1,
    scanr,
    scanr1,
    mapAccumL,
    mapAccumR,
    iterate,
    iterate',
    repeat,
    replicate,
    cycle,
    unfoldr,
    take,
    drop,
    splitAt,
    takeWhile,
    dropWhile,
    dropWhileEnd,
    span,
    break,
    stripPrefix,
    group,
    inits,
    tails,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,
    isSubsequenceOf,
    elem,
    notElem,
    lookup,
    find,
    filter,
    partition,
    (!!),
    elemIndex,
    elemIndices,
    findIndex,
    findIndices,
    zip,
    zip3,
    zip4,
    zip5,
    zip6,
    zip7,
    zipWith,
    zipWith3,
    zipWith4,
    zipWith5,
    zipWith6,
    zipWith7,
    unzip,
    unzip3,
    unzip4,
    unzip5,
    unzip6,
    unzip7,
    lines,
    words,
    unlines,
    unwords,
    nub,
    delete,
    (\\),
    union,
    intersect,
    sort,
    sortOn,
    insert,
    nubBy,
    deleteBy,
    deleteFirstsBy,
    unionBy,
    intersectBy,
    groupBy,
    sortBy,
    insertBy,
    maximumBy,
    minimumBy,
    genericLength,
    genericTake,
    genericDrop,
    genericSplitAt,
    genericIndex,
    genericReplicate,
  )
where

import Data.Foldable (find, maximumBy, minimumBy)
import Data.Traversable (mapAccumL, mapAccumR)
import Hazy.Prelude (placeholder)

uncons :: [a] -> Maybe (a, [a])
uncons = \case
  x : xs -> Just (x, xs)
  [] -> Nothing

singleton :: a -> [a]
singleton x = [x]

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse separator (x : xs) = x : go xs
  where
    go [] = []
    go (y : ys) = separator : y : go ys

intercalate :: [a] -> [[a]] -> [a]
intercalate separator xs = concat (intersperse separator xs)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : rows) = transpose rows
transpose ((x : xs) : rows) =
  (x : [h | h : _ <- rows]) : transpose (xs : [t | _ : t <- rows])

subsequences :: [a] -> [[a]]
subsequences = placeholder

permutations :: [a] -> [[a]]
permutations = placeholder

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl = placeholder

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' = placeholder

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 = placeholder

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr = placeholder

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 = placeholder

iterate' :: (a -> a) -> a -> [a]
iterate' = placeholder

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f seed = case f seed of
  Just (a, next) -> a : unfoldr f next
  Nothing -> []

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x : xs) (y : ys) | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

group :: (Eq a) => [a] -> [[a]]
group = groupBy (==)

inits :: [a] -> [[a]]
inits xs = [] : case xs of
  [] -> []
  y : ys -> map (y :) (inits ys)

tails :: [a] -> [[a]]
tails xs = xs : case xs of
  [] -> []
  _ : ys -> tails ys

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf = placeholder

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldr (\x both -> if p x then (x : fst both, snd both) else (fst both, x : snd both)) ([], [])

elemIndex :: (Eq a) => a -> [a] -> Maybe Int
elemIndex x = findIndex (== x)

elemIndices :: (Eq a) => a -> [a] -> [Int]
elemIndices = placeholder

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = case [index | (index, x) <- zip [0 ..] xs, p x] of
  index : _ -> Just index
  [] -> Nothing

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices = placeholder

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 = zipWith4 (\a b c d -> (a, b, c, d))

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 = placeholder

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 = placeholder

zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zip7 = placeholder

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zipWith4 f as bs cs ds
zipWith4 _ _ _ _ _ = []

zipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith5 = placeholder

zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith6 = placeholder

zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
zipWith7 = placeholder

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 = placeholder

unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip5 = placeholder

unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip6 = placeholder

unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
unzip7 = placeholder

nub :: (Eq a) => [a] -> [a]
nub = nubBy (==)

delete :: (Eq a) => a -> [a] -> [a]
delete = deleteBy (==)

infix 5 \\

(\\) :: (Eq a) => [a] -> [a] -> [a]
xs \\ ys = foldl (\rest y -> delete y rest) xs ys

union :: (Eq a) => [a] -> [a] -> [a]
union = unionBy (==)

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect = intersectBy (==)

sort :: (Ord a) => [a] -> [a]
sort = sortBy compare

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (\a b -> compare (fst a) (fst b)) . map (\x -> let key = f x in key `seq` (key, x))

insert :: (Ord a) => a -> [a] -> [a]
insert = insertBy compare

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy equal = go []
  where
    go _ [] = []
    go seen (x : xs)
      | any (\earlier -> equal earlier x) seen = go seen xs
      | otherwise = x : go (x : seen) xs

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy equal x (y : ys) = if equal x y then ys else y : deleteBy equal x ys

deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy = placeholder

unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy equal xs ys = xs ++ foldl (\rest x -> deleteBy equal x rest) (nubBy equal ys) xs

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy equal xs ys = [x | x <- xs, any (equal x) ys]

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy equal (x : xs) = (x : same) : groupBy equal others
  where
    (same, others) = span (equal x) xs

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy order = mergeAll . runs
  where
    -- The list is cut into runs that are in order already, going up or
    -- down, and the runs are merged two by two.
    runs (a : b : xs)
      | order a b == GT = descending b [a] xs
      | otherwise = ascending b (a :) xs
    runs xs = [xs]
    descending a as (b : bs)
      | order a b == GT = descending b (a : as) bs
    descending a as bs = (a : as) : runs bs
    ascending a as (b : bs)
      | order a b /= GT = ascending b (\ys -> as (a : ys)) bs
    ascending a as bs = as [a] : runs bs
    mergeAll [x] = x
    mergeAll xs = mergeAll (mergePairs xs)
    mergePairs (a : b : xs) = merge a b : mergePairs xs
    mergePairs xs = xs
    merge [] bs = bs
    merge as [] = as
    merge (a : as) (b : bs)
      | order a b == GT = b : merge (a : as) bs
      | otherwise = a : merge as (b : bs)

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy order x (y : ys) = case order x y of
  GT -> y : insertBy order x ys
  _ -> x : y : ys

genericLength :: (Num i) => [a] -> i
genericLength = foldr (\_ n -> n + 1) 0

genericTake :: (Integral i) => i -> [a] -> [a]
genericTake = placeholder

genericDrop :: (Integral i) => i -> [a] -> [a]
genericDrop = placeholder

genericSplitAt :: (Integral i) => i -> [a] -> ([a], [a])
genericSplitAt = placeholder

genericIndex :: (Integral i) => [a] -> i -> a
genericIndex = placeholder

genericReplicate :: (Integral i) => i -> a -> [a]
genericReplicate = placeholder
