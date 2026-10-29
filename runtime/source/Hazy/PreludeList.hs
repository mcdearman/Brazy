module Hazy.PreludeList where

import Hazy.Helper (augment, build, listFoldr)
import Hazy.Prelude

infixl 9 !!

infixr 5 ++

infix 4 `notElem`

-- The functions that make lists are written with 'build' and 'augment', and
-- go through lists with 'listFoldr': what one makes and another goes
-- through at once is never made (see "Hazy.Helper").

map :: (a -> b) -> [a] -> [b]
map f xs = build (\c n -> listFoldr (\x rest -> c (f x) rest) n xs)

(++) :: [a] -> [a] -> [a]
xs ++ ys = augment (\c n -> listFoldr c n xs) ys

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = build (\c n -> listFoldr (\x rest -> if p x then c x rest else rest) n xs)

concat :: (Foldable t) => t [a] -> [a]
concat xss = build (\c n -> foldr (\xs rest -> listFoldr c rest xs) n xss)

concatMap :: (Foldable t) => (a -> [b]) -> t a -> [b]
concatMap f xs = build (\c n -> foldr (\x rest -> listFoldr c rest (f x)) n xs)

head :: [a] -> a
head (x : _) = x
head [] = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "Prelude.tail: empty list"

last :: [a] -> a
last [x] = x
last (_ : xs) = last xs
last [] = error "Prelude.last: empty list"

init :: [a] -> [a]
init [x] = []
init (x : xs) = x : init xs
init [] = error "Prelude.init: empty list"

(!!) :: [a] -> Int -> a
xs !! n | n < 0 = error "Prelude.!!: negative index"
[] !! _ = error "Prelude.!!: index too large"
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs =
  q
    : ( case xs of
          [] -> []
          x : xs -> scanl f (f q x) xs
      )

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x : xs) = scanl f x xs
scanl1 _ [] = []

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 [] = [q0]
scanr f q0 (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr f q0 xs

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f [] = []
scanr1 f [x] = [x]
scanr1 f (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr1 f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs where xs = x : xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = xs' where xs' = xs ++ xs'

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p xs@(x : xs')
  | p x = dropWhile p xs'
  | otherwise = xs

span, break :: (a -> Bool) -> [a] -> ([a], [a])
span p [] = ([], [])
span p xs@(x : xs')
  | p x = (x : ys, zs)
  | otherwise = ([], xs)
  where
    (ys, zs) = span p xs'
break p = span (not . p)

lines :: String -> [String]
lines "" = []
lines s =
  let (l, s') = break (== '\n') s
   in l : case s' of
        [] -> []
        (_ : s'') -> lines s''

words :: String -> [String]
words s = case dropWhile isSpace s of
  "" -> []
  s' -> w : words s''
    where
      (w, s'') = break isSpace s'

unlines :: [String] -> String
unlines = concatMap (++ "\n")

unwords :: [String] -> String
unwords [] = ""
unwords ws = foldr1 (\w s -> w ++ ' ' : s) ws

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and, or :: (Foldable t) => t Bool -> Bool
and = foldr (&&) True
or = foldr (||) False

any, all :: (Foldable t) => (a -> Bool) -> t a -> Bool
any p = or . map p . toList
all p = and . map p . toList

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x xs = not (elem x xs)

lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup key [] = Nothing
lookup key ((x, y) : xys)
  | key == x = Just y
  | otherwise = lookup key xys

zip :: [a] -> [b] -> [(a, b)]
zip as bs = zipWith (,) as bs

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = zipWith3 (,,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith z as bs = build (\c n -> listFoldr (step c n) (\_ -> n) as bs)
  where
    step c n a rest = \case
      b : bs -> c (z a b) (rest bs)
      [] -> n

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z as bs cs = build (\c n -> zipping c n as bs cs)
  where
    zipping c n = go
      where
        go (a : as) (b : bs) (d : ds) = c (z a b d) (go as bs ds)
        go _ _ _ = n

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a, b) ~(as, bs) -> (a : as, b : bs)) ([], [])

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 =
  foldr
    (\(a, b, c) ~(as, bs, cs) -> (a : as, b : bs, c : cs))
    ([], [], [])
