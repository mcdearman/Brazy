module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Functions that call themselves and are given an instance that is known,
-- each copied for it.

-- A function of a class of its own, at two types.
largest :: (Ord a) => a -> [a] -> a
largest best [] = best
largest best (x : xs)
  | x > best = largest x xs
  | otherwise = largest best xs

-- One that passes its instance on and something else as well.
count :: (Eq a) => a -> [a] -> Int -> Int
count _ [] seen = seen
count wanted (x : xs) seen
  | x == wanted = count wanted xs (seen + 1)
  | otherwise = count wanted xs seen

-- One that gives itself another instance, a list of what it was given: it
-- cannot be copied for the first.
nested :: (Show a) => Int -> a -> String
nested 0 x = show x
nested n x = nested (n - 1) [x]

newtype Box a = Box a

-- An instance that needs another, which only builds the instance.
instance (Show a) => Show (Box a) where
  show (Box x) = "Box " ++ show x

main :: IO ()
main = do
  let table = foldr (\k acc -> Map.insert k (k * k) acc) Map.empty [1 .. 200 :: Int]
  print (Map.lookup 12 table, Map.lookup 500 table, Map.size table)
  let words' = Set.fromList "the quick brown fox jumps over the lazy dog"
  print (Set.toList words', Set.member 'q' words', Set.member 'z' words')
  print (sum [1 .. 100 :: Int], product [1 .. 10 :: Int], elem (3 :: Int) [1, 2, 3])
  print (largest 0 [3, 9, 2, 7 :: Int], largest 0.5 [0.25, 1.5, 0.75 :: Double])
  print (largest 'a' "copies", count 'e' "the essence" 0, count (2 :: Int) [1, 2, 2, 3, 2] 0)
  putStrLn (nested 3 (5 :: Int))
  print (Box (Box [1, 2 :: Int]))
  print (Map.toList (Map.unionWith (+) table (Map.fromList [(1, 100), (300, 3)])) !! 0)
