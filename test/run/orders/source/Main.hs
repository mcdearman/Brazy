module Main where

import Data.List (sort, sortBy, sortOn)
import qualified Data.Map as Map
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import qualified Data.Text as Text

data Kind = Global !Int !Int | Local !Int | Shift !Kind
  deriving (Eq, Ord, Show)

data Key = Index !Kind | Lifted !Int | Bool | Char | Tuple !Int | Every
  deriving (Eq, Ord, Show)

numbers :: [Int]
numbers = take 40 (iterate (\x -> (x * 7919 + 13) `mod` 101 - 50) 3)

main :: IO ()
main = do
  let pairs = [(a `mod` 5, b) | (a, b) <- zip numbers (drop 7 numbers)]
      triples = [(a `mod` 3, b `mod` 4, c) | (a, (b, c)) <- zip numbers (zip (drop 3 numbers) (drop 11 numbers))]
  print (sort pairs)
  print (sort triples)
  print [compare a b | a <- take 6 pairs, b <- take 6 pairs]
  print [(a < b, a <= b, a > b, a >= b, max a b, min a b) | a <- take 4 pairs, b <- take 4 pairs]
  print (sort [(Text.pack (show a), b) | (a, b) <- take 12 pairs])
  print (sortOn Down (take 12 numbers), sortBy (comparing snd) (take 10 pairs))
  let keys = [Index (Global (a `mod` 4) (b `mod` 6)) | (a, b) <- pairs] ++ [Lifted 3, Bool, Tuple 2, Every, Char, Index (Shift (Local 1)), Index (Local 2), Tuple 1]
  print (sort keys)
  print (Map.toList (Map.fromListWith (+) [((k, n `mod` 3), 1 :: Int) | (k, n) <- zip keys numbers]))
  print (Set.toList (Set.fromList (zip (map (`mod` 7) numbers) keys)))
  print (sort [[a, b] | (a, b) <- take 10 pairs], sort [Just a | a <- take 8 numbers] ++ [Nothing])
  print (sort [if even a then Left a else Right (show a) | a <- take 12 numbers])
  print (sort (map toInteger numbers), compare (2 ^ (40 :: Int) :: Integer) (2 ^ (41 :: Int)), sort [(1 :: Integer, 'b'), (1, 'a'), (0, 'z')])
  print (maximum pairs, minimum triples, compare (1 :: Int, 'a', "x") (1, 'a', "w"), (3 :: Int, 4 :: Int) == (3, 4))
