module Main where

-- A set or a map made from a list is made as the list is gone through, so
-- that nothing is held for each element until the end; what a lazy map
-- holds is still not worked out until it is asked for.

import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  let s = Set.fromList [x * x `mod` 1009 | x <- [1 .. 300000 :: Int]]
  print (Set.size s, Set.findMin s, Set.findMax s)
  print (Set.toList (Set.fromList "mississippi"))
  print (Set.toList (Set.unions [Set.fromList [3, 1], Set.fromList [2, 3 :: Int]]))
  print (Set.toList (Set.unions (Just (Set.fromList "ab"))))
  let m = Map.fromList [(k `mod` 97, k) | k <- [1 .. 300000 :: Int]]
  print (Map.size m, Map.lookup 5 m)
  -- What a lazy map holds is not worked out by making it.
  let lazy = Map.fromList [(1 :: Int, undefined), (2, 20 :: Int)]
  print (Map.size lazy, Map.lookup 2 lazy)
  print (Map.toList (Map.fromListWith (++) [(1 :: Int, "a"), (2, "b"), (1, "c")]))
  print (Map.toList (Map.unionsWith (+) [Map.fromList [(1 :: Int, 1 :: Int)], Map.fromList [(1, 2), (2, 3)]]))
