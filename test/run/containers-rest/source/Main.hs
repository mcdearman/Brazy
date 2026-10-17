module Main where

import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map
import Data.Semigroup (Arg (..))
import qualified Data.Set as Set

-- The less used functions of Data.Map and Data.Set.

main :: IO ()
main = do
  let m = Map.fromList [(k, k * 10) | k <- [1 .. 10 :: Int]]
      n = Map.fromList [(k, show k) | k <- [5, 7 .. 15 :: Int]]
  print (Map.mergeWithKey (\_ a b -> if even a then Just (show a ++ b) else Nothing) (Map.map show) (const Map.empty) m n)
  print (Map.compose n (Map.fromList [('a', 5), ('b', 6), ('c', 9 :: Int)]))
  print (runIdentity (Map.traverseMaybeWithKey (\k v -> Identity (if odd k then Just (v + 1) else Nothing)) m))
  print (Map.takeWhileAntitone (< 4) m, Map.dropWhileAntitone (< 8) m, Map.spanAntitone (<= 2) (Map.take 4 m))
  print (concatMap Map.toList (Map.splitRoot (Map.take 3 m)) == Map.toList (Map.take 3 m))
  print (Map.isProperSubmapOf (Map.take 3 m) m, Map.isProperSubmapOf m m, Map.isProperSubmapOfBy (<=) (Map.take 2 m) (Map.map (+ 1) m))
  print (Map.updateAt (\_ v -> Just (v * 2)) 0 m Map.! 1, Map.size (Map.updateAt (\_ _ -> Nothing) 4 m), Map.valid (Map.updateAt (\_ _ -> Nothing) 4 m))
  print (Map.lookupMin (Map.updateMin (const Nothing) m), Map.lookupMax (Map.updateMax (Just . negate) m), Map.updateMinWithKey (\k v -> Just (k + v)) (Map.take 2 m))
  print (Map.argSet (Map.take 3 m), Map.fromArgSet (Set.fromList [Arg 'x' True, Arg 'y' False]))
  print (Map.valid m, Map.valid (foldr Map.delete m [2, 4, 6]))
  let s = Set.fromList "hello world"
  print (Set.powerSet (Set.fromList [1, 2, 3 :: Int]))
  print (Set.alterF (\present -> [present, not present]) 'h' s)
  print (Set.disjointUnion (Set.fromList "ab") (Set.fromList [1, 2 :: Int]))
  print (Set.takeWhileAntitone (< 'l') s, Set.dropWhileAntitone (< 'l') s, Set.spanAntitone (< 'e') s)
  print (concatMap Set.toList (Set.splitRoot s) == Set.toList s, Set.valid s, Set.valid (Set.map succ s))
