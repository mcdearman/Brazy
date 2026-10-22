module Main where

import Data.Foldable (foldl')
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map

-- Going through lists from the left: each a loop with what has been gathered
-- as an argument, whether the list is made there or given.

-- What is gathered depends on something the function is given.
scaled :: Int -> [Int] -> Int
scaled factor xs = foldl' (\total x -> total + factor * x) 0 xs

-- One function written there refers to another.
weighted :: Int -> Int -> Int
weighted low high =
  let weight x = x * x `mod` 7
      step total x = total + weight x
   in foldl' step 0 [low .. high]

-- What is gathered is a pair, and the list is a comprehension.
spread :: [Int] -> (Int, Int)
spread xs = foldl' (\(low, high) x -> (min low x, max high x)) (1000, -1000) [x * 3 | x <- xs, odd x]

-- A fold from the right that stops at the first big number goes no further.
firstBig :: [Int] -> Int
firstBig = foldr (\x rest -> if x > 100 then x else rest) 0

-- Walking a record of maps with a function of its own for each, in an
-- applicative that is only known where it is used: what each copy is given
-- is named as the copy it is made from named its own.
newtype Morph f g = Morph (forall a. f a -> g a)

class Walk t where
  walk :: (Applicative h) => Morph f (Compose h g) -> t f -> h (t g)

data Entry f = Entry Int (f Int)

instance Walk Entry where
  walk (Morph f) (Entry position x) = Entry position <$> getCompose (f x)

data Table f = Table !(Map.Map Int (Entry f)) !(Map.Map Int (Entry f))

instance Walk Table where
  walk f (Table left right) = Table <$> traverse (walk f) left <*> traverse (walk f) right

doubled :: Morph Identity (Compose Maybe Identity)
doubled = Morph (\(Identity x) -> Compose (Just (Identity x)))

contents :: Map.Map Int (Entry Identity) -> [(Int, Int)]
contents m = [(p, v) | Entry p (Identity v) <- Map.elems m]

main :: IO ()
main = do
  print (sum [1 .. 1000000 :: Int], product [1 .. 15 :: Int], length [1 .. 12345 :: Int])
  let given = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5 :: Int]
  print (sum given, length given, foldl' (-) 100 given, foldl (flip (:)) [] given)
  print (scaled 3 given, scaled 0 given, scaled 2 [1 .. 1000])
  print (weighted 1 100, weighted 10 5)
  print (spread given, spread [])
  print (length (filter even [1 .. 99 :: Int]), sum (map (* 2) [1 .. 10 :: Int]))
  print (sum [x * y | x <- [1 .. 10 :: Int], y <- [x .. 10]])
  print (firstBig [1, 50, 200, 7], firstBig [])
  print (foldl' (\m (k, v) -> Map.insert k v m) Map.empty (zip "fold" [1 :: Int ..]))
  let table k = Map.fromList [(i, Entry i (Identity (k + i))) | i <- [1 .. 4]]
  case walk doubled (Table (table 0) (table 100)) of
    Just (Table left right) -> print (contents left, contents right)
    Nothing -> putStrLn "nothing"
