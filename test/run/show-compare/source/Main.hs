module Main where

import Data.List (sort, sortBy)
import Data.Ord (comparing)

-- Ints shown, of every size and sign and inside other things; and lists
-- and characters compared every way there is.

main :: IO ()
main = do
  print [0, 7, -7, 10, -10, 123456789, -987654321, 4611686018427387903, -4611686018427387903 :: Int]
  print (Just (-5 :: Int), [Left (-1), Right 2 :: Either Int Int], (-3 :: Int, 4 :: Int))
  putStrLn (showsPrec 7 (-12 :: Int) "" ++ " " ++ showsPrec 6 (-12 :: Int) "" ++ " " ++ show (abs (-42 :: Int)))
  print (map show [99, 100, 101, 999999, 1000000 :: Int])
  let words' = ["banana", "apple", "", "app", "apples", "Banana", "b"]
  print (sort words', maximum words', minimum words')
  print [(a < b, a <= b, a > b, a >= b, compare a b, a == b, a /= b) | (a, b) <- [("app", "apple"), ("b", "a"), ("", ""), ("x", "x")]]
  print ([1, 2, 3] < [1, 2 :: Int], [[1], [2, 3]] > [[1], [2 :: Int]], compare [Just 'a'] [Nothing])
  print [(c, c == 'a', c < 'm', compare c 'm') | c <- "aMz \n"]
  print (sortBy (comparing snd) (zip [1 :: Int ..] "hazy"))
