module Main where

import Data.List (foldl')

-- Functions that are not known where they are called, given as many
-- arguments as they take, fewer, and more; local and at the top; of up
-- to seven arguments.

add3 :: Int -> Int -> Int -> Int
add3 a b c = a * 100 + b * 10 + c

apply2 :: (a -> b -> c) -> a -> b -> c
apply2 f x y = f x y

apply3 :: (a -> b -> c -> d) -> a -> b -> c -> d
apply3 f x y z = f x y z

apply6 :: (a -> a -> a -> a -> a -> a -> b) -> a -> a -> a -> a -> a -> a -> b
apply6 f a b c d e g = f a b c d e g

apply7 :: (a -> a -> a -> a -> a -> a -> a -> b) -> a -> a -> a -> a -> a -> a -> a -> b
apply7 f a b c d e g h = f a b c d e g h

twice :: (a -> a) -> a -> a
twice f = f . f

pick :: Bool -> Int -> Int -> Int
pick b = if b then \x y -> x - y else \x y -> y - x

main :: IO ()
main = do
  print (apply2 (+) 1 (2 :: Int), apply3 add3 1 2 3)
  let partial = add3 1
  print (apply2 partial 2 3, map (add3 1 2) [4, 5, 6], zipWith3 add3 [1, 2] [3, 4] [5, 6])
  print (apply3 pick True 10 3, apply3 pick False 10 3, apply2 (pick True) 8 1)
  let k = 7 :: Int
      f a b c d e g = a + b * k + c - d + e * g
  print (foldr (\g acc -> g acc) 0 [f 1 2 3 4 5, f 6 5 4 3 2 . (+ 1)])
  print (apply6 f 1 2 3 4 5 6, map (apply6 f 1 2 3) [4, 5] <*> [6, 7] <*> [8])
  let go :: Int -> Int -> Int
      go n acc = if n == 0 then acc else go (n - 1) (acc + n)
  print (foldl' (\acc n -> go n acc) 0 [1 .. 100], foldr go 0 [1, 2, 3])
  print (twice (twice (* 2)) (3 :: Int), foldr (.) id [(+ 1), (* 2), subtract 3] (10 :: Int))
  let six a b c d e g = [a, b, c, d, e, g]
      seven a b c d e g h = (a, b, c, d, e, g, h)
  print (apply6 six 'h' 'a' 'z' 'y' '!' '?', apply7 seven 'a' 'b' 'c' 'd' 'e' 'f' 'g')
  print (apply3 (\x -> \y -> \z -> x ++ y ++ z) "eval" "/" "apply")
  let curried = \x -> let y = x * 2 in \z -> y + z
  print (map (curried 5) [1, 2, 3 :: Int], apply2 curried 4 (1 :: Int))
  print (sum (zipWith3 (\a b c -> a * b + c) [1 .. 10] [2 .. 11] [3 .. 12 :: Int]))
