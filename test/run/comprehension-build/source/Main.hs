module Main where

-- List comprehensions, made by build and gone through with listFoldr: what
-- each gives, in what order, with patterns that do not match, lets that
-- name what later parts use, and lists that never end.

pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [1 .. 3], y <- [x .. 3]]

triangles :: Int -> [(Int, Int, Int)]
triangles n = [(a, b, c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a * a + b * b == c * c]

justs :: [Maybe Int] -> [Int]
justs xs = [x | Just x <- xs]

firsts :: [[Int]] -> [Int]
firsts xss = [x | (x : _) <- xss]

named :: [Int]
named = [y | x <- [1 .. 6], let y = x * x, odd x, let z = y + 1, z > 2]

main :: IO ()
main = do
  print pairs
  print (triangles 20)
  print (justs [Just 1, Nothing, Just 3, Nothing])
  print (firsts [[1, 2], [], [3], []])
  print named
  print [c | c <- "hello world", c `notElem` "lo"]
  -- What never ends, taken from.
  print (take 5 [x * 2 | x <- [1 :: Int ..], x `mod` 3 == 0])
  print (takeWhile (< 40) [x * x | x <- [1 :: Int ..]])
  -- What a comprehension makes is not worked out until it is asked for.
  print (length [undefined | _ <- [1 :: Int .. 4]])
  print (head [x | x <- [1 :: Int ..], x > 10])
  -- Gone through as it is made.
  print (sum [x | x <- [1 :: Int .. 100000], even x])
  print (and [x < 10 | x <- [1 :: Int .. 9]], or [x > 9 | x <- [1 :: Int .. 9]])
  print ([] :: [Int], [x | x <- [] :: [Int]], [() | False])
  print [(i, w) | (i, w) <- zip [1 :: Int ..] (words "a b c"), i /= 2]
