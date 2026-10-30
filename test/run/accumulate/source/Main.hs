module Main where

-- A function that adds a number to what calling itself gives back gives
-- back the same when it adds as it goes.

collatz :: Int -> Int
collatz 1 = 0
collatz n = 1 + collatz (if even n then n `div` 2 else 3 * n + 1)

-- The number on the right, and a name rather than a number.
steps :: Int -> Int -> Int
steps by 0 = by
steps by n = steps by (n - 1) + by

-- Calling itself last as well, and giving back what a primitive gives.
mixed :: Int -> Int
mixed n
  | n <= 0 = n * 7
  | even n = mixed (n - 3)
  | otherwise = 2 + mixed (n - 1)

-- Giving back what another function gives, and a value that is named.
other :: Int -> Int
other n
  | n < 10 = length (show n)
  | otherwise = let m = n `div` 10 in 1 + other m

-- Two ways down, as a tree is gone through.
data Tree = Leaf | Node Tree Tree

check :: Tree -> Int
check Leaf = 0
check (Node l r) = 1 + check l + check r

make :: Int -> Tree
make 0 = Leaf
make d = Node (make (d - 1)) (make (d - 1))

-- Wrapping round as it adds.
big :: Int -> Int
big 0 = maxBound
big n = maxBound + big (n - 1)

main :: IO ()
main = do
  print (maximum (map collatz [1 .. 10000]))
  print (steps 3 100000, steps (-2) 5)
  print (map mixed [-2, 0, 1, 5, 10, 11])
  print (other 1234567890)
  print (check (make 16))
  print (big 3 == maxBound * 4)
