module Main where

-- A definition with no type written that calls itself, and requires
-- something of its type: it is used at two types.
total [] = 0
total (x : xs) = x + total xs

-- Two that call each other.
evens [] = []
evens (x : xs) = x : odds xs

odds [] = []
odds (_ : xs) = evens xs

-- One inside another.
count :: [Int] -> Integer -> (Int, Integer)
count small big = (go (length small), go big)
  where
    go n = if n == 0 then 0 else 1 + go (n - 1)

main :: IO ()
main = do
  print (total [1, 2, 3 :: Int])
  print (total [1.5, 2.5 :: Rational])
  print (evens "abcdef", evens [1, 2, 3 :: Int])
  print (count [1, 2] 3)
