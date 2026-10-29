module Main where

-- zip and zipWith, which make their lists with build and go through the
-- first with listFoldr: what each gives, how far each list is gone into,
-- and what goes through what they make without its being made.

main :: IO ()
main = do
  print (zip [1 :: Int, 2, 3] "ab")
  print (zipWith (+) [1 :: Int, 2, 3] [10, 20, 30, 40])
  print (zipWith3 (\a b c -> a + b * c) [1 :: Int, 2] [3, 4, 5] [6, 7])
  -- The second list is not gone into once the first is done, nor further
  -- than the first is.
  print (zip ([] :: [Int]) (undefined :: [Int]))
  print (zip [1 :: Int] (2 : undefined :: [Int]))
  print (take 3 (zip [1 :: Int ..] "abcdef"))
  print (zip (1 : 2 : undefined :: [Int]) "")
  -- Gone through as it is made.
  print (and [x < y | (x, y) <- zip [1 :: Int .. 100000] [2 ..]])
  print (sum (zipWith (*) [1 :: Int .. 1000] [1000, 999 .. 1]))
  print (length (filter id (zipWith (==) (cycle "ab") "abbaabab")))
  print (foldr (\(i, c) rest -> if c == 'x' then i else rest) (-1) (zip [0 :: Int ..] "abcxdx"))
  print (unzip (zip [1 :: Int, 2, 3] "xyz"))
