module Main where

-- Lists that are made and gone through at once, which are never made, and
-- the same functions where they cannot be: what is worked out is the same
-- either way, and as lazy.

squares :: [Int] -> [Int]
squares xs = map (\x -> x * x) xs

main :: IO ()
main = do
  print (foldr (+) 0 (map (* 2) [1 .. 100 :: Int]))
  print (sum [x * y | x <- [1 .. 20 :: Int], y <- [x .. 20], odd (x + y)])
  print [(x, c) | x <- [1, 3 .. 11 :: Int], x /= 5, c <- "ab"]
  print (foldr (:) [] ([10, 8 .. 1 :: Int] ++ [] ++ [100 .. 98] ++ [7]))
  print (and (map even [2, 4 .. 100 :: Int]), or (map odd [2, 4 .. 100 :: Int]), any (> 9) [1 .. 10 :: Int])
  print (concatMap (\x -> [x, x * 10]) [1 .. 5 :: Int], concat [[1, 2], [], [3 :: Int]])
  print (take 5 (map (* 3) [1 :: Int ..]), head (filter (> 1000) [1 :: Int ..]))
  print (any (> 1000000) [1 :: Int ..], takeWhile (< 20) [x * x | x <- [1 :: Int ..]])
  print (elem 99 (map (+ 1) [1 .. 100 :: Int]), length (filter odd [1 .. 1001 :: Int]))
  let shared = squares [1 .. 10]
  print (foldr (+) 0 shared, length shared, shared)
  print (foldr (\x rest -> x : take 2 rest) [] [1 .. 10 :: Int])
  print (foldr (\x _ -> x) 0 (map (\x -> if x > 7 then undefined else x) [7, 8, 9 :: Int]))
  print [if b then 'y' else 'n' | b <- map (> 2) [1 .. 4 :: Int]]
  print (foldr (+) 0 [x | Just x <- [Just 1, Nothing, Just (3 :: Int)]])
  print (sum (concatMap (\n -> [1 .. n]) [1 .. 30 :: Int]))
  print (foldr (\x acc -> acc * 2 + x) 0 ([1 .. 10] ++ [1 .. 10 :: Int]))
