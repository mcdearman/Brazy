module Main where

-- Three passes over a list of a hundred thousand numbers. Each pass takes
-- every cell of the list apart and builds one of the same size.

count :: Int -> Int -> [Int]
count from to = if from > to then [] else from : count (from + 1) to

total :: Int -> [Int] -> Int
total acc [] = acc
total acc (x : xs) = let acc' = acc + x in acc' `seq` total acc' xs

main :: IO ()
main = print (total 0 (map (+ 1) (map (* 2) (map (subtract 3) (count 1 100000)))))
