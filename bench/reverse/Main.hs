module Main where

-- Reverse a list of a hundred thousand numbers ten times over.

count :: Int -> Int -> [Int]
count from to = if from > to then [] else from : count (from + 1) to

reverseOnto :: [Int] -> [Int] -> [Int]
reverseOnto acc [] = acc
reverseOnto acc (x : xs) = reverseOnto (x : acc) xs

times :: Int -> [Int] -> [Int]
times 0 xs = xs
times n xs = times (n - 1) (reverseOnto [] xs)

main :: IO ()
main = print (head (times 10 (count 1 100000)))
