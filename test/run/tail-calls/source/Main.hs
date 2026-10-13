module Main where

-- A function that calls itself last goes round in the stack it started
-- with, however many times. Each of these goes round more times than a
-- program's stack has room for calls.

-- Calls itself last, and looks at both arguments each time round.
count :: Int -> Int -> Int
count total n
  | n == 0 = total
  | total < 0 = total
  | otherwise = count (total + 1) (n - 1)

-- The same down a list.
walk :: [Int] -> Int -> Int
walk [] total = total
walk (x : rest) total
  | total < 0 = total
  | otherwise = walk rest (total + x - x + 1)

-- Two that call each other last.
even', odd' :: Int -> Bool
even' n = if n == 0 then True else odd' (n - 1)
odd' n = if n == 0 then False else even' (n - 1)

-- One that is inside another and uses what is outside it.
inner :: Int -> Int
inner limit = go 0 0
  where
    go total n
      | n == limit = total
      | total < 0 = total
      | otherwise = go (total + 1) (n + 1)

-- One that requires something of its type.
times :: (Eq a, Num a) => a -> a -> a
times total n
  | n == 0 = total
  | total == 0 - 1 = total
  | otherwise = times (total + 1) (n - 1)

-- One that is given more than it takes at once.
choose :: Int -> Int -> Int -> Int
choose n total
  | n == 0 = \extra -> total + extra
  | total < 0 = \extra -> extra
  | otherwise = choose (n - 1) (total + 1)

main :: IO ()
main = do
  print (count 0 100000)
  print (walk [1 .. 100000] 0)
  print (even' 100000)
  print (inner 100000)
  print (times 0 (100000 :: Int))
  print (choose 100000 0 7)
