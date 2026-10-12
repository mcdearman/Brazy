module Main where

-- Values that refer to themselves. As objects they would hold themselves,
-- which counting references cannot free, so the compiler sees to it that
-- none is made. The native tests fail if a block is left that nothing
-- reaches.

-- A list that goes round: its last cell points back to its first. It uses
-- a variable of where it is, so it is worked out again as it goes round,
-- and the compiler warns of it.
ring :: Int -> [Int]
ring n = xs
  where
    xs = n : ys
    ys = (n + 1) : zs
    zs = (n + 2) : xs

-- Two functions that are defined by each other.
parity :: Int -> (Bool, Bool)
parity n = (even' n, odd' n)
  where
    even' 0 = True
    even' m = odd' (m - 1)
    odd' 0 = False
    odd' m = even' (m - 1)

-- A list that goes round and uses nothing of where it is. It goes to the
-- top of the program, where it is one cell for good.
ones :: Int -> [Int]
ones n = take n cells
  where
    cells = 1 : cells

-- The same for a list that is defined by what it has already worked out.
-- At the top it remembers, so this takes no time to speak of.
fibonacci :: Int -> Int
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : next fibs (tail fibs)
    next (a : as) (b : bs) = (a + b) : next as bs
    next _ _ = []

-- And one that does use a variable of where it is, which is worked out
-- again each time it looks back at itself: it is kept short.
from :: Int -> Int -> Int
from start n = sums !! n
  where
    sums = start : next sums
    next (a : as) = (a + start) : next as
    next [] = []

-- A value and a function that are defined by each other.
steps :: Int -> [Int]
steps limit = go 0
  where
    go n = if n >= limit then [] else n : rest n
    rest n = go (n + stride)
    stride = if limit > 100 then head (go 0) + 2 else 3

total :: Int -> Int -> Int
total 0 acc = acc
total n acc = total (n - 1) (acc + sum (take 7 (ring n)))

count :: Int -> Int -> Int
count 0 acc = acc
count n acc = case parity (n `rem` 9) of
  (True, False) -> count (n - 1) (acc + 1)
  _ -> count (n - 1) acc

main :: IO ()
main = do
  print (take 8 (ring 1))
  print (total 300 0)
  print (count 300 0)
  print (ones 5, sum (ones 1000))
  print (fibonacci 40, fibonacci 30)
  print (from 3 12, from 1 0)
  print (steps 10, length (steps 200))
