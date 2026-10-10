module Main where

-- Values that refer to themselves, which counting references cannot free.

-- A list that goes round: its last cell points back to its first.
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
