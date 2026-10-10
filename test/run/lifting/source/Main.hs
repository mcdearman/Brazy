module Main where

-- A function in a `where` that uses a variable of where it is.
below :: Int -> [Int] -> Int
below limit = go 0
  where
    go total [] = total
    go total (x : xs)
      | x < limit = go (total + 1) xs
      | otherwise = go total xs

-- Two that call each other, and use different variables.
parity :: Int -> Int -> Int -> Int
parity yes no = even'
  where
    even' n = if n == 0 then yes else odd' (n - 1)
    odd' n = if n == 0 then no else even' (n - 1)

-- One that is passed on as a value, and one that is given too few arguments.
scale :: Int -> [Int] -> [Int]
scale factor xs = map times (map (plus factor) xs)
  where
    times x = x * factor
    plus a b = a + b + factor

-- One inside another, each using what is around it.
grid :: Int -> Int -> Int
grid width height = rows 0 0
  where
    rows y total
      | y == height = total
      | otherwise = rows (y + 1) (columns 0 total)
      where
        columns x sum'
          | x == width = sum'
          | otherwise = columns (x + 1) (sum' + x * y)

-- One that uses a value which is itself worked out where they are.
offset :: Int -> [Int] -> [Int]
offset n = go
  where
    base = n * n
    go [] = []
    go (x : xs) = (x + base) : go xs

main :: IO ()
main = do
  print (below 3 [1, 2, 3, 4, 5])
  print (parity 10 20 7)
  print (parity 10 20 8)
  print (scale 3 [1, 2, 3])
  print (grid 4 3)
  print (offset 5 [1, 2, 3])
