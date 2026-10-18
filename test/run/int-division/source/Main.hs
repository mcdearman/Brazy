module Main where

-- Division of Ints both ways it rounds, for every sign of each side, and
-- with what is left over and nothing left over.

pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [-17, -16, -1, 0, 1, 16, 17, 1000000007], y <- [-5, -4, -1, 1, 4, 5, 3]]

main :: IO ()
main = do
  print [(x `div` y, x `mod` y) | (x, y) <- pairs]
  print [(x `quot` y, x `rem` y) | (x, y) <- pairs]
  print [x `divMod` y == (x `div` y, x `mod` y) && x `quotRem` y == (x `quot` y, x `rem` y) | (x, y) <- pairs]
  print (and [y * (x `div` y) + x `mod` y == x | (x, y) <- pairs])
  print (sum [x `mod` 7 | x <- [-1000 .. 1000 :: Int]], sum [x `div` 3 | x <- [-1000 .. 1000 :: Int]])
