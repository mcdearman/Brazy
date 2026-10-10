module Main where

-- A number whose type nothing settles is an Integer.

-- Without a signature, and so not generalised: an Integer.
ambiguous = 1 + 2

power = 2 ^ 70

main :: IO ()
main = do
  print ambiguous
  print power
  print (3 * 4)
  print [1 .. 5]
  print (sum (map (* 2) [1, 2, 3]))
  print (10 `div` 3, 10 `mod` 3)
  print (compare 1 2, 1 == 1, max 3 7)
  print (fromIntegral (3 :: Int) + 1)
  print (length [1, 2, 3])
  -- An Int where something says so.
  print (length [1, 2, 3] + 1)
