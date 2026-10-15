module Main where

-- A type that is never worked out is taken to be Integer, and if Integer
-- cannot be it, Double.

half x = x / 2

main :: IO ()
main = do
  print 2.5
  print (sqrt 2, pi, exp 1)
  print (round 2.5, round 3.5, truncate (-2.7), floor 2.7, ceiling 2.1)
  print (1 / 3, 2 ** 10, logBase 2 1024)
  print (fromIntegral (7 :: Int) / 2)
  print (realToFrac (1.5 :: Float) * 2)
  print (isNaN (0 / 0), isInfinite (1 / 0), decodeFloat 0.5)
  print (half 3, half (3 :: Float))
  print (read "1.25e2" / 2, read "12" + 1)
  print [1.5, 2 .. 4]
  print (2 ^ 70, 10 `div` 3, toRational 0.75)
  print (show 0.1, atan2 1 (-1), significand 8, exponent 8)
  print (properFraction 3.75, properFraction (-3.75))
  print (sum [0.1, 0.2, 0.3], maximum [1, 2.5], minimum [3, 1])
