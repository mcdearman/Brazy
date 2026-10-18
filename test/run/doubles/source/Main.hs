module Main where

-- Numbers written with a point, at Double and at Float; the arithmetic
-- that is done where it is used; comparisons, with what is not a number;
-- and Ints made floating-point numbers.

half :: Double
half = 0.5

third :: Float
third = 0.3333333333

main :: IO ()
main = do
  print (half, third, 1.0e-3 :: Double, 2.5e10 :: Float, 0.1 + 0.2 :: Double, 0.1 + 0.2 :: Float)
  print (1 / 3 :: Double, 1 / 3 :: Float, 7 - 2.25 :: Double, 3 * 0.1 :: Float)
  print (negate 2.5 :: Double, abs (-4.25) :: Float, sqrt 2 :: Double, sqrt 2 :: Float)
  let nan = 0 / 0 :: Double
      infinity = 1 / 0 :: Double
  print (nan == nan, nan < 1, nan > 1, compare 1 (2 :: Double), compare 2.5 (2.5 :: Double), max 1 infinity)
  print (isNaN nan, isInfinite infinity, signum (-0.5 :: Double), signum (0 :: Float))
  print [fromIntegral x / 4 :: Double | x <- [-3, 0, 5, 1000000007 :: Int]]
  print [fromIntegral x * 1.5 :: Float | x <- [-3, 0, 5, 1000000007 :: Int]]
  print (sum [fromIntegral i * 0.25 | i <- [1 .. 1000 :: Int]] :: Double)
  print (truncate (1e10 :: Double) :: Int, round (2.5 :: Double) :: Int, floor (-0.5 :: Float) :: Int, ceiling (1.000001 :: Double) :: Int)
  print (realToFrac (1.1 :: Float) :: Double, realToFrac (1.1 :: Double) :: Float)
