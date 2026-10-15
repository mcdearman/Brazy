module Main where

import Numeric (showEFloat, showFFloat, showGFloat, showHex, readHex, floatToDigits, fromRat)

-- The edges of floating point, as GHC has them.

main :: IO ()
main = do
  -- What is not a number, and what is endless.
  let nan = 0 / 0 :: Double
      infinity = 1 / 0 :: Double
  print (nan, infinity, negate infinity, nan == nan, nan < 1, compare nan 1)
  print (isInfinite infinity, isNaN infinity, -0.0 :: Double, isNegativeZero (-0.0 :: Double))
  print (Just (-1.5 :: Double), [negate 0.0 :: Double], Left (-0.0) :: Either Double Int)
  -- The smallest and the largest.
  print (5.0e-324 :: Double, 2.2250738585072014e-308 :: Double, 1.7976931348623157e308 :: Double)
  print (isDenormalized (5.0e-324 :: Double), decodeFloat (5.0e-324 :: Double))
  print (1.0e-45 :: Float, 3.4028235e38 :: Float, 1.17549435e-38 :: Float)
  -- Rounding to whole numbers.
  print (map round [0.5, 1.5, 2.5, -0.5, -1.5 :: Double] :: [Integer])
  print (map truncate [2.7, -2.7 :: Double] :: [Int], map floor [2.7, -2.7 :: Double] :: [Int], map ceiling [2.7, -2.7 :: Double] :: [Int])
  print (properFraction (-3.75 :: Double) :: (Integer, Double))
  print (truncate (1.0e20 :: Double) :: Integer)
  -- Between the two, and to and from fractions.
  print (realToFrac (0.1 :: Float) :: Double, realToFrac (0.1 :: Double) :: Float)
  print (toRational (0.75 :: Double), toRational (0.1 :: Float), fromRational (1 / 3) :: Double)
  print (fromIntegral (2 ^ 70 :: Integer) :: Double, fromIntegral (2 ^ 70 + 1 :: Integer) :: Float)
  print (fromRat (2 / 3) :: Double, floatToDigits 10 (0.3 :: Double), floatToDigits 10 (123.456 :: Float))
  -- Single precision is worked out in single precision.
  print (0.1 + 0.2 :: Float, 1 / 3 :: Float, sqrt 2 :: Float, exp 1 :: Float, 16777217 :: Float)
  -- Functions.
  print (sin 0 :: Double, cos pi :: Double, atan2 1 (-1) :: Double, 2 ** 0.5 :: Double, logBase 2 1024 :: Double)
  print (significand (8 :: Double), exponent (8 :: Double), scaleFloat 3 (1 :: Double), floatDigits (1 :: Float))
  -- Text.
  print (showEFloat (Just 3) (1234.5678 :: Double) "", showFFloat (Just 2) (3.14159 :: Double) "", showGFloat Nothing (0.05 :: Double) "")
  print (showFFloat Nothing (1.0e-4 :: Double) "", showEFloat Nothing (0 :: Double) "", showFFloat (Just 0) (2.5 :: Double) "")
  print (map read ["1.5e3", "  2", "-0.25", "1e-2", "NaN", "Infinity"] :: [Double])
  print (reads "3.5 rest" :: [(Double, String)], read "(-1.5)" :: Double)
  print (showHex (255 :: Int) "", fst (head (readHex "ff" :: [(Int, String)])))
  -- Counting.
  print ([0.1, 0.2 .. 0.5 :: Double], take 3 [1.5 :: Float ..], fromEnum (3.9 :: Double), toEnum 4 :: Double)
  print (minimum [3.5, 1.25, 2 :: Double], maximum [1, 2.5 :: Float], sum [0.1, 0.2, 0.3 :: Double], product [1.5, 2 :: Double])
