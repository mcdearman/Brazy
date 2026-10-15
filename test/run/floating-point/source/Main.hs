module Main where

main :: IO ()
main = do
  let x = 1.5 :: Double
  print (x + 2)
  print (x * x, x / 4, sqrt 2 :: Double)
  print (truncate (x * 10) :: Int, round (2.5 :: Double) :: Integer, floor (-1.5 :: Double) :: Int)
  print (fromIntegral (3 :: Int) / 2 :: Double)
  print (1.0e-2 :: Double, 12345678.9 :: Double, 0.1 + 0.2 :: Double)
  print (read "3.25" :: Double)
  print (pi :: Float, exp 1 :: Double)
  print (decodeFloat (0.75 :: Double), isNaN (0 / 0 :: Double))
  print (show (-2.5 :: Double), [1.0, 1.5 .. 3.0 :: Double])
