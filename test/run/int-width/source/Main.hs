module Main where

import Data.Int (Int16, Int32, Int64, Int8)

-- An Int holds what programs expect it to, and ranges go either way.

main :: IO ()
main = do
  print (sum [1 .. 100000 :: Int], product [1 .. 20 :: Int], 2 ^ 40 :: Int)
  print (3000000000 * 3 :: Int, negate 5000000000 :: Int, (10 ^ 15) `div` 7 :: Int, (10 ^ 15) `mod` 7 :: Int)
  print (fromIntegral (2 ^ 50 :: Integer) :: Int, toInteger (123456789012 :: Int), fromEnum (2 ^ 40 :: Integer))
  print (read "9876543210" :: Int, show (-9876543210 :: Int), compare (2 ^ 33) (2 ^ 32 :: Int))
  -- Ranges down as well as up, of every kind of number.
  print ([10, 8 .. 1] :: [Int], [10, 7 .. -5] :: [Integer], [5, 4 .. 5] :: [Int], [1, 3 .. 0] :: [Int])
  print (['z', 'x' .. 'q'], [5.0, 4.5 .. 3.0] :: [Double], take 3 [10, 9 ..] :: [Int], [3 .. 1] :: [Int])
  -- The sized types come round at their own widths.
  print (fromIntegral (300 :: Int) :: Int8, fromIntegral (70000 :: Int) :: Int16, fromIntegral (2 ^ 33 + 5 :: Int) :: Int32)
  print (maxBound :: Int32, minBound :: Int64, (maxBound :: Int64) + 1, fromIntegral (2 ^ 40 :: Int) :: Int64)
  -- A monad of Either.
  print (do x <- Right 2; y <- Right 3; return (x * y :: Int) :: Either String Int)
  print (sequence [Right 1, Left "no", Right (3 :: Int)], traverse (\x -> if x > 0 then Right x else Left x) [1, 2, 3 :: Int])
