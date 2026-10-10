module Main where

factorial :: Integer -> Integer
factorial n = if n <= 1 then 1 else n * factorial (n - 1)

big :: Integer
big = 123456789012345678901234567890

main :: IO ()
main = do
  print (factorial 30)
  print big
  print (big * big - 1)
  print (negate big)
  print (abs (negate big), signum (negate big))
  print (quot (negate big) 1000, rem (negate big) 1000)
  print (div (negate big) 1000, mod (negate big) 1000)
  print (big == big + 1 - 1, big < big + 1, compare big 0)
  print (fromInteger big :: Int)
  print (fromInteger (negate big) :: Int)
  print (toInteger (maxBound :: Int) + 1)
  print (read "-98765432109876543210" :: Integer)
