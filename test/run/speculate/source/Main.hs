module Main where

-- What is named and only works out a few sums of numbers already worked
-- out is worked out where it is named, and nothing else is: a division
-- that may be by nothing, and anything that would work out something not
-- yet worked out, are left until they are asked for, if they are.

data Point = Point !Double !Double

distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') =
  let dx = x' - x
      dy = y' - y
      d2 = dx * dx + dy * dy
   in sqrt d2

pick :: Bool -> Int -> Int -> Int
pick useIt n d =
  let quotient = n `div` d
      sum' = n + 1
   in if useIt then quotient + sum' else sum'

main :: IO ()
main = do
  print (distance (Point 0 0) (Point 3 4))
  print (sum [distance (Point (fromIntegral i) 0) (Point 0 (fromIntegral i)) | i <- [1 .. 100 :: Int]])
  -- Never divided by nothing, as the division is never asked for.
  print (pick False 10 0, pick True 10 3)
  -- Something that is not worked out is not worked out for the sum of it.
  let a = undefined :: Int
      b = a + 1
      c = 2 * 3 :: Int
  print (if c > 5 then c else b)
  let x = 1 / 0 :: Double
      y = 0 / 0 :: Double
  print (x, isNaN y, negate x)
