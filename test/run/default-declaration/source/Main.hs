module Main where

import Floats (third)
import Plain (big)

-- A module's default declaration: here an ambiguous number is an Int, so
-- it is Bounded.
default (Int, Double)

main :: IO ()
main = do
  print (2 ^ 61 + 2 ^ 61 - 1, 7 `div` 2, 1 / 4)
  print (maxBound - 1 == pred maxBound)
  print (third, big)
