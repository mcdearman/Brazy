module Main where

import Shapes (Shape (..), area, describe, unit)

main :: IO ()
main = do
  putStrLn (describe (Square 3))
  putStrLn (describe (Rectangle 2 5))
  print (area unit + area unit)
  print (map area [Square 2, Rectangle 3 4], (area unit, describe unit) == (1, "square of area 1"))
