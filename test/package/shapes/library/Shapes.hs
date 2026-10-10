module Shapes (Shape (..), area, describe, unit) where

import Shapes.Names (name)

data Shape
  = Square Int
  | Rectangle Int Int

area :: Shape -> Int
area (Square side) = side * side
area (Rectangle width height) = width * height

-- A value that is evaluated once, however many programs' modules use it.
unit :: Shape
unit = Square 1

describe :: Shape -> String
describe shape = name (sides shape) ++ " of area " ++ show (area shape)
  where
    sides (Square _) = 1 :: Int
    sides (Rectangle _ _) = 2
