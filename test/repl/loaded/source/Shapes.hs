module Shapes where

data Shape = Circle Int | Square Int
  deriving (Show)

area :: Shape -> Int
area = \case
  Circle radius -> 3 * radius * radius
  Square side -> side * side
