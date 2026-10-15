module Main where

-- Negation is a prefix operator of the precedence of addition, and groups
-- to the left: what follows it at that precedence takes it as its left
-- operand.

x, y :: Int
x = 5
y = 3

main :: IO ()
main = do
  print (- 1 + 2)
  print (- x + y)
  print (- x - y)
  print (- 2 * 3)
  print (- x ^ 2)
  print (- 1 + 2 + 3)
  print (- x == - 5)
  print (- x < y)
  print ((- 1) + 2)
  print [- 1, - x, - x * y]
