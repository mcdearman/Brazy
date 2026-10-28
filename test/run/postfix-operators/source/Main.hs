{-# LANGUAGE NoPostfixOperators #-}
module Main where

import Postfix (factorials)

-- An operator that looks at its first argument before it takes its second.
(%) :: Int -> Int -> Int
(%) n = n `seq` (+ n)

infixl 6 %

data Pair = Int :& Int
  deriving (Show)

main :: IO ()
main = do
  -- With postfix operators off, a left section is a function of the second
  -- argument: making one looks at nothing.
  print (((undefined :: Int) %) `seq` "a function")
  print (map (10 %) [1, 2, 3], map (1 + 1 %) [4])
  print (map (2 :) [[3], []], map (1 :&) [2, 3], map ("x" ++) ["y", "z"])
  -- With them on, a left section is the operator called on its left, so an
  -- operator of one argument can be used after what it takes.
  print factorials
