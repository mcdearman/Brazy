module Postfix (factorials) where

-- An operator of one argument: with postfix operators, (n !) is the
-- factorial of n.
(!) :: Integer -> Integer
(!) n = product [1 .. n]

factorials :: [Integer]
factorials = [(n !) | n <- [0 .. 6]]
