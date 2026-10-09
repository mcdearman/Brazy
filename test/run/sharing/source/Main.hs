module Main where

import Hazy.Prelude (trace)

-- A lazy value is evaluated at most once, however often it is used.
twice :: Int -> Int
twice x = x + x

data Pair = Pair Int Int

sumPair :: Pair -> Int
sumPair (Pair a b) = a + b + a

main :: IO ()
main = do
  let shared = trace "shared" (20 + 1 :: Int)
  print (twice shared)
  print (shared + 1)
  let field = trace "field" (5 :: Int)
  print (sumPair (Pair field field))
  let unused = trace "unused" (0 :: Int)
  print (const (1 :: Int) unused)
