{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Map as Map

-- A loop whose accumulator is evaluated each time round.
sumTo :: Int -> Int
sumTo n = go 0 1
  where
    go !acc i
      | i > n = acc
      | otherwise = go (acc + i) (i + 1)

-- A bang on nothing evaluates what it is matched against too.
both :: Int -> Int -> Int
both !_ y = y

-- A bang on a pattern that already looks: no different.
first :: (Int, Int) -> Int
first !(a, _) = a

-- In a let.
strictLet :: Int -> Int
strictLet x = let !y = x * 2 in y + 1

-- Strict fields.
data P = P !Int !Int deriving (Show)

-- The operator, written apart and written together: not bangs.
lookups :: Map.Map Int String -> (String, String)
lookups m = (m Map.! 1, m Map.!2)

(!) :: Int -> Int -> Int
a ! b = a * 10 + b

-- Something that is never evaluated without a bang.
unused :: Int -> Int
unused _ = 0

main :: IO ()
main = do
  print (sumTo 1000000, both 1 2, first (3, 4), strictLet 5)
  print (P 1 2, lookups (Map.fromList [(1, "one"), (2, "two")]), 4 ! 2, 4!3)
  print ((\ !x -> x + 1) (41 :: Int))
  print (case 7 :: Int of !n -> n * 6)
  print (unused (error "never evaluated"))
