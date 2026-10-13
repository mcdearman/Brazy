module Main where

import Data.List (sort)

-- At the top, with no type written: it requires what it uses.
twice x = x + x

shown x = "<" ++ show x ++ ">"

smallest x y = if x <= y then x else y

-- Two things are required, of two types.
describe x y = show x ++ (if y == y then "!" else "?")

-- What a class requires comes with it.
ordered xs = sort xs == xs && all (\x -> x == x) xs

outer :: Int -> Bool -> String
outer n b = wrap n ++ wrap b ++ wrap "s" ++ show (pair n, pair 'c')
  where
    -- Inside another definition, used at three types.
    wrap value = "[" ++ show value ++ "]"
    pair value = (value, value == value)

-- A definition that is no function is of one type, worked out once.
limit = 10

total = sum [1 .. limit]

-- One that uses itself is of one type too.
count n = if n <= 0 then 0 else 1 + count (n - 1)

main :: IO ()
main = do
  print (twice (2 :: Int), twice (2 :: Integer), twice 3)
  putStrLn (shown 'a' ++ shown True ++ shown [1, 2 :: Int])
  print (smallest 'b' 'a', smallest (2 :: Int) 3, smallest "b" "a")
  putStrLn (describe (1 :: Int) 'x' ++ describe "s" True)
  print (ordered [1, 2, 3 :: Int], ordered "ba")
  putStrLn (outer 1 True)
  print (total, count 5)
  let local x = show x ++ show x
      each = map local
  putStrLn (local 'a' ++ local (1 :: Int) ++ concat (each [True]))
