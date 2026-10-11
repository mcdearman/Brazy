module Main where

-- What is certain to be evaluated may be evaluated early, and nothing else
-- may. Each thing here that must be left alone would stop the program.

-- The second argument is used on one path only.
choose :: Bool -> Int -> Int
choose True _ = 0
choose False x = x

-- Both are forced on every path, whichever way they are added.
sumTo :: Int -> Int -> Int
sumTo 0 acc = acc
sumTo n acc = sumTo (n - 1) (acc + n)

sumFrom :: Int -> Int -> Int
sumFrom 0 acc = acc
sumFrom n acc = sumFrom (n - 1) (twice n + acc)

twice :: Int -> Int
twice n = n + n

-- The two change places at each call, so neither is certain to be forced:
-- which is given back depends on the number.
pick :: Int -> Int -> Int -> Int
pick 0 x _ = x
pick n x y = pick (n - 1) y x

-- The seed is not looked at when there is nothing left to do.
walk :: Int -> Int -> Int -> Int
walk 0 _ total = total
walk n seed total = walk (n - 1) next (total + next)
  where
    next = (seed * 7 + 1) `rem` 11

-- Functions that call each other.
isEven :: Int -> Bool -> Bool
isEven 0 answer = answer
isEven n answer = isOdd (n - 1) (not answer)

isOdd :: Int -> Bool -> Bool
isOdd 0 answer = answer
isOdd n answer = isEven (n - 1) (not answer)

-- A name that is used on one path, and one that is used on both.
branch :: Bool -> Int -> Int
branch flag n =
  let never = error "a name that is not used was evaluated"
      always = n * 2
   in if flag then always + 1 else always + (if n > 0 then 2 else never)

-- Only as much of a list as is asked for.
from :: Int -> [Int]
from n = n : from (n + 1)

firsts :: Int -> [Int] -> [Int]
firsts 0 _ = []
firsts _ [] = []
firsts n (x : xs) = x : firsts (n - 1) xs

-- What an action is given is not evaluated before the actions in front of
-- it have run.
later :: Bool -> Int -> IO ()
later wanted x = do
  putStrLn "first"
  if wanted then print x else putStrLn "not wanted"

main :: IO ()
main = do
  print (choose True (error "an argument that is not used was evaluated"))
  print (choose False 5)
  print (sumTo 1000 0)
  print (sumFrom 1000 0)
  print (pick 1 (error "the wrong one was evaluated") 5)
  print (pick 2 6 (error "the wrong one was evaluated"))
  print (walk 0 (error "a seed that is not used was evaluated") 7)
  print (walk 5 3 0)
  print (isEven 10 True)
  print (isEven 7 True)
  print (branch True 0)
  print (branch False 3)
  print (firsts 3 (from 1))
  print (firsts 2 (1 : 2 : error "more of a list than was asked for"))
  later False (error "what an action did not want was evaluated")
  later True 9
