module Main where

-- A class asked of one type variable more than once, given different
-- arguments each time.

exotic :: (Eq (f Int), Eq (f Char)) => f Int -> f Int -> f Char -> f Char -> Bool
exotic a b c d = a == b && c == d

-- Three times, and with a class that has a superclass, which is then known
-- for each as well.
ordered :: (Ord (f Int), Ord (f Bool), Ord (f Char)) => f Int -> f Bool -> f Char -> (Bool, Bool, Bool)
ordered a b c = (a < a, b == b, c <= c)

-- Passed on to a function that asks the same.
both :: (Eq (f Int), Eq (f Char)) => f Int -> f Char -> Bool
both a c = exotic a a c c

-- Two type variables, each asked twice.
pairs :: (Show (f Int), Show (f Bool), Show (g Int), Show (g Bool)) => f Int -> f Bool -> g Int -> g Bool -> String
pairs a b c d = show a ++ show b ++ show c ++ show d

main :: IO ()
main = do
  print (exotic (Just 1) (Just 1) (Just 'a') (Just 'b'))
  print (exotic [1] [1] "x" "x")
  print (ordered (Just 1) (Just True) (Just 'a'))
  print (both [1, 2] "ab")
  putStrLn (pairs (Just 1) (Just True) [2] [False])
