module Main where

-- Clauses that start the same way and then differ: on another argument, or
-- on what is inside the constructor they both matched. Each value should
-- be looked at once, and nothing that a clause does not reach should be
-- evaluated.

-- The second argument is not looked at when the first is empty.
pair :: [Int] -> [Int] -> Int
pair [] _ = 0
pair (_ : _) [] = 1
pair (x : _) (y : _) = x + y

-- Three arguments, each clause going one further.
depth :: [Int] -> [Int] -> [Int] -> Int
depth [] _ _ = 0
depth (_ : _) [] _ = 1
depth (_ : _) (_ : _) [] = 2
depth (x : _) (y : _) (z : _) = x + y + z

-- What is inside a constructor, and what is inside that.
inside :: Maybe [Int] -> Int
inside Nothing = -1
inside (Just []) = 0
inside (Just [x]) = x
inside (Just (x : y : _)) = x * y

-- Clauses for a constructor that are not next to each other.
data Colour = Red | Green | Blue

mixed :: Colour -> Bool -> Int
mixed Red True = 1
mixed Green _ = 2
mixed Red False = 3
mixed Blue True = 4
mixed _ _ = 5

-- A clause that is passed over by its guard goes on to the next, which
-- starts with the same test.
sign :: [Int] -> Int
sign (x : _) | x > 0 = 1
sign (x : _) | x < 0 = -1
sign (_ : rest) = sign rest
sign [] = 0

-- Strict fields, and a pair of things taken apart together.
data Shape = Circle !Int | Box !Int !Int

same :: Shape -> Shape -> Bool
same (Circle a) (Circle b) = a == b
same (Box a b) (Box c d) = a == c && b == d
same _ _ = False

-- A tuple of lists, as zipping writes it.
zipped :: ([Int], [Int]) -> [Int]
zipped (x : xs, y : ys) = x + y : zipped (xs, ys)
zipped _ = []

main :: IO ()
main = do
  print (pair [] (error "the second list was looked at"))
  print (pair [1] [], pair [1] [2])
  print (depth [] (error "second") (error "third"))
  print (depth [1] [] (error "third"))
  print (depth [1] [2] [], depth [1] [2] [3])
  print (map inside [Nothing, Just [], Just [7], Just [2, 3], Just (4 : 5 : error "the rest")])
  print [mixed Red True, mixed Green (error "not needed"), mixed Red False, mixed Blue True, mixed Blue False]
  print (map sign [[3], [-3], [0, 0, 5], [0, 0], []])
  print [same (Circle 1) (Circle 1), same (Box 1 2) (Box 1 2), same (Box 1 2) (Box 1 3), same (Circle 1) (Box 1 1)]
  print (zipped ([1, 2, 3], [10, 20]))
