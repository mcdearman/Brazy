module Main where

-- Functions that only read what they are given, called by something that
-- goes on holding it. The native tests work every count of references out
-- again as these run, so a value that is given up twice, or not at all, or
-- used after it has gone, is found.

data Tree = Leaf | Node !Tree !Int !Tree

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x (Node left y right)
  | x < y = Node (insert x left) y right
  | y < x = Node left y (insert x right)
  | otherwise = Node left y right

fromList :: [Int] -> Tree
fromList = foldr insert Leaf

-- Only reads the tree, all the way down.
member :: Int -> Tree -> Bool
member _ Leaf = False
member x (Node left y right)
  | x < y = member x left
  | y < x = member x right
  | otherwise = True

-- Reads the tree and comes back to itself for the other side, so what it
-- reads out has to be kept across a call.
size :: Tree -> Int
size Leaf = 0
size (Node left _ right) = size left + 1 + size right

-- Gives back a part of what it reads, which whoever asked then owns.
leftmost :: Tree -> Tree
leftmost Leaf = Leaf
leftmost (Node Leaf x right) = Node Leaf x right
leftmost (Node left _ _) = leftmost left

root :: Tree -> Int
root Leaf = 0
root (Node _ x _) = x

-- Keeps what it reads in something it makes.
spine :: Tree -> [Tree]
spine Leaf = []
spine (Node left x right) = Node left x right : spine left

-- Keeps what it reads in a function it gives back.
below :: Tree -> Int -> Bool
below Leaf = \_ -> False
below (Node _ x _) = \y -> y < x

-- Reads on one path and stops the program on another, which is not taken.
checked :: Int -> Tree -> Int
checked _ Leaf = 0
checked limit (Node left x right)
  | x > limit = error "a number over the limit"
  | otherwise = checked limit left + checked limit right + 1

-- A list, whose cells hold thunks, read while it is kept.
count :: Int -> Int -> [Int]
count from to = if from > to then [] else from : count (from + 1) to

total :: [Int] -> Int -> Int
total [] acc = acc
total (x : xs) acc = total xs (acc + x)

longer :: [Int] -> Int -> Bool
longer [] _ = False
longer (_ : xs) n = if n == 0 then True else longer xs (n - 1)

-- Counts the numbers up to a limit that are in a tree, which is kept for
-- the next time round.
hits :: Int -> Tree -> Int -> Int
hits 0 _ found = found
hits n tree found = hits (n - 1) tree (if member n tree then found + 1 else found)

main :: IO ()
main = do
  let tree = fromList [5, 2, 8, 1, 9, 3, 7, 4, 6, 12, 15, 11]
      list = count 1 50
  print (hits 20 tree 0)
  print (size tree)
  print (size tree + size tree)
  print (root (leftmost tree))
  print (map root (spine tree))
  print (map (below tree) [1, 20])
  print (checked 100 tree)
  print (member 7 tree, member 10 tree)
  print (size (leftmost tree), size tree)
  print (longer list 10, total list 0, longer list 60)
  print (total list 0)
  print (hits 20 (insert 13 tree) 0, size tree)
