module Main where

-- Twenty thousand insertions into a search tree. Each takes apart the nodes
-- on the way down and builds them again around the new one.

data Tree = Leaf | Node !Tree !Int !Tree

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x (Node left y right)
  | x < y = Node (insert x left) y right
  | y < x = Node left y (insert x right)
  | otherwise = Node left y right

build :: Int -> Int -> Tree -> Tree
build 0 _ tree = tree
build n seed tree = build (n - 1) next (insert (next `rem` 100003) tree)
  where
    next = (seed * 7919 + 12345) `rem` 1000003

size :: Tree -> Int
size Leaf = 0
size (Node left _ right) = size left + 1 + size right

main :: IO ()
main = print (size (build 20000 1 Leaf))
