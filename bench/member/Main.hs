module Main where

-- Two hundred thousand searches of a tree that is kept. Each goes down the
-- tree and only reads it: nothing is taken apart for good or built.

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

member :: Int -> Tree -> Bool
member _ Leaf = False
member x (Node left y right)
  | x < y = member x left
  | y < x = member x right
  | otherwise = True

hits :: Int -> Tree -> Int -> Int
hits 0 _ found = found
hits n tree found = hits (n - 1) tree (if member (n `rem` 100003) tree then found + 1 else found)

main :: IO ()
main = print (hits 200000 (build 20000 1 Leaf) 0)
