module Main where

-- Two thunks that work out the same thing are one, and nothing else is.

data Tree = Leaf | Node Tree Tree

-- Both halves are the same call: made once, each level is one node, and
-- both are gone through.
make :: Int -> Tree
make 0 = Leaf
make d = Node (make (d - 1)) (make (d - 1))

size :: Tree -> Integer
size Leaf = 1
size (Node l r) = size l + size r

-- Written the same, where a name means something else in between.
shadow :: Int -> (Int, Int)
shadow x = (x + 1, let x = 10 in x + 1)

-- Written the same, in a function and around it.
twice :: Int -> (Int, Int, Int)
twice y = let f z = y * y + z in (y * y, f 1, f 2)

-- Written the same, but of different things.
apart :: Int -> Int -> (Int, Int)
apart a b = (a * 2, b * 2)

main :: IO ()
main = do
  print (size (make 20))
  print (shadow 5)
  print (twice 7)
  print (apart 3 4)
  print [(a * 2, a * 2, b) | a <- [1 .. 3 :: Int], let b = a * 2 + 1]
  print (map (\x -> (x + 1, x + 1)) [1, 2, 3 :: Int])
