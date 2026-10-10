module Main where

-- Values that are taken apart, and built again after something is called.

data Tree = Leaf | Node !Tree !Int !Tree

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x (Node left y right)
  | x < y = Node (insert x left) y right
  | y < x = Node left y (insert x right)
  | otherwise = Node left y right

toList :: Tree -> [Int]
toList tree = go tree []
  where
    go Leaf rest = rest
    go (Node left x right) rest = go left (x : go right rest)

fromList :: [Int] -> Tree
fromList = foldr insert Leaf

data Pair = Pair !Int !Int

-- Built again on one path only: on the other the block is given up.
bump :: Int -> Pair -> Maybe Pair
bump limit (Pair a b)
  | a + b < limit = Just (Pair (a + 1) (b + 1))
  | otherwise = Nothing

total :: Maybe Pair -> Int
total Nothing = -1
total (Just (Pair a b)) = a + b

data Strict = End | Cons !Int !Strict

-- Each cell is built again when the rest has been.
double :: Strict -> Strict
double End = End
double (Cons x rest) = Cons (x * 2) (double rest)

count :: Int -> Strict
count 0 = End
count n = Cons n (count (n - 1))

add :: Strict -> Int
add End = 0
add (Cons x rest) = x + add rest

main :: IO ()
main = do
  let tree = fromList [5, 3, 8, 1, 4, 7, 9, 2, 6]
  print (toList tree)
  -- The tree is used again afterwards, so nothing of it may be built over.
  let more = insert 10 (insert 0 tree)
  print (toList more)
  print (toList tree)
  print (toList (insert 4 (insert 11 more)))
  print (total (bump 10 (Pair 1 2)))
  print (total (bump 2 (Pair 1 2)))
  let cells = count 10
  print (add (double cells))
  print (add (double (double cells)))
  print (add cells)
