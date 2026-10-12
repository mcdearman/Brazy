module Main where

-- Instances asked for where the type is declared, with a deriving clause.

data Colour = Red | Green | Blue
  deriving (Eq, Ord, Show, Enum, Bounded)

data Point = Point Int Int
  deriving (Eq, Ord, Show)

-- The instance requires the class of the type's parameter.
data Pair a = Pair a a
  deriving (Eq, Show)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

-- One class, without parentheses.
newtype Wrapper a = Wrapper a
  deriving Show

data Record = Record {name :: String, size :: Int}
  deriving (Eq, Show)

-- A type that is local to a definition.
local :: Int -> String
local n = show (Box n, Box n == Box (n + 1))
  where
    data Box = Box Int
      deriving (Eq, Show)

main :: IO ()
main = do
  print [minBound .. maxBound :: Colour]
  print (Red < Blue, succ Red, Red == Red)
  print (Point 1 2, Point 1 2 == Point 1 2, Point 1 2 < Point 1 3)
  print (Pair 'a' 'b', Pair 1 2 == Pair 1 (2 :: Int))
  print (Node Leaf 1 (Node Leaf 2 Leaf) :: Tree Int)
  print (Node Leaf 'a' Leaf < Node Leaf 'b' Leaf, (Leaf :: Tree Int) == Leaf)
  print (Wrapper (Just 3))
  print (Record {name = "one", size = 1}, Record "a" 1 == Record "a" 2)
  putStrLn (local 4)
