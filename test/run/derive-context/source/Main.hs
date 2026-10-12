module Main where

import Data.Functor.Classes (showsPrec1)
import Data.Functor.Identity (Identity (..))

-- An instance that a deriving clause asks for requires what it takes to
-- have the class for everything the type holds, and that is worked out.

-- A parameter that takes a type: the class is required of it given what it
-- is given, and not of the parameter by itself, which could not have it.
data Wrap f a = Wrap (f a) deriving (Show, Eq)

-- A parameter that nothing of whose type is held: nothing is required of
-- it.
data Tagged tag a = Tagged a deriving (Show, Eq, Ord)

data Unshowable = Unshowable

-- Through another type, and through types that are built in.
data Pair a b = Pair a b deriving (Show)

data Both f a = Both (Pair (f a) [a]) (Maybe (a, a)) deriving (Show)

-- A type that holds itself, and two that hold each other.
data List a = Nil | Cons a (List a) deriving (Show, Eq)

data Even a = Zero | Even a (Odd a) deriving (Show)

data Odd a = Odd a (Even a) deriving (Show)

-- As before for the plain ones.
data Colour = Red | Green deriving (Show, Eq, Ord, Enum, Bounded)

data Box a = Box a | Empty deriving (Show, Eq, Ord)

main :: IO ()
main = do
  print (Wrap (Just 'x'), Wrap [1 :: Int, 2] == Wrap [1, 2])
  print (Tagged 'x' :: Tagged Unshowable Char, (Tagged 1 :: Tagged Unshowable Int) < Tagged 2)
  print (Both (Pair (Just 'x') "yz") (Just ('a', 'b')))
  print (Cons 'x' (Cons 'y' Nil), Cons (1 :: Int) Nil == Cons 1 Nil)
  print (Even 'a' (Odd 'b' Zero))
  print (Box Red, Box 'x' < Empty, [minBound .. maxBound :: Colour], Box [Just (1 :: Int)])
  -- Showing something that holds values, given how to show the values.
  putStrLn (showsPrec1 11 (Identity 'x') (showsPrec1 0 (Just [1 :: Int]) ""))
