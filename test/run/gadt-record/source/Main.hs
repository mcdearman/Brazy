module Main where

import Data.Kind (Type)

-- A constructor that is given with its whole type may name what it holds.

data T a where
  C :: {field :: a, count :: !Int} -> T a

-- A name that two constructors hold something under selects from both.
data Shape where
  Circle :: {name :: String, radius :: Int} -> Shape
  Square :: {side :: Int, name :: String} -> Shape
  deriving (Show)

-- Names given together.
data V where
  V :: {x, y :: Int} -> V

-- Constructors that tell the type as well.
data E a where
  N :: {number :: Int} -> E Int
  B :: {truth :: Bool} -> E Bool

eval :: E a -> a
eval (N n) = n
eval B {truth = b} = b

newtype W a where
  W :: {unW :: a} -> W a

-- The kind of a type may be said where it is declared, in place of some or
-- all of its parameters, and then it may have no constructors at all.
data Box :: Type -> Type where
  Box :: a -> Box a
  deriving (Show, Eq)

data P a :: Type -> Type where
  P :: a -> b -> P a b

data App :: (Type -> Type) -> Type -> Type where
  App :: f a -> App f a

data Void :: Type

data Tag :: Type -> Type

-- Instances may be asked for after the constructors, and are derived for
-- constructors that tell the type too.
data Value a where
  Number :: Int -> Value Int
  Truth :: Bool -> Value Bool
  deriving (Show, Eq)

-- An instance that is asked for on its own asks of the type's parameters
-- only what it says, which may be nothing: what a constructor hides could
-- not be asked anything.
data Tree a where
  Leaf :: Int -> Tree Int
  Fork :: Tree a -> Tree b -> Tree (a, b)

deriving instance Show (Tree a)

deriving instance Eq (Tree a)

data One where { One :: Int -> One } deriving (Show)

main :: IO ()
main = do
  let c = C {field = 'x', count = 2}
  print (field c, count c)
  case c of C {field = f} -> print f
  case C 'y' 3 of C f n -> print (f, n)
  print (count c {count = 5}, field c {field = 'z'})
  print (name (Circle "c" 1), name (Square 2 "s"), radius (Circle "c" 7), side (Square 9 "s"))
  print (Circle {name = "round", radius = 3})
  print (x (V 1 2) + y (V 1 2))
  print (eval (N 4), eval (B True), number (N 9))
  print (unW (W 'x'))
  print (Box 'x', Box 1 == Box 1)
  case P 'x' True of P a b -> print (a, b)
  case App (Just 'x') of App m -> print m
  print (Number 1, Number 1 == Number 1, Truth True == Truth False)
  print (Fork (Leaf 1) (Leaf 2), Fork (Leaf 1) (Leaf 2) == Fork (Leaf 1) (Leaf 3))
  print (One 1)
