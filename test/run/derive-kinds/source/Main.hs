module Main where

import Data.Kind (Type)

-- A deriving clause on a type whose parameter is not a type of values. The
-- class cannot be asked of such a parameter, and is not.

data Scope = Global | Local Scope

type Index :: Scope -> Type
data Index scope = Index Int
  deriving (Show, Eq, Ord)

type Term :: Scope -> Type
data Term scope = Variable (Index scope) | Call (Term scope) (Term scope)
  deriving (Show, Eq)

-- One parameter of each kind: the class is asked of the second only.
type Tagged :: Scope -> Type -> Type
data Tagged scope value = Tagged (Index scope) value
  deriving (Show, Eq, Ord)

-- A parameter that stands for a type that takes an argument.
type Wrap :: (Type -> Type) -> Type
data Wrap f = Wrap Int
  deriving (Show)

main :: IO ()
main = do
  print (Call (Variable (Index 1)) (Variable (Index 2)) :: Term 'Global)
  print ((Variable (Index 1) :: Term ('Local 'Global)) == Variable (Index 1))
  print (Tagged (Index 3) 'x' :: Tagged 'Global Char)
  print ((Tagged (Index 3) 1 :: Tagged 'Global Int) < Tagged (Index 3) 2)
  print (Wrap 5 :: Wrap Maybe)
