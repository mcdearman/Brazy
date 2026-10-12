module Main where

import Data.Kind (Type)

-- Types that only say what stage something is at.
data Stage = Resolve | Check

-- A value that is there at one stage and not at the other.
type Inferred :: Type -> Stage -> Type
data Inferred value stage where
  Unknown :: Inferred value 'Resolve
  Known :: !value -> Inferred value 'Check

get :: Inferred value 'Check -> value
get (Known value) = value

describe :: (Show value) => Inferred value stage -> String
describe = \case
  Unknown -> "unknown"
  Known value -> "known " ++ show value

-- How to get from one list of scopes to another, as the compiler has it.
data Scope = Global | Local Scope

type Category :: Scope -> Scope -> Type
data Category from to where
  Id :: Category scope scope
  Shift :: Category scope ('Local scope)
  (:.) :: Category middle to -> Category from middle -> Category from to

infixr 9 :.

-- An index into a list of scopes.
type Index :: Scope -> Type
data Index scope where
  Here :: !Int -> Index ('Local scope)
  There :: !(Index scope) -> Index ('Local scope)

depth :: Index scope -> Int
depth (Here n) = n
depth (There index) = 1 + depth index

move :: Category from to -> Index from -> Index to
move Id index = index
move Shift index = There index
move (after :. before) index = move after (move before index)

class Functor' f where
  map' :: Category from to -> f from -> f to

instance Functor' Index where
  map' = move

main :: IO ()
main = do
  putStrLn (describe (Unknown :: Inferred Int 'Resolve))
  putStrLn (describe (Known 3))
  print (get (Known 'x'))
  print (depth (move (Shift :. Shift :. Id) (Here 5)))
  print (depth (map' Shift (There (Here 0))))
