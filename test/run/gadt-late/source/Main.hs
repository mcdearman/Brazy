module Main where

import Data.Kind (Type)

data Scope = Global | Scope :+ Scope

infixr 5 :+

data Local

type Index :: Scope -> Type
data Index scope where
  Here :: Int -> Index (scope ':+ scopes)
  There :: Index scopes -> Index (scope ':+ scopes)

type Category :: Scope -> Scope -> Type
data Category scope scope' where
  Shift :: Category scopes (scope ':+ scopes)
  Over :: Category scope scope' -> Category (s ':+ scope) (s ':+ scope')
  Twice :: Category scopes (a ':+ b ':+ scopes)
  Swap :: Category (a ':+ b ':+ scopes) (b ':+ a ':+ scopes)

depth :: Index scope -> Int
depth = \case
  Here n -> n
  There index -> 1 + depth index

twice :: Category scopes (a ':+ b ':+ scopes)
twice = Twice

-- What `general` is depends on which shift was matched, and its type is
-- told only by how it is used.
apply :: Category scope scope' -> Index scope -> Index scope'
apply Shift index = There index
apply (Over category) index = case index of
  Here n -> Here n
  There index -> There (apply category index)
apply category index = apply general index
  where
    general = case category of
      Twice -> twice
      Swap -> Swap

-- A function by cases with no type written: what it is given is known from
-- its use.
lower :: Int -> Index (scope ':+ scopes) -> Maybe (Index scopes)
lower limit index = go index
  where
    go = \case
      Here n | n < limit -> Nothing
      Here _ -> Nothing
      There inner -> Just inner

main :: IO ()
main = do
  print (depth (apply Shift (Here 3 :: Index ('Global ':+ 'Global))))
  print (depth (apply (Over Shift) (There (Here 1) :: Index ('Global ':+ 'Global ':+ 'Global))))
  print (fmap depth (lower 2 (There (There (Here 5)) :: Index ('Global ':+ 'Global ':+ 'Global ':+ 'Global))))
  print (fmap depth (lower 2 (Here 5 :: Index ('Global ':+ 'Global))))
