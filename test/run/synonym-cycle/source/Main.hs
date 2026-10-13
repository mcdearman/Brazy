module Main where

-- A name for a type, and the types it is a name by way of, which are
-- defined by way of the name: none of them has its kind written.
type Type s scope = TypeF (Logical s scope) scope

data TypeF logical scope
  = Known Int
  | Unknown logical
  | Apply (TypeF logical scope) (TypeF logical scope)

data Logical s scope
  = Box (Box s scope)
  | Shifted (Logical s scope)

data Box s scope
  = Unsolved {kind :: Type s scope, name :: s}
  | Solved (Type s scope)

-- A name by way of a name.
type Pair a = Both a a

type Both a b = (Tree a, Tree b)

data Tree a = Leaf a | Node (Pair a)

-- A name that takes a type that takes a type.
type Twice f a = f (Wrap f a)

data Wrap f a = Wrap (Twice f a) | Stop a

size :: Type s scope -> Int
size = \case
  Known n -> n
  Unknown logical -> inside logical
  Apply function argument -> size function + size argument

inside :: Logical s scope -> Int
inside = \case
  Box Unsolved {kind} -> 1 + size kind
  Box (Solved typex) -> size typex
  Shifted logical -> inside logical

total :: Tree Int -> Int
total = \case
  Leaf n -> n
  Node (left, right) -> total left + total right

depth :: Wrap Maybe a -> Int
depth = \case
  Stop _ -> 0
  Wrap Nothing -> 1
  Wrap (Just inner) -> 1 + depth inner

main :: IO ()
main = do
  let boxed = Unknown (Shifted (Box (Unsolved {kind = Known 2, name = 'n'}))) :: Type Char ()
  print (size (Apply boxed (Unknown (Box (Solved (Apply (Known 3) (Known 4)))))))
  print (total (Node (Node (Leaf 1, Leaf 2), Leaf 3)))
  print (depth (Wrap (Just (Wrap (Just (Stop 'x'))))))
