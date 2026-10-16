module Main (main) where

-- Functor, Foldable and Traversable derived from the classes, whatever the
-- module has in scope: the Prelude here is only qualified, as in Freddy's
-- example, and the module has names of its own that a written-out
-- instance would have taken.

import Prelude ()
import qualified Prelude as P

data MyType a = MyType a
  deriving (P.Show, P.Functor, P.Foldable, P.Traversable)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (P.Show, P.Functor, P.Foldable, P.Traversable)

-- The last parameter, the whole of a field, inside a list, a Maybe, a pair
-- and another type's own instance; and fields it is not in.
data Mixed b a = Mixed P.Int a [a] (P.Maybe a) (b, a) (Tree a) [P.Maybe a] b
  deriving (P.Show, P.Functor, P.Foldable, P.Traversable)

-- What a type applied to the parameter requires.
data Wrap f a = Wrap (f a)
  deriving (P.Functor, P.Foldable)

newtype Box a = Box {unbox :: a}
  deriving (P.Show, P.Functor, P.Foldable, P.Traversable)

data Empty a
  deriving (P.Functor, P.Foldable)

-- Names a written-out instance would have used.
fmap :: P.Int
fmap = 0

foldr :: P.Int
foldr = 0

traverse :: P.Int
traverse = 0

pure :: P.Int
pure = 0

tree :: Tree P.Int
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

main :: P.IO ()
main = do
  P.print (P.fmap (P.+ 1) (MyType 1 :: MyType P.Int), P.sum (MyType (5 :: P.Int)))
  P.print (P.fmap (P.* 10) tree, P.foldr (:) [] tree, P.length tree, P.maximum tree)
  P.print (P.traverse (\x -> if x P.> 0 then P.Just x else P.Nothing) tree)
  let mixed = Mixed 7 1 [2, 3] (P.Just 4) ('k', 5) tree [P.Just 6, P.Nothing] 'z' :: Mixed P.Char P.Int
  P.print (P.fmap P.show mixed)
  P.print (P.foldr (:) [] mixed, P.sum mixed, P.elem 6 mixed)
  P.print (P.traverse (\x -> [x, x P.+ 100]) (Box (1 :: P.Int)), P.sequenceA (MyType [1, 2 :: P.Int]))
  P.print (P.foldr (:) [] (P.fmap P.not (Wrap (P.Just P.True))), P.length (Wrap [1, 2, 3 :: P.Int]))
  P.print (fmap P.+ foldr P.+ traverse P.+ pure)
