-- |
-- Strict vectors meant to have at least two elements. 'Vector2' is only
-- another name for a strict vector: the length is not checked.
module Data.Strict.Vector2 where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector

type Vector2 = Strict.Vector

fromList'' :: a -> a -> [a] -> Vector2 a
fromList'' head1 head2 tail = Strict.Vector.fromList (head1 : head2 : tail)
