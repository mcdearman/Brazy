-- |
-- Strict vectors meant to have at least one element. 'Vector1' is only
-- another name for a strict vector: the length is not checked, and
-- 'uncons' fails on an empty one.
module Data.Strict.Vector1 where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector

type Vector1 = Strict.Vector

fromList' :: a -> [a] -> Vector1 a
fromList' head tail = Strict.Vector.fromList (head : tail)

fromNonEmpty :: NonEmpty.NonEmpty a -> Vector1 a
fromNonEmpty (head NonEmpty.:| tail) = fromList' head tail

uncons :: Vector1 a -> (a, Strict.Vector a)
uncons = fromJust . Strict.Vector.uncons
