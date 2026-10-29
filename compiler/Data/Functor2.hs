-- |
-- A functor over a type constructor rather than a type: 'fmap2' changes the
-- @f@ that a structure is built over, by a natural transformation. The
-- semantic trees and "Graph.Topological" use it.
module Data.Functor2 where

import Data.NaturalTransformation (NaturalTransformation (..))

class Functor2 f where
  fmap2 :: NaturalTransformation a b -> f a -> f b
