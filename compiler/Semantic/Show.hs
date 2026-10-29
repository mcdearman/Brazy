-- |
-- A @Show@ class for semantic trees, which are indexed by layout, stage
-- and scope: one instance shows a tree at any of them.
module Semantic.Show where

import Data.Kind (Constraint, Type)
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type Show :: (Layout -> Stage -> Environment -> Type) -> Constraint
class Show term where
  showsPrec :: Int -> term layout stage scope -> ShowS
