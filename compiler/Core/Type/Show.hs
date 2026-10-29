-- |
-- 'Show' for a tree of Core types whatever its logical variables are, so
-- that a tree that holds one, like @ForallOver@, can show it.
module Core.Type.Show where

import Data.Kind (Constraint, Type)
import Semantic.Scope (Environment)

type Show :: (Type -> Environment -> Type) -> Constraint
class Show typef where
  showsPrec :: (Prelude.Show logical) => Int -> typef logical scope -> ShowS
