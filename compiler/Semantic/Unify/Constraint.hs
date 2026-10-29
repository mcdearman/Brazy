-- |
-- A type synonym: a Core class constraint whose types may hold the type
-- checker's unknowns.
module Semantic.Unify.Constraint where

import Core.Tree.Constraint (ConstraintF (..))
import {-# SOURCE #-} Semantic.Unify.Type (Logical)

type Constraint s scope = ConstraintF (Logical s scope) scope
