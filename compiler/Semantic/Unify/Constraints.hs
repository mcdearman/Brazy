-- |
-- A type synonym: the Core constraints of a polymorphic type, whose types
-- may hold the type checker's unknowns.
module Semantic.Unify.Constraints where

import Core.Tree.Constraints (ConstraintsF (..))
import {-# SOURCE #-} Semantic.Unify.Type (Logical)

type Constraints s scope = ConstraintsF (Logical s scope) scope
