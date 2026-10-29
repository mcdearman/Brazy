-- |
-- The stages of a semantic tree, as a type-level tag: 'Resolve', once
-- names are resolved to indexes, and 'Check', once types are checked.
-- 'Equal' witnesses that two stages are the same; a field of type
-- 'Unsupported' marks a construct the checker does not take, which can
-- then only be in a resolved tree.
module Semantic.Stage where

import Data.Kind (Constraint, Type)

data Stage
  = Resolve
  | Check

type Resolve = 'Resolve

type Check = 'Check

-- |
-- For constructs that are currently unsupported in Semantic
type Unsupported = Equal Resolve

type Equal :: Stage -> Stage -> Type
data Equal stage1 stage2 where
  Refl :: Equal stage stage

instance Show (Equal stage1 stage2) where
  showsPrec _ Refl = showString "Refl"

instance Eq (Equal stage1 stage2) where
  Refl == Refl = True

type IsResolve :: Stage -> Constraint
class IsResolve stage where
  isResolve :: Unsupported stage

instance IsResolve 'Resolve where
  isResolve = Refl
