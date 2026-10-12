{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.LocalBinding where

import qualified Data.Kind as Kind
import Semantic.Scope (Environment)

type role Equality nominal nominal

type Equality :: Kind.Type -> Environment -> Kind.Type
data Equality s scope
