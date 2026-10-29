{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What is kept of a type variable to print it back as syntax: its name.
-- See "Semantic.Label.Context".
module Semantic.Label.Binding.Local where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import qualified Semantic.Shift0 as Shift0
import Syntax.Variable (VariableIdentifier)

type LocalBinding :: Environment -> Type
newtype LocalBinding scope = LocalBinding
  { name :: VariableIdentifier
  }

instance Shift0.Functor LocalBinding where
  map _ LocalBinding {name} = LocalBinding {name}
