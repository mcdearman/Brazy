{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What is kept of a type declaration to print it back as syntax: its
-- qualified name and the names of its constructors, for a constructor used
-- as a type. See "Semantic.Label.Context".
module Semantic.Label.Binding.Type where

import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Environment)
import qualified Semantic.Shift0 as Shift0
import Syntax.Variable (QualifiedConstructor, QualifiedConstructorIdentifier)

type TypeBinding :: Environment -> Type
data TypeBinding scope = TypeBinding
  { name :: !QualifiedConstructorIdentifier,
    constructorNames :: !(Strict.Vector QualifiedConstructor)
  }

instance Shift0.Functor TypeBinding where
  map _ TypeBinding {name, constructorNames} = TypeBinding {name, constructorNames}
