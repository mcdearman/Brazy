-- |
-- What is kept of a term declaration to print it back as syntax: its
-- qualified name, or, for the value a pattern binding shares among its
-- variables, which has no name of its own, 'SharedTermBinding'. See
-- "Semantic.Label.Context".
module Semantic.Label.Binding.Term where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import qualified Semantic.Shift0 as Shift0
import Syntax.Variable (QualifiedVariable)

type TermBinding :: Environment -> Type
data TermBinding scope
  = TermBinding
      { name :: !QualifiedVariable
      }
  | SharedTermBinding

instance Shift0.Functor TermBinding where
  map _ TermBinding {name} = TermBinding {name}
  map _ SharedTermBinding = SharedTermBinding
