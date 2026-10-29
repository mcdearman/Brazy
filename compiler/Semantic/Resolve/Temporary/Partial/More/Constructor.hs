{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- Where a data constructor is declared, as one piece of a constructor
-- declaration: the type it is of and its place among that type's
-- constructors, its fields, what each selector of the type reads from it,
-- and whether a match on it can fail.
module Semantic.Resolve.Temporary.Partial.More.Constructor where

import Data.Map (Map)
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Stage (Resolve)
import Semantic.Tree.Pattern (Failable)
import Syntax.Variable (Variable)

data Constructor = Constructor
  { typeIndex :: !Int,
    constructorIndex :: !Int,
    fields :: !(Map Variable Int),
    selections :: !(Strict.Vector (Strict.Maybe Int)),
    failable :: !(Failable Resolve)
  }
