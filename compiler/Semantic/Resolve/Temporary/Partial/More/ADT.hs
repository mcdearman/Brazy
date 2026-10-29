{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What an ordinary data declaration says, as one piece of a type
-- declaration: its brand, parameters, constructors and record selectors.
module Semantic.Resolve.Temporary.Partial.More.ADT where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Resolve.Temporary.Complete.Constructor (Constructor)
import Semantic.Resolve.Temporary.Complete.Selector (Selector)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Stage (Resolve)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

data ADT scope = ADT
  { brand :: !Brand,
    parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
    constructors :: !(Strict.Vector (Constructor (Local ':+ scope))),
    selectors :: !(Strict.Vector Selector)
  }
