{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What a GADT declaration says, as one piece of a type declaration: its
-- brand, parameters, constructors and record selectors.
module Semantic.Resolve.Temporary.Partial.More.GADT where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Resolve.Temporary.Complete.GADTConstructor (GADTConstructor)
import Semantic.Resolve.Temporary.Complete.Selector (Selector)
import Semantic.Stage (Resolve)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

data GADT scope = GADT
  { brand :: !Brand,
    parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
    gadtConstructors :: !(Strict.Vector (GADTConstructor scope)),
    selectors :: !(Strict.Vector Selector)
  }
