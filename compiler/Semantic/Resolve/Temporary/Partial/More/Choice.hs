{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A variable bound by a pattern binding, as one piece of a term
-- declaration: the shared declaration that holds the right-hand side, the
-- pattern, and where in it the variable is bound.
module Semantic.Resolve.Temporary.Partial.More.Choice where

import qualified Semantic.Index.Term as Term (Bound (..), Index)
import Semantic.Stage (Resolve)
import Semantic.Tree.Pattern (Pattern)
import Syntax.Position (Position)

data Choice scope = Choice
  { temporary :: !Int,
    index :: Term.Index scope,
    bound :: Term.Bound,
    patternx :: Pattern Position Resolve scope
  }
