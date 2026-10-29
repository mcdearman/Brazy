{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A type variable that a declaration or a @forall@ brings into scope, with
-- its kind still being worked out. Solving it gives the kind it was found
-- to have.
module Semantic.Check.Temporary.TypePattern where

import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.TypePattern as Solved
import qualified Semantic.Unify as Unify
import Syntax.Lexer (VariableIdentifier)
import Syntax.Position (Position)

data TypePattern s scope = TypePattern
  { position :: !Position,
    typex :: !(Unify.Type s scope),
    name :: !VariableIdentifier
  }

solve :: Context s scope -> TypePattern s scope -> Unify.Solve s (Solved.TypePattern Position Check scope)
solve context TypePattern {position, name, typex} = do
  typex <- Unify.solve context position typex
  pure
    Solved.TypePattern
      { position,
        name,
        typex = Solved typex
      }
