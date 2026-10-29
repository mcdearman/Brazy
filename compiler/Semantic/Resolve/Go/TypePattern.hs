-- |
-- Resolves a type variable where it is bound, as in a @forall@ or the head
-- of a declaration: its name and position carry over, and its kind is left
-- to be inferred.
module Semantic.Resolve.Go.TypePattern where

import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.TypePattern (TypePattern (..))
import qualified Syntax.Tree.TypePattern as Syntax

resolve :: Syntax.TypePattern position -> TypePattern position Resolve scope
resolve Syntax.TypePattern {position, name} =
  TypePattern
    { position,
      name,
      typex = Inferred
    }
