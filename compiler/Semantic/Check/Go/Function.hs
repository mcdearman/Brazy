module Semantic.Check.Go.Function where

import Control.Monad.ST (ST)
import Core.Tree.Type ((-#>))
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Type
import Core.Type.Functor (shiftLogical)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Go.RightHandSide as RightHandSide
import qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Function as Semantic (Function (..))
import qualified Semantic.Tree.Function as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Function Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Function Group Check scope))
check context typex = \case
  Semantic.Plain {rightHandSide} -> do
    rightHandSide <- RightHandSide.check context typex rightHandSide
    pure $ do
      rightHandSide <- rightHandSide
      pure Solved.Plain {rightHandSide}
  Semantic.Bound {functionPosition, patternx, function} -> do
    argument <- Unify.fresh Core.typex
    result <- Unify.fresh Core.typex
    Unify.unify context functionPosition typex (argument -#> result)
    patternx <- Pattern.check context Type.lazy argument patternx
    function <- check (Pattern.augment patternx context) (shiftLogical result) function
    Pattern.sealed context patternx [argument, result]
    pure $ do
      patternx <- Pattern.solve patternx
      function <- function
      pure Solved.Bound {functionPosition, patternx, function}
