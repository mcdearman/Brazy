-- |
-- Checking one alternative of a @case@ or a lambda case: its pattern,
-- against the type of what is taken apart, and its right-hand side, against
-- the type the whole gives, with what the pattern binds in scope.
module Semantic.Check.Go.Alternative where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Core.Type
import Core.Type.Functor (shiftLogical)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Go.RightHandSide as RightHandSide
import qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Alternative as Semantic
import qualified Semantic.Tree.Alternative as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Unify.Type s scope ->
  Semantic.Alternative Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Alternative Group Check scope))
check context typex binder Semantic.Alternative {parameter, rightHandSide} = do
  parameter <- Pattern.check context Core.Type.lazy binder parameter
  rightHandSide <- RightHandSide.check (Pattern.augment parameter context) (shiftLogical typex) rightHandSide
  Pattern.sealed context parameter [binder, typex]
  pure $ do
    parameter <- Pattern.solve context parameter
    rightHandSide <- rightHandSide
    pure Solved.Alternative {parameter, rightHandSide}
