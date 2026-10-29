-- |
-- Checking what comes after a lambda's first parameter: its other
-- parameters, one at a time, and what it gives once it has them all. The
-- first parameter is checked in "Semantic.Check.Go.Expression".
module Semantic.Check.Go.Lambda where

import Control.Monad.ST (ST)
import Core.Tree.Type ((-#>))
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Type
import Core.Type.Functor (shiftLogical)
import Semantic.Check.Context (Context)
import {-# SOURCE #-} qualified Semantic.Check.Go.Expression as Expression
import qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Lambda as Semantic
import qualified Semantic.Tree.Lambda as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Lambda Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Lambda Group Check scope))
check context typex = \case
  Semantic.Plain {plain} -> do
    plain <- Expression.check context typex plain
    pure $ do
      plain <- plain
      pure Solved.Plain {plain}
  Semantic.Bound {boundPosition, parameter, body} -> do
    parameterType <- Unify.fresh Core.typex
    resultType <- Unify.fresh Core.typex
    -- What the function is to be is said before its parameter is looked
    -- at, as for the first parameter of a lambda.
    Unify.unify context boundPosition typex (parameterType -#> resultType)
    parameter <- Pattern.check context Type.lazy parameterType parameter
    body <- check (Pattern.augment parameter context) (shiftLogical resultType) body
    Pattern.sealed context parameter [parameterType, resultType]
    pure $ do
      parameter <- Pattern.solve context parameter
      body <- body
      pure Solved.Bound {boundPosition, parameter, body}
