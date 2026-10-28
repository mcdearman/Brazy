module Semantic.Check.Go.Expression where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Expression as Semantic
import qualified Semantic.Tree.Expression as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Expression Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Expression Group Check scope))
