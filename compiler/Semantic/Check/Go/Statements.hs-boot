module Semantic.Check.Go.Statements where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Statements as Semantic
import qualified Semantic.Tree.Statements as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Statements Semantic.Guard Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Statements Solved.Guard Group Check scope))
