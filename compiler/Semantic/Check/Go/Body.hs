module Semantic.Check.Go.Body where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import {-# SOURCE #-} qualified Semantic.Check.Go.Expression as Expression
import {-# SOURCE #-} qualified Semantic.Check.Go.Statements as Statements
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Body as Semantic (Body (..))
import qualified Semantic.Tree.Body as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Body Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Body Group Check scope))
check context typex = \case
  Semantic.Body expression -> do
    expression <- Expression.check context typex expression
    pure $ Solved.Body <$> expression
  Semantic.Guards statements -> do
    statements <- traverse (Statements.check context typex) statements
    pure $ Solved.Guards <$> sequence statements
