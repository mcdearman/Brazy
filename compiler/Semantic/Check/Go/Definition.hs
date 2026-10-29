module Semantic.Check.Go.Definition where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context, postponing)
import qualified Semantic.Check.Go.Function as Function
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Definition as Semantic (Definition (..))
import qualified Semantic.Tree.Definition as Solved
import qualified Semantic.Unify as Unify

-- What is defined inside a definition and put off is checked when the rest
-- of the definition has been.
check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Definition Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Definition Group Check scope))
check context typex definition = postponing context $ \context -> clauses context typex definition

clauses ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Definition Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Definition Group Check scope))
clauses context typex = \case
  Semantic.Alternative {definition = function, alternative} -> do
    function <- Function.check context typex function
    alternative <- clauses context typex alternative
    pure $ do
      definition <- function
      alternative <- alternative
      pure Solved.Alternative {definition, alternative}
  Semantic.Definition function -> do
    function <- Function.check context typex function
    pure $ Solved.Definition <$> function
