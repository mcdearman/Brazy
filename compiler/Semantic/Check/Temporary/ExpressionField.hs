module Semantic.Check.Temporary.ExpressionField where

import Control.Monad.ST (ST)
import {-# SOURCE #-} Semantic.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Expression as Expression (solve)
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Expression as Given (Expression)
import qualified Semantic.Tree.ExpressionField as Semantic
import qualified Semantic.Tree.ExpressionField as Solved
import qualified Semantic.Unify as Unify

data Field s scope
  = Field
  { index :: !Int,
    expression :: !(Expression s scope)
  }

-- |
-- Check what a record is given for one of the things it holds. How that is
-- checked depends on what the constructor holds there, which whoever asks
-- knows.
check ::
  (Int -> Given.Expression Group Resolve scope -> ST s (Expression s scope)) ->
  Semantic.Field Group Resolve scope ->
  ST s (Field s scope)
check fill Semantic.Field {index, expression} = do
  expression <- fill index expression
  pure
    Field
      { index,
        expression
      }

solve :: Field s scope -> Unify.Solve s (Solved.Field Group Check scope)
solve Field {index, expression} = do
  expression <- Expression.solve expression
  pure $ Solved.Field {index, expression}
