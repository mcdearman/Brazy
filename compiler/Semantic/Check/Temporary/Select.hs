module Semantic.Check.Temporary.Select where

import Control.Monad.ST (ST)
import {-# SOURCE #-} Semantic.Check.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Expression as Expression (solve)
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Expression as Given (Expression)
import qualified Semantic.Tree.Select as Semantic
import qualified Semantic.Unify as Unify

data Select s scope
  = Select
  { pick :: !Int,
    update :: !(Expression s scope)
  }

-- |
-- Check what a value is given in place of one of the things it holds. How
-- that is checked depends on what it holds there, which whoever asks knows.
check ::
  (Int -> Given.Expression Group Resolve scope -> ST s (Expression s scope)) ->
  Semantic.Select Group Resolve scope ->
  ST s (Select s scope)
check fill Semantic.Select {pick, update} = do
  update <- fill pick update
  pure Select {pick, update}

solve :: Select s scope -> Unify.Solve s (Semantic.Select Group Check scope)
solve Select {pick, update} = do
  update <- Expression.solve update
  pure Semantic.Select {pick, update}
