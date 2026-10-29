module Semantic.Check.Go.RightHandSide where

import Control.Monad.ST (ST)
import Core.Type.Functor (shiftLogical)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Go.Body as Body
import {-# SOURCE #-} qualified Semantic.Check.Go.Declarations as Declarations
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.RightHandSide as Semantic (RightHandSide (..))
import qualified Semantic.Tree.RightHandSide as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.RightHandSide Group Resolve scope ->
  ST s (Unify.Solve s (Solved.RightHandSide Group Check scope))
check context typex Semantic.RightHandSide {body, declarations} = do
  context Declarations.:& declarations <- Declarations.check context declarations
  body <- Body.check context (shiftLogical typex) body
  pure $ do
    body <- body
    declarations <- declarations
    pure Solved.RightHandSide {body, declarations}
