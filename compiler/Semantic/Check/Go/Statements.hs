module Semantic.Check.Go.Statements where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Type
import Core.Type.Functor (shiftLogical)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Go.Declarations as Declarations
import {-# SOURCE #-} qualified Semantic.Check.Go.Expression as Expression
import qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Statements as Semantic
import qualified Semantic.Tree.Statements as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Statements Semantic.Guard Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Statements Solved.Guard Group Check scope))
check context typex = \case
  Semantic.Done {startPosition, done} -> do
    done <- Expression.check context typex done
    pure $ do
      done <- done
      pure Solved.Done {startPosition, done}
  Semantic.Run {startPosition, effect, after} -> do
    effect <- Expression.check context Core.bool effect
    after <- check context typex after
    pure $ do
      effect <- effect
      after <- after
      pure
        Solved.Run
          { startPosition,
            evidence = Solved Semantic.Bool,
            effect,
            after
          }
  Semantic.Bind {startPosition, patternx, effect, thenx, fail} -> do
    binder <- Unify.fresh Core.typex
    effect <- Expression.check context binder effect
    patternx <- Pattern.check context Type.lazy binder patternx
    thenx <- check (Pattern.augment patternx context) (shiftLogical typex) thenx
    Pattern.sealed context patternx [binder, typex]
    pure $ do
      patternx <- Pattern.solve patternx
      effect <- effect
      thenx <- thenx
      pure
        Solved.Bind
          { startPosition,
            patternx,
            evidence = Solved Semantic.Bool,
            effect,
            thenx,
            fail
          }
  Semantic.Let {startPosition, declarations, body} -> do
    context Declarations.:& declarations <- Declarations.check context declarations
    body <- check context (shiftLogical typex) body
    pure $ do
      declarations <- declarations
      body <- body
      pure Solved.Let {startPosition, declarations, body}
