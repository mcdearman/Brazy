module Semantic.Check.Go.Do where

import Control.Monad.ST (ST)
import Core.Tree.Type ((#), (-#>))
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Type
import Core.Type.Functor (shiftLogical)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Go.Declarations as Declarations
import {-# SOURCE #-} qualified Semantic.Check.Go.Expression as Expression
import qualified Semantic.Check.Temporary.Pattern as Pattern
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Pattern as Semantic.Pattern
import qualified Semantic.Tree.Statements as Semantic
import qualified Semantic.Tree.Statements as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Statements Semantic.Do Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Statements Solved.Do Group Check scope))
check context typex = \case
  Semantic.Done {startPosition, done} -> do
    done <- Expression.check context typex done
    pure $ do
      done <- done
      pure Solved.Done {startPosition, done}
  Semantic.Run {startPosition, effect, after} -> do
    monad <- Unify.fresh $ Core.typex -#> Core.typex
    evidence <- Unify.constrain context startPosition Type2.Monad monad
    ignore <- Unify.fresh Core.typex
    kept <- Unify.fresh Core.typex
    Unify.unify context startPosition (monad # kept) typex
    effect <- Expression.check context (monad # ignore) effect
    after <- check context typex after
    pure $ do
      evidence <- Unify.solveEvidence startPosition evidence
      effect <- effect
      after <- after
      pure
        Solved.Run
          { startPosition,
            evidence = Solved $ Semantic.Monad evidence,
            effect,
            after
          }
  Semantic.Bind {startPosition, patternx, effect, thenx} -> do
    monad <- Unify.fresh $ Core.typex -#> Core.typex
    input <- Unify.fresh Core.typex
    output <- Unify.fresh Core.typex
    let neverFail = Semantic.Pattern.neverFails patternx
        classx
          | neverFail = Type2.Monad
          | otherwise = Type2.MonadFail
    evidence <- Unify.constrain context startPosition classx monad
    Unify.unify context startPosition (monad # output) typex
    effect <- Expression.check context (monad # input) effect
    patternx <- Pattern.check context Type.lazy input patternx
    thenx <- check (Pattern.augment patternx context) (shiftLogical $ monad # output) thenx
    Pattern.sealed context patternx [input, monad # output]
    pure $ do
      patternx <- Pattern.solve patternx
      evidence <- Unify.solveEvidence startPosition evidence
      effect <- effect
      thenx <- thenx
      pure
        Solved.Bind
          { startPosition,
            patternx,
            evidence = Solved $ Semantic.Monad evidence,
            effect,
            thenx,
            fail = not neverFail
          }
  Semantic.Let {startPosition, declarations, body} -> do
    context Declarations.:& declarations <- Declarations.check context declarations
    body <- check context (shiftLogical typex) body
    pure $ do
      declarations <- declarations
      body <- body
      pure Solved.Let {startPosition, declarations, body}
