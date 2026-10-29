-- |
-- Checking the statements of a list comprehension: generators, each drawing
-- from a list, conditions, which are @Bool@, and @let@s, ending in an element
-- of the list it makes.
module Semantic.Check.Go.Comprehension where

import Control.Monad.ST (ST)
import Core.Tree.Type ((#))
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
import qualified Semantic.Tree.Pattern as Semantic.Pattern
import qualified Semantic.Tree.Statements as Semantic
import qualified Semantic.Tree.Statements as Solved
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Statements Semantic.Comprehension Group Resolve scope ->
  ST s (Unify.Solve s (Solved.Statements Solved.Comprehension Group Check scope))
check context typex = \case
  Semantic.Done {startPosition, done} -> do
    let monad = Core.listing
    inner <- Unify.fresh Core.typex
    done <- Expression.check context inner done
    Unify.unify context startPosition (monad # inner) typex
    pure $ do
      done <- done
      pure Solved.Done {startPosition, done}
  Semantic.Run {startPosition, effect, after} -> do
    let monad = Core.listing
    kept <- Unify.fresh Core.typex
    Unify.unify context startPosition (monad # kept) typex
    effect <- Expression.check context Core.bool effect
    after <- check context typex after
    pure $ do
      effect <- effect
      after <- after
      pure
        Solved.Run
          { startPosition,
            evidence = Solved Semantic.List,
            effect,
            after
          }
  Semantic.Bind {startPosition, patternx, effect, thenx} -> do
    let monad = Core.listing
    input <- Unify.fresh Core.typex
    output <- Unify.fresh Core.typex
    let neverFail = Semantic.Pattern.neverFails patternx
    Unify.unify context startPosition (monad # output) typex
    effect <- Expression.check context (monad # input) effect
    patternx <- Pattern.check context Type.lazy input patternx
    thenx <- check (Pattern.augment patternx context) (shiftLogical $ monad # output) thenx
    Pattern.sealed context patternx [input, monad # output]
    pure $ do
      patternx <- Pattern.solve context patternx
      effect <- effect
      thenx <- thenx
      pure
        Solved.Bind
          { startPosition,
            patternx,
            evidence = Solved Semantic.List,
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
