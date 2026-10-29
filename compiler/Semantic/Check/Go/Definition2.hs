module Semantic.Check.Go.Definition2 where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Type
import Semantic.Check.Binding.Term (TermBinding (TermBinding), Type (..))
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Go.Definition as Definition
import qualified Semantic.Check.Go.RightHandSide as RightHandSide
import Semantic.Check.Scheme (instanciate)
import Semantic.Check.Temporary.Pattern (Pattern)
import qualified Semantic.Check.Temporary.Pattern as Pattern
import qualified Semantic.Index.Table.Term as Term ((!))
import Semantic.Index.Term (Bound)
import qualified Semantic.Index.Term as Term (Index)
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Combinators.Inferred as Inferred
import Semantic.Tree.Definition2 (Annotated, Inferred, Single)
import qualified Semantic.Tree.Definition2 as Semantic
import qualified Semantic.Tree.Definition2 as Solved (Choice (..), Definition2 (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

-- |
-- Check a definition with a type it is said to have. The position is the
-- definition's, which what it names are said to be at.
checkManual ::
  Position ->
  Context s (Local ':+ scopes) ->
  Unify.Type s (Local ':+ scopes) ->
  Semantic.Definition2 source Annotated Group Resolve scopes ->
  ST s (Unify.Solve s (Solved.Definition2 source Annotated Group Check (Local ':+ scopes)))
checkManual _ context typex (Semantic.Definition definition) = do
  definition <- Definition.check context typex (shift definition)
  pure $ Solved.Definition <$> definition
checkManual _ _ _ (Semantic.Foreign call) = pure $ pure $ Solved.Foreign call
checkManual position context typex (Semantic.Exported call definition) = do
  definition <- checkManual position context typex definition
  pure $ Solved.Exported call <$> definition
checkManual _ context typex (Semantic.Scoped definition) = do
  definition <- Definition.check context typex definition
  pure $ Solved.Definition <$> definition
checkManual outer context@Context {termEnvironment} typex declaration = case declaration of
  Semantic.Piece Semantic.Choice {position, index, patternx, bound} -> do
    let TermBinding binding = termEnvironment Term.! shift index
    (full, instanciation) <-
      binding >>= \case
        Wobbly typex -> Unify.instanciate context position typex
        Rigid scheme -> instanciate context position scheme
    patternx <- Pattern.check context Type.lazy full (shift patternx)
    Pattern.bindable position patternx
    (levity, typex') <- pure $ patternx Pattern.! bound
    Unify.unify context position typex typex'
    pure $ piece context outer (shift index) instanciation levity patternx bound

-- |
-- Check a definition whose type is worked out. The position is the
-- definition's, which what it names are said to be at.
checkAuto ::
  Position ->
  Context s scopes ->
  Unify.Type s scopes ->
  Semantic.Definition2 source Inferred Group Resolve scopes ->
  ST s (Unify.Solve s (Solved.Definition2 source Inferred Group Check scopes))
checkAuto _ context typex (Semantic.Definition definition) = do
  definition <- Definition.check context typex definition
  pure $ Solved.Definition <$> definition
checkAuto _ context typex (Semantic.Shared definition) = do
  definition <- RightHandSide.check context typex definition
  pure $ Solved.Shared <$> definition
checkAuto outer context@Context {termEnvironment} typex declaration = case declaration of
  Semantic.Piece Semantic.Choice {position, index, patternx, bound} -> do
    let TermBinding binding = termEnvironment Term.! index
    (full, instanciation) <-
      binding >>= \case
        Wobbly typex -> Unify.instanciate context position typex
        Rigid scheme -> instanciate context position scheme
    patternx <- Pattern.check context Type.lazy full patternx
    Pattern.bindable position patternx
    (levity, typex') <- pure $ patternx Pattern.! bound
    Unify.unify context position typex typex'
    pure $ piece context outer index instanciation levity patternx bound

-- |
-- One of the things a pattern that is defined names.
piece ::
  Context s scope ->
  Position ->
  Term.Index scope ->
  Unify.Instanciation s scope ->
  Unify.Type s scope ->
  Pattern s scope ->
  Bound ->
  Unify.Solve s (Solved.Definition2 Single mark Group Check scope)
piece context position index instanciation levity patternx bound = do
  instanciation <- Unify.solveEvidence context position instanciation
  patternx <- Pattern.solve context patternx
  levity <- Unify.solve context position levity
  pure $
    Solved.Piece
      Solved.Choice
        { position,
          index,
          instanciation = Inferred.Solved instanciation,
          levity = Inferred.Solved levity,
          patternx,
          bound
        }
