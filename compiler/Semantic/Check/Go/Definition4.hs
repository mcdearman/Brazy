module Semantic.Check.Go.Definition4 where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef)
import Core.Substitute (logicalType)
import qualified Core.Foreign as Foreign
import qualified Core.Tree.Constraints as Simple.Constraints (simplify)
import qualified Core.Tree.Forall as Forall (simplify)
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Simple (simplify)
import qualified Core.Tree.TypeLambda as Simple (TypeLambdaOver (..))
import Core.Type.Functor (shiftLogical)
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Error (cyclicalTypeChecking, strictDeclaration)
import qualified Graph.Topological as Topological
import qualified Core.Builtin as Builtin
import Core.Tree.TypeDeclaration (assumeData)
import Semantic.Check.Context (Context (..), groupTermBindings, postponed)
import qualified Semantic.Check.Binding.Type as TypeBinding
import qualified Semantic.Index.Table.Type as Table.Type
import qualified Semantic.Check.Go.Scheme as Solved.Scheme
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Temporary.Definition3 as Definition3
import qualified Semantic.Check.Temporary.Scheme as Scheme
import qualified Semantic.Layout as Layout
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Category (Shift))
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Definition2 as Definition2 (Definition2 (..), ForeignCall (..))
import qualified Semantic.Tree.Definition3 as Tree.Definition3 (Definition3 (..))
import qualified Semantic.Tree.Constraints as Constraints
import Semantic.Tree.Definition4 (Annotation (..), Definition4 (..))
import Semantic.Tree.Group (Element (..), Group (..), Set (..), Types (..))
import Semantic.Tree.Scheme as Solved (Scheme (..))
import qualified Semantic.Tree.TypePattern as TypePattern
import Semantic.Unify (SolveType)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

checkAnnotation ::
  Context s scope ->
  Position ->
  Solved.Scheme Position Check scope ->
  ( Context s (Local ':+ scope) ->
    Unify.Type s (Local ':+ scope) ->
    ST s (Unify.Solve s (typex (Local ':+ scope)))
  ) ->
  ST s (Unify.Solve s (Simple.TypeLambdaOver typex scope))
checkAnnotation
  context
  position
  Solved.Scheme
    { parameters,
      constraints,
      result
    }
  go =
    do
      let typex = logicalType $ Simple.simplify result
      context <- Solved.Scheme.augment position parameters constraints Mask.Runtime context
      definition <- go context typex
      pure $ do
        definition <- definition
        pure
          Simple.TypeLambdaOver
            { parameters = TypePattern.typex' <$> parameters,
              constraints = Simple.Constraints.simplify constraints,
              result = definition
            }

data Solve solve logical s scope = Solve
  { solve :: forall a. Unify.Solve s a -> ST s (solve a),
    logical ::
      forall typef.
      (SolveType typef) =>
      Position ->
      typef (Unify.Logical s scope) scope ->
      ST s (typef logical scope)
  }

delay :: Solve (Unify.Solve s) (Unify.Logical s scope) s scope
delay =
  Solve
    { solve = pure,
      logical = const pure
    }

now :: Solve Identity Void s scope
now =
  Solve
    { solve = fmap Identity . Unify.runSolve,
      logical = (.) Unify.runSolve . Unify.solve
    }

check ::
  (Functor solve) =>
  Position ->
  Solve solve logical s scope ->
  (declarations (ST s) -> Definition4 solve' logical locality (ST s) Layout.Group Check scope) ->
  (declarations (ST s) -> Context s scope) ->
  Definition4 Identity Void locality Identity Layout.Group Resolve scope ->
  Definition4 solve logical locality (Topological.Formula declarations s) Layout.Group Check scope
check position Solve {solve, logical} reflection information = \case
  Annotated (Identity scheme) ::: body ->
    Annotated
      Topological.Formula
        { cycle = cyclicalTypeChecking position,
          run = \declarations -> do
            let context = information declarations
            scheme <- Scheme.check context scheme
            scheme <- Unify.runSolve $ Scheme.solve context scheme
            pure scheme
        }
      ::: Topological.Formula
        { cycle = cyclicalTypeChecking position,
          run = \declarations -> do
            let context = information declarations
                self = reflection declarations
            case self of
              Annotated annotation ::: _ -> do
                scheme <- annotation
                case scheme of
                  Scheme {constraints} -> case constraints of
                    Constraints.Constraints {} -> pure ()
                    Constraints.Lazy -> pure ()
                    _ -> strictDeclaration position
                -- What a function of C is given and gives has to be what C
                -- has, which is known from its type, and from the
                -- definitions of the newtypes in it.
                definition <- case definition of
                  Tree.Definition3.Label info (Definition2.Foreign call@Definition2.ForeignCall {foreignPosition, target, symbol}) -> do
                    let Context {typeEnvironment} = context
                        content index = assumeData <$> TypeBinding.content (typeEnvironment Table.Type.! index)
                    marshalled <- Foreign.callThrough (Builtin.index pure content) foreignPosition target symbol (Forall.simplify scheme)
                    marshalled `seq` pure (Tree.Definition3.Label info (Definition2.Foreign call {Definition2.marshalled = Just marshalled}))
                  Tree.Definition3.Label info (Definition2.Exported call@Definition2.ForeignCall {foreignPosition, target, symbol} exported) -> do
                    let Context {typeEnvironment} = context
                        content index = assumeData <$> TypeBinding.content (typeEnvironment Table.Type.! index)
                    marshalled <- Foreign.callThrough (Builtin.index pure content) foreignPosition target symbol (Forall.simplify scheme)
                    marshalled `seq` pure (Tree.Definition3.Label info (Definition2.Exported call {Definition2.marshalled = Just marshalled} exported))
                  _ -> pure definition
                definition <- checkAnnotation context position scheme $ \context typex -> do
                  definition <- Definition3.checkManual context typex definition
                  definition <- pure $ Definition3.solve position definition
                  pure definition
                definition <- solve definition
                pure $ Implicit.Check <$> definition
              _ -> error "bad self annotation"
        }
    where
      Identity (Identity (Implicit.Resolve definition)) = body
  Link link id -> Link link id
  Group group ->
    Group
      Topological.Formula
        { cycle = cyclicalTypeChecking position,
          run = \declarations -> do
            let context = information declarations
            -- A definition's type requires what the definition requires, as
            -- the Haskell report has it, if every definition here is of a
            -- function. A definition that is not is of one type, so that
            -- it is worked out once.
            --
            -- A definition that uses itself, or another that is defined
            -- with it, is such a use of one type, and is given the evidence
            -- the definitions were given when they are taken apart again
            -- (see 'Shift.grouped').
            recursive <- newSTRef False
            let functions = all (\Element {element} -> Definition3.functional element) set
                requiring = pure functions
            -- A definition that takes apart a value of an indexed type, and
            -- is no function, is put off until what it is inside has been
            -- checked: by then how it is used has said what type it is, and
            -- it is checked against that. It is of that one type.
            waiting <- case (toList set, postponed context) of
              ([Element {element}], Just waiting) -> do
                indexed <- Definition3.indexed context element
                pure $ if indexed then Just waiting else Nothing
              _ -> pure Nothing
            types Unify.::: set <- Unify.generalizeBody position context requiring (null waiting) $ Unify.Generalize $ \context -> do
              fresh <- Vector.replicateM (length set) $ Unify.fresh Core.typex
              let checked = flip Strict.Vector.imapM set $ \index Element {element, link} -> do
                    let element' = Shift.map (Shift.Over Shift) element
                        typex = fresh Vector.! index
                        used = writeSTRef recursive True
                    element <- Definition3.checkAuto (groupTermBindings used fresh context) (shiftLogical typex) element'
                    pure $ do
                      element <- Definition3.solve position element
                      pure Element {element, link}
                  types = Types (Strict.Vector.fromLazy fresh)
              case waiting of
                Nothing -> do
                  set <- checked
                  pure $ types Unify.::: (Set <$> sequence set)
                Just waiting -> do
                  cell <- newSTRef Nothing
                  modifySTRef waiting (++ [checked >>= writeSTRef cell . Just])
                  let solved =
                        Unify.later (readSTRef cell) >>= \case
                          Just set -> Set <$> sequence set
                          Nothing -> error "a definition that was put off was never checked"
                  pure $ types Unify.::: solved
            set <- solve set
            types <- logical position types
            pure $ Solved types :::: (Implicit.Check <$> set)
        }
    where
      Identity (_ :::: Identity (Implicit.Resolve (Set set))) = group

solveDefinition ::
  Position ->
  Definition4 (Unify.Solve s) (Unify.Logical s scope) locality Identity Layout.Group Check scope ->
  Unify.Solve s (Definition4 Identity Void locality Identity Layout.Group Check scope)
solveDefinition position = \case
  (annotation ::: Identity definition) -> do
    definition <- definition
    pure $ annotation ::: Identity (Identity definition)
  Link link id -> pure (Link link id)
  Group (Identity (Solved types :::: set)) -> do
    types <- Unify.solve position types
    set <- set
    pure $ Group $ Identity (Solved types :::: pure set)
