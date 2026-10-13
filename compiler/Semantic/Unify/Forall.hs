module Semantic.Unify.Forall where

import Control.Monad (zipWithM, zipWithM_)
import Control.Monad.ST (ST)
import Core.Tree.Constraint (ConstraintF (..))
import Core.Tree.Constraints (ConstraintsF (..))
import qualified Core.Tree.Constraints as Constraints (ConstraintsF (..))
import Core.Tree.Forall (ForallOver (..))
import Core.Tree.Instanciation (InstanciationF (..))
import Core.Tree.Type (TypeF (Logical, Variable))
import qualified Core.Tree.Type as Type (TypeF (..))
import qualified Core.Tree.TypeLambda as Simple (TypeLambdaOver (..))
import Data.Foldable (toList, traverse_)
import qualified Data.Kind as Kind
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.STRef (STRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import Semantic.Check.LocalBinding (shiftEquality)
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Evidence as Index.Evidence (assumed)
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Table.Local as Table.Local
import qualified Semantic.Index.Table.Term as Table.Term
import qualified Semantic.Index.Table.Type as Table.Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Unify.Evidence as Evidence (unify)
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Semantic.Unify.Generalizable (Collected (..), Collector (..), Generalizable (..))
import Semantic.Unify.Instanciation (Instanciation)
import Semantic.Unify.Solve (Solve (..), solve)
import Semantic.Unify.Type
  ( Box (..),
    Delay (..),
    Logical (..),
    Type (..),
    constrainWith,
    fresh,
    unshift,
  )
import Semantic.Unify.Zonk (Zonker (..), zonk)
import Syntax.Position (Position)
import Prelude hiding (Functor, map)

type Forall s scope = ForallOver TypeF (Logical s scope) scope

-- todo move this to Core
substitute ::
  Strict.Vector.Vector (Type s scope) ->
  TypeF (Logical s scope) (Scope.Local ':+ scope) ->
  Type s scope
substitute replacements (Variable index) = case index of
  Local.Local index -> replacements Strict.Vector.! index
  Local.Shift index -> Variable index
substitute replacements typex = case typex of
  Type.Logical logical -> Type.Logical logical
  Type.Constructor index -> Type.Constructor (Type2.unlocal index)
  Type.Call function argument -> Type.Call (substitute replacements function) (substitute replacements argument)
  Type.Function parameter result ->
    Type.Function (substitute replacements parameter) (substitute replacements result)
  Type.Type universe -> Type.Type (substitute replacements universe)
  Type.Constraint -> Type.Constraint
  Type.Small -> Type.Small
  Type.Large -> Type.Large
  Type.Universe -> Type.Universe
  Type.Levity -> Type.Levity

instanciate ::
  Context s scope ->
  Position ->
  Forall s scope ->
  ST s (Type s scope, Instanciation s scope)
instanciate context position ForallOver {parameters, constraints, result} = do
  fresh <- traverse fresh parameters
  instanciation <- case constraints of
    Constraints constraints -> do
      evidence <- for constraints $ \Constraint {classx, head, arguments} -> do
        head <- pure $ fresh Strict.Vector.! head
        arguments <- pure $ toList $ fmap (substitute fresh) arguments
        constrainWith context position classx head arguments
      pure $ Instanciation evidence
    None -> pure Mono
  pure $ (substitute fresh result, instanciation)

newtype Generalize typex s scopes = Generalize
  { runGeneralize ::
      forall scope.
      Context s (scope ':+ scopes) ->
      ST s (typex s (scope ':+ scopes))
  }

type Body ::
  (Kind.Type -> Environment -> Kind.Type) ->
  (Environment -> Kind.Type) ->
  Kind.Type ->
  Environment ->
  Kind.Type
data Body typef term s scope = (:::)
  { typex :: !(typef (Logical s scope) scope),
    term :: !(Solve s (term scope))
  }

infix 5 :::

--
-- A type that is still unknown and has something required of it is made a
-- variable too, with what is required of it, if the caller allows: then
-- what was waiting for the evidence is given what the type made here
-- assumes. Otherwise the unknown is left to whatever is outside to work
-- out, and what is made here does not vary in it.
generalizeBody ::
  (Generalizable typex) =>
  Position ->
  Context s scope ->
  -- | Whether a type that something is required of may become a variable.
  -- It is asked after the body is checked.
  ST s Bool ->
  Generalize (Body typex term) s scope ->
  ST s (Body (ForallOver typex) (Simple.TypeLambdaOver term) s scope)
generalizeBody position context requiring (Generalize run) = do
  typex ::: term <- run $ case context of
    Context {termEnvironment, localEnvironment, typeEnvironment, equalities} ->
      Context
        { termEnvironment = Table.Term.Local termEnvironment,
          localEnvironment = Table.Local.Local Vector.empty localEnvironment,
          typeEnvironment = Table.Type.Local typeEnvironment,
          equalities = shiftEquality <$> equalities
        }
  requiring <- requiring
  direct <- collect (Collector Mask.Runtime) typex
  -- A type that is only in what is required of another is a variable too.
  indirect <- if requiring then concat <$> traverse mentioned direct else pure []
  let candidates = direct ++ indirect
  traverse_ (shiftUnwanted requiring) candidates
  boxes <- nub . catMaybes <$> traverse (selectBox requiring) candidates
  parameters <- Strict.Vector.fromList <$> traverse parameter boxes
  required <- concat <$> zipWithM requirements [0 ..] boxes
  zipWithM_ writeVariable [0 ..] boxes
  result <- zonk Zonker Shift0.Id typex
  assumed <- for (zip [0 ..] required) $ \(index, (classx, head, Delay {arguments, evidence})) -> do
    Evidence.unify evidence $
      Evidence.Variable
        { variable = Index.Evidence.assumed index,
          instanciation = Mono
        }
    arguments <- traverse (zonk Zonker Shift0.Id) arguments
    pure Constraint {classx = Type2.unlocal classx, head, arguments = Strict.Vector.fromList arguments}
  let constraints
        | null assumed = Constraints.None
        | otherwise = Constraints.Constraints (Strict.Vector.fromList assumed)
  pure $
    ForallOver
      { parameters,
        constraints,
        result
      }
      ::: do
        parameters <- traverse (solve position) parameters
        constraints <- solve position constraints
        result <- term
        pure
          Simple.TypeLambdaOver
            { parameters,
              constraints,
              result
            }
  where
    -- The types in what is required of a type.
    mentioned :: Collected s (scope ':+ scopes) -> ST s [Collected s (scope ':+ scopes)]
    mentioned = \case
      Collect reference ->
        readSTRef reference >>= \case
          Unsolved {constraints} ->
            concat <$> traverse (collect (Collector Mask.Runtime)) [argument | delays <- Map.elems constraints, Delay {arguments} <- delays, argument <- arguments]
          Solved {} -> pure []
          Skolem {} -> pure []
      Reach {} -> pure []
    -- What is required of a type that is to be a variable: each class, with
    -- which variable it is and what was waiting for the evidence.
    requirements ::
      Int ->
      STRef s (Box s (Scope.Local ':+ scopes)) ->
      ST s [(Type2.Index (Scope.Local ':+ scopes), Int, Delay s (Scope.Local ':+ scopes))]
    requirements head reference =
      readSTRef reference >>= \case
        Unsolved {constraints} ->
          pure [(classx, head, delay) | (classx, delays) <- Map.toList constraints, delay <- delays]
        Solved {} -> pure []
        Skolem {} -> pure []
    shiftUnwanted :: Bool -> Collected s (scope ':+ scopes) -> ST s ()
    shiftUnwanted requiring = \case
      Collect reference ->
        readSTRef reference >>= \case
          Unsolved {kind, constraints}
            | null constraints || requiring -> do
                _ <- unshift fail fail kind
                pure ()
            | otherwise -> do
                _ <- unshift fail fail $ Logical (Box reference)
                pure ()
            where
              fail :: a
              fail = error "unsink can't fail"
          Solved {} -> pure ()
          Skolem {} -> pure ()
      Reach {} -> pure ()
    selectBox :: Bool -> Collected s (scope ':+ scopes) -> ST s (Maybe (STRef s (Box s (scope ':+ scopes))))
    selectBox requiring = \case
      Collect reference ->
        readSTRef reference >>= \case
          Unsolved {constraints}
            | null constraints || requiring -> pure $ Just reference
            | otherwise -> error "select box with unsolved"
          Solved {} -> pure Nothing
          Skolem {} -> pure Nothing
      Reach {} -> pure Nothing
    parameter :: STRef s (Box s (scope' ':+ scope)) -> ST s (Type s scope)
    parameter reference =
      readSTRef reference >>= \case
        Unsolved {kind} -> unshift fail fail kind
          where
            fail :: a
            fail = error "parameter can't fail"
        Solved {} -> error "bad kind"
        Skolem {} -> error "bad kind"
    writeVariable :: Int -> STRef s (Box s (Scope.Local ':+ scopes)) -> ST s ()
    writeVariable variable reference =
      writeSTRef reference $ Solved $ Variable $ Local.Local variable

type MapForall ::
  (Kind.Type -> Environment -> Kind.Type) ->
  (Kind.Type -> Environment -> Kind.Type) ->
  Kind.Type
newtype MapForall typef typef'
  = MapForall (forall s scope scope'. typef (Logical s scope') scope -> typef' (Logical s scope') scope)

mapForall ::
  MapForall typef typef' ->
  ForallOver typef (Logical s scope) scope ->
  ForallOver typef' (Logical s scope) scope
mapForall (MapForall map) ForallOver {parameters, constraints, result} =
  ForallOver
    { parameters,
      constraints,
      result = map result
    }
