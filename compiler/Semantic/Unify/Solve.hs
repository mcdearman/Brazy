-- |
-- Reading off what type checking worked out: types and evidence with each
-- unknown replaced by what it was found to be, and an unknown that never
-- was worked out defaulted. 'Solve' holds this work, to be run once
-- checking is done.
module Semantic.Unify.Solve where

import Control.Monad (ap, liftM)
import Control.Monad.ST (ST)
import Core.Tree.Constraint (ConstraintF (..))
import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.Evidence (EvidenceF)
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Core.Tree.Forall (ForallOver (..))
import Core.Tree.Instanciation (InstanciationF (..))
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type (TypeF (..))
import qualified Core.Type.Functor as Core
import Data.Kind (Constraint)
import qualified Data.Kind as Kind
import Data.STRef (readSTRef)
import Data.Void (Void)
import Error (Position, unsupportedFeatureConstraintedTypeDefaulting)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Unify.Evidence as Evidence (Box (..), Logical (..))
import Semantic.Unify.Type (Logical, defaultFrom)
import qualified Semantic.Unify.Type as Type (Box (..), Logical (..))
import {-# SOURCE #-} Semantic.Unify.Type1 (defaulted)

newtype Solve s a = Solve (ST s a)

-- | Something that is done when the solving gets to it.
later :: ST s a -> Solve s a
later = Solve

instance Functor (Solve s) where
  fmap = liftM

instance Applicative (Solve s) where
  pure a = Solve (pure a)
  (<*>) = ap

instance Monad (Solve s) where
  Solve m >>= f = Solve (m >>= (\(Solve a) -> a) . f)

-- |
-- What is solved is solved where it was checked: the context is that of
-- where its type variables were made, so that one that was never worked out
-- can be taken to be one of the types its module's @default@ declaration
-- lists, as an instance is looked up there.
type SolveType :: (Kind.Type -> Environment -> Kind.Type) -> Constraint
class SolveType typef where
  solveWith ::
    Context s scopex ->
    Shift0.Category scopex scope ->
    Position ->
    typef (Logical s scopex) scope ->
    Solve s (typef Void scope)

solve ::
  (SolveType typef) =>
  Context s scope ->
  Position ->
  typef (Logical s scope) scope ->
  Solve s (typef Void scope)
solve context = solveWith context Shift0.Id

instance SolveType TypeF where
  solveWith context category position = \case
    Type.Logical (Type.Box reference) ->
      Solve (readSTRef reference) >>= \case
        Type.Solved typex ->
          Shift0.map category <$> solve context position typex
        Type.Unsolved {kind, constraints}
          -- Classes are asked of it: what it is taken to be has to be an
          -- instance of each.
          | not (null constraints) -> do
              taken <- Solve $ defaulted context position (Type.Logical (Type.Box reference))
              if taken
                then
                  Solve (readSTRef reference) >>= \case
                    Type.Solved typex -> Shift0.map category <$> solve context position typex
                    _ -> error "a type that was defaulted is not worked out"
                else Solve $ unsupportedFeatureConstraintedTypeDefaulting position
          | otherwise -> Solve $ defaultFrom position (Core.mapLogical category kind)
        -- There is no type to say for it, and none is needed: nothing is
        -- done with types once they are checked. Any type will do.
        Type.Skolem {} -> pure $ Type.Constructor (Type2.Tuple 0)
    Type.Logical (Type.Shift logical) ->
      solveWith (Shift.unshift context) (category Shift0.:. Shift0.Shift) position (Type.Logical logical)
    Type.Variable name -> pure $ Type.Variable name
    Type.Constructor index -> pure $ Type.Constructor index
    Type.Call function argument -> do
      function <- solveWith context category position function
      argument <- solveWith context category position argument
      pure $ Type.Call function argument
    Type.Function argument result -> do
      argument <- solveWith context category position argument
      result <- solveWith context category position result
      pure $ Type.Function argument result
    Type.Type universe -> do
      universe <- solveWith context category position universe
      pure $ Type.Type universe
    Type.Constraint -> pure Type.Constraint
    Type.Small -> pure Type.Small
    Type.Large -> pure Type.Large
    Type.Universe -> pure Type.Universe
    Type.Levity -> pure Type.Levity

instance SolveType ConstraintF where
  solveWith context category position Constraint {classx, head, arguments} = do
    arguments <- traverse (solveWith context (Shift0.Shift Shift0.:. category) position) arguments
    pure Constraint {classx, head, arguments}

instance SolveType ConstraintsF where
  solveWith context category position = \case
    Constraints constraints -> do
      constraints <- traverse (solveWith context category position) constraints
      pure $ Constraints constraints
    Value {levity} -> do
      levity <- solveWith context category position levity
      pure Value {levity}

instance (SolveType typef) => SolveType (ForallOver typef) where
  solveWith context category position ForallOver {parameters, constraints, result} = do
    parameters <- traverse (solveWith context category position) parameters
    constraints <- solveWith context category position constraints
    result <- solveWith context (Shift0.Shift Shift0.:. category) position result
    pure ForallOver {parameters, constraints, result}

type SolveEvidence :: (Kind.Type -> Kind.Type -> Environment -> Kind.Type) -> Constraint
class SolveEvidence evidencef where
  solveEvidenceWith ::
    Context s scopex ->
    Shift0.Category scopex scope ->
    Position ->
    evidencef (Evidence.Logical s scopex) (Type.Logical s scopex) scope ->
    Solve s (evidencef Void Void scope)

solveEvidence ::
  (SolveEvidence evidencef) =>
  Context s scope ->
  Position ->
  evidencef (Evidence.Logical s scope) (Type.Logical s scope) scope ->
  Solve s (evidencef Void Void scope)
solveEvidence context = solveEvidenceWith context Shift0.Id

instance SolveEvidence EvidenceF where
  solveEvidenceWith context category position = \case
    Evidence.Variable {variable, instanciation} -> do
      instanciation <- solveEvidenceWith context category position instanciation
      pure Evidence.Variable {variable, instanciation}
    Evidence.Super {base, index} -> do
      base <- solveEvidenceWith context category position base
      pure $ Evidence.Super {base, index}
    Evidence.Held term -> pure Evidence.Held {term}
    Evidence.Applied {function, arguments} -> do
      function <- solveEvidenceWith context category position function
      arguments <- traverse (solveEvidenceWith context category position) arguments
      pure Evidence.Applied {function, arguments}
    -- It is only made of evidence that is worked out already.
    Evidence.Abstract {} -> error "solving evidence that is a function"
    Evidence.Logical (Evidence.Shift evidence) ->
      solveEvidenceWith (Shift.unshift context) (category Shift0.:. Shift0.Shift) position (Evidence.Logical evidence)
    Evidence.Logical (Evidence.Box reference) ->
      Solve (readSTRef reference) >>= \case
        Evidence.Solved evidence -> Shift0.map category <$> solveEvidence context position evidence
        -- The type the evidence is for was never worked out. If it can be
        -- taken to be one of the types the module's @default@ declaration
        -- lists, this is the evidence of that type's instance.
        Evidence.Unsolved {subject} -> do
          taken <- Solve $ maybe (pure False) (defaulted context position) subject
          if taken
            then
              Solve (readSTRef reference) >>= \case
                Evidence.Solved evidence -> Shift0.map category <$> solveEvidence context position evidence
                Evidence.Unsolved {} -> error "evidence of a type that was defaulted is not worked out"
            else Solve $ unsupportedFeatureConstraintedTypeDefaulting position

instance SolveEvidence InstanciationF where
  solveEvidenceWith context category position = \case
    Instanciation instanciation -> do
      instanciation <- traverse (solveEvidenceWith context category position) instanciation
      pure $ Instanciation instanciation
    Mono {levity} -> do
      levity <- solveWith context category position levity
      pure Mono {levity}

-- todo, figure out how to make sure nodes that are being solved early have a
-- rigid context
runSolve :: Solve s a -> ST s a
runSolve (Solve a) = a
