-- | This module exists purely to work around `.hs-boot` module limitations.
-- Here, `constrain` returns evidence so it would make the `.hs-boot` modules
-- of `Semantic.Unify.Type` and `Semantic.Unify.Evidence` mutually recursive,
-- which GHC doesn't allow.
module Semantic.Unify.Type1 where

import Control.Monad.ST (ST)
import Core.Evidence.Functor (shiftLogical)
import Core.Substitute (logicalEvidence, logicalType, substituteType)
import qualified Core.Tree.Constraint as Simple (argument)
import qualified Core.Tree.Constraint as Simple.Constraint
import Core.Tree.Constraints (ConstraintsF (..))
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Core.Tree.Instanciation (InstanciationF (Instanciation))
import qualified Core.Tree.Instanciation as Instanciation (InstanciationF (..))
import Core.Tree.Type (TypeF (..), (#))
import Data.Foldable (toList, traverse_)
import qualified Data.Map as Map
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Context as Context
import qualified Semantic.Check.Binding.Local as Local (Constraint (..), Equality (..), LocalBinding (..))
import Semantic.Check.Binding.Type (TypeBinding (TypeBinding))
import qualified Semantic.Check.Binding.Type as TypeBinding
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import qualified Semantic.Index.Table.Local as Local.Table
import qualified Semantic.Index.Table.Type as Type.Table
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Unify.Builtin as Builtin (constrain)
import {-# SOURCE #-} Semantic.Unify.Error (Error (..), abort)
import Semantic.Unify.Evidence (Evidence)
import qualified Semantic.Unify.Evidence as Evidence (Box (..), Logical (..))
import Semantic.Unify.Type
  ( Box (..),
    Delay (..),
    Logical (..),
    Type,
    alikeAll,
    compared,
    defaults,
    equated,
    equatedHidden,
    firstM,
    fresh,
    hidden,
    sameAll,
    typeCheck,
    unify,
    unshift,
  )
import Control.Monad (filterM)
import qualified Core.Tree.Constraints as Instance (Prerequisites (..))
import qualified Core.Tree.Type as Simple (Type)
import Data.Foldable (for_)
import qualified Semantic.Unify.Evidence as Evidence (Fallback (..), defaultable)
import Syntax.Position (Position)
import Prelude hiding (Functor, head, map)

constrain ::
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  ST s (Evidence s scope)
constrain context position classx term = constrainWith context position classx term []

constrainWith ::
  forall s scope.
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  [Type s scope] ->
  ST s (Evidence s scope)
constrainWith context_ position classx_ term_ arguments_ = constrainWith context_ classx_ term_ arguments_
  where
    constrainWith ::
      forall scope.
      Context s scope ->
      Type2.Index scope ->
      Type s scope ->
      [Type s scope] ->
      ST s (Evidence s scope)
    constrainWith context@Context {equalities, localEnvironment} classx term arguments = do
      -- What a match that this is inside says holds, holds. So does what
      -- is known of a type variable that a match says is this type: what
      -- was required of the variable is had of the type.
      let whole = foldl (#) term arguments
          known = [(subject, evidence) | Local.Given classx' subject evidence <- equalities, classx' == classx]
          told =
            [ (foldl (#) typex (logicalType <$> toList arguments'), logicalEvidence evidence)
            | Local.Equality index typex <- equalities,
              Local.Rigid {constraints} <- [localEnvironment Local.Table.! index],
              Just candidates <- [Map.lookup classx constraints],
              Local.Constraint {arguments = arguments', evidence} <- candidates
            ]
      given <- firstM (\(subject, _) -> compared False subject whole) (known ++ told)
      case given of
        Just (_, evidence) -> pure evidence
        Nothing ->
          hidden term >>= \case
            -- Of a type that stands for itself alone, only what is known
            -- of it where this is can be had.
            Just logical
              | Just known <- equatedHidden context logical -> constrainWith context classx known arguments
              | otherwise -> abort position (Constrain context_ classx_ term_ arguments_)
            Nothing -> plainly context classx term arguments
    plainly ::
      forall scope.
      Context s scope ->
      Type2.Index scope ->
      Type s scope ->
      [Type s scope] ->
      ST s (Evidence s scope)
    plainly context classx term@(Logical (Box reference)) arguments =
      readSTRef reference >>= \case
        Solved term -> constrainWith context classx term arguments
        Skolem {} -> error "plain constraint of a type that stands for itself"
        Unsolved {kind, constraints, erasure} -> do
          case Map.lookup classx constraints of
            Nothing -> do
              universe <- fresh Universe
              target <- fresh (Type universe)
              real <- Context.lookupKind position context classx
              unify context position (Function target Constraint) real

              typeCheck context position target (foldl (#) term arguments)
              let fallback = do
                    classx <- Evidence.defaultable classx
                    pure Evidence.Fallback {classx, permitted = defaults (defaulting context) reference}
              logical <- newSTRef Evidence.Unsolved {fallback}
              let delay = Delay {arguments, evidence = Evidence.Logical (Evidence.Box logical)}
              writeSTRef reference $! Unsolved {kind, constraints = Map.insert classx [delay] constraints, erasure}
              pure $ Evidence.Logical (Evidence.Box logical)
            -- The class is asked of the variable already. If it is asked
            -- of it given these same arguments, this is the same question.
            -- Otherwise it is one more.
            Just delays -> do
              found <- firstM (\Delay {arguments = arguments'} -> sameAll arguments arguments') delays
              case found of
                Just Delay {evidence} -> pure evidence
                Nothing -> do
                  logical <- newSTRef Evidence.Unsolved {fallback = Nothing}
                  let delay = Delay {arguments, evidence = Evidence.Logical (Evidence.Box logical)}
                  writeSTRef reference $!
                    Unsolved {kind, constraints = Map.insert classx (delays ++ [delay]) constraints, erasure}
                  pure $ Evidence.Logical (Evidence.Box logical)
    plainly context classx (Logical (Shift logical)) arguments = do
      arguments <- traverse (unshift context position) arguments
      let quit = abort position $ Unshift context (Constructor classx)
      classx <- Shift.partialUnshift quit classx
      shiftLogical <$> constrainWith (Shift.unshift context) classx (Logical logical) arguments
    plainly context@Context {localEnvironment} classx (Variable index) arguments
      | Local.Rigid {constraints} <- localEnvironment Local.Table.! index,
        Just known <- Map.lookup classx constraints,
        candidates@(_ : _) <-
          [ ([logicalType argument | argument <- toList arguments'], evidence)
          | Local.Constraint {arguments = arguments', evidence} <- known,
            length arguments' == length arguments,
            not (everywhere (toList arguments'))
          ] = do
          -- What is known of the variable may be known for several lists of
          -- arguments. The one that is meant is the one these arguments can
          -- be: if only one can, they are made to be it.
          let use (arguments', evidence) = do
                traverse_ (uncurry $ unify context position) (zip arguments arguments')
                pure (logicalEvidence evidence)
              quit = abort position (Constrain context_ classx_ term_ arguments_)
          case candidates of
            [only] -> use only
            _ -> do
              exact <- filterM (sameAll arguments . fst) candidates
              possible <- filterM (alikeAll arguments . fst) candidates
              case (exact, possible) of
                (first : _, _) -> use first
                ([], [only]) -> use only
                _ -> quit
    -- What is known of the variable may be known of it given every type
    -- that has the class: then it is known of it given this type, if this
    -- type has the class, and the evidence is made of the evidence for
    -- that.
    plainly context@Context {localEnvironment} classx (Variable index) arguments@(_ : _)
      | Local.Rigid {constraints} <- localEnvironment Local.Table.! index,
        Just known <- Map.lookup classx constraints,
        (before, evidence) : _ <-
          [ ([logicalType argument | argument <- init (toList arguments')], evidence)
          | Local.Constraint {arguments = arguments', evidence} <- known,
            length arguments' == length arguments,
            everywhere (toList arguments')
          ] = do
          traverse_ (uncurry $ unify context position) (zip (init arguments) before)
          given <- constrainWith context classx (last arguments) []
          pure Evidence.Applied {function = logicalEvidence evidence, arguments = [given]}
    -- A variable that is known here to be some type is asked as that type.
    plainly context classx (Variable index) arguments
      | Just known <- equated context index = constrainWith context classx known arguments
    plainly context@Context {typeEnvironment} (Type2.Index classx) (Constructor index) arguments
      | TypeBinding {classInstances} <- typeEnvironment Type.Table.! classx,
        Just instancex <- Map.lookup index classInstances = do
          Instance.Prerequisites {required = dependencies, equated = fixed} <- TypeBinding.prerequisites instancex
          -- What the instance requires its type to be given, it is given.
          for_ fixed $ \(index, typex) -> case drop index arguments of
            argument : _ -> unify context position argument (substituteType (Vector.fromList arguments) typex)
            [] -> pure ()
          case dependencies of
            Constraints dependencies -> do
              arguments <- for dependencies $ \constraint@Simple.Constraint.Constraint {classx} ->
                let argument = substituteType (Vector.fromList arguments) (Simple.argument constraint)
                 in constrain context position classx argument
              pure $ Evidence.Variable variable (Instanciation arguments)
            None {levity} ->
              pure $ Evidence.Variable variable Instanciation.Mono {levity = logicalType levity}
      where
        variable = Evidence.Direct (Type2.Index classx) index
    plainly context@Context {typeEnvironment} classx (Constructor (Type2.Index index)) arguments
      | TypeBinding {dataInstances} <- typeEnvironment Type.Table.! index,
        Just instancex <- Map.lookup classx dataInstances = do
          Instance.Prerequisites {required = dependencies, equated = fixed} <- TypeBinding.prerequisites instancex
          -- What the instance requires its type to be given, it is given.
          for_ fixed $ \(index, typex) -> case drop index arguments of
            argument : _ -> unify context position argument (substituteType (Vector.fromList arguments) typex)
            [] -> pure ()
          case dependencies of
            Constraints dependencies -> do
              arguments <- for dependencies $ \constraint@Simple.Constraint.Constraint {classx} ->
                let argument = substituteType (Vector.fromList arguments) (Simple.argument constraint)
                 in constrain context position classx argument
              pure $ Evidence.Variable variable (Instanciation arguments)
            None {levity} ->
              pure $ Evidence.Variable variable Instanciation.Mono {levity = logicalType levity}
      where
        variable = Evidence.Direct classx (Type2.Index index)
    plainly context classx (Call function argument) arguments =
      constrainWith context classx function (argument : arguments)
    plainly context classx (Function argument result) arguments = do
      typeCheck context position (Type Small) argument
      typeCheck context position (Type Small) result
      constrainWith context classx (Constructor Type2.Arrow `Call` argument `Call` result) arguments
    plainly context classx typex arguments = case typex of
      Constructor typex -> Builtin.constrain quit constrain classx typex arguments
      _ -> quit
      where
        constrain classx typex = constrainWith context classx typex []
        quit = abort position (Constrain context_ classx_ term_ arguments_)

-- |
-- Whether what a class is required of is something given every type: the
-- last thing it is given is the type that stands for them all.
everywhere :: [Simple.Type scope] -> Bool
everywhere = \case
  [] -> False
  arguments -> case last arguments of
    Constructor Type2.Every -> True
    _ -> False

