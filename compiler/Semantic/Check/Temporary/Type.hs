{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Check.Temporary.Type where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import Core.Tree.Type ((-#>))
import qualified Core.Tree.Type as Core
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Core.Type.Functor (shiftLogical)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector (toLazy)
import Error
  ( cyclicalTypeChecking,
    partialSynonym,
    uncheckable,
    universeMustBeSmall,
    unsupportedFeatureStrictFunctions,
  )
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Context as Context
import qualified Semantic.Check.LocalBinding as LocalBinding (Equality (Expanding), LocalBinding (..), shiftEquality)
import qualified Semantic.Check.Temporary.TypePattern as Unsolved (TypePattern (..))
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Table.Local as Local.Table
import qualified Semantic.Index.Table.Term as Term.Table (Table (..))
import qualified Semantic.Index.Table.Type as Type.Table (Table (..))
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Local as Label
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope (Local)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Synonym as Synonym (Synonym (..), SynonymBody (..))
import qualified Semantic.Tree.Type as Semantic (Type (..))
import qualified Semantic.Tree.Type as Solved
import qualified Semantic.Tree.TypePattern as Semantic.TypePattern (TypePattern (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Type s scope
  = Variable
      { startPosition :: !Position,
        variable :: !(Local.Index scope)
      }
  | Constructor
      { startPosition, constructorPosition :: !Position,
        constructor :: !(Type2.Index scope),
        -- | What the constructor is a name for, if it is one. It is found
        -- when the type is solved: inside types that are checked together
        -- it is not known before.
        synonym :: Unify.Solve s (Solved.Synonym Check scope)
      }
  | Tuple
      { startPosition :: !Position,
        elements :: !(Strict.Vector2 (Type s scope))
      }
  | Call
      { startPosition :: !Position,
        function :: !(Type s scope),
        argument :: !(Type s scope)
      }
  | Function
      { startPosition, operatorPosition :: !Position,
        parameter :: !(Type s scope),
        result :: !(Type s scope)
      }
  | List
      { startPosition :: !Position,
        element :: !(Type s scope)
      }
  | LiftedList
      { startPosition :: !Position,
        items :: !(Strict.Vector1 (Type s scope))
      }
  | SmallType {startPosition :: !Position}
  | Constraint {startPosition :: !Position}
  | Levity {startPosition :: !Position}

checkCount ::
  Context s scope ->
  Int ->
  Unify.Type s scope ->
  Semantic.Type Position Resolve scope ->
  ST s (Type s scope)
checkCount context count kind = \case
  Semantic.Call {startPosition, function, argument} -> do
    level <- Unify.fresh Core.universe
    parameterType <- Unify.fresh (Core.typeWith level)
    function <- checkCount context (count + 1) (parameterType -#> kind) function
    argument <- check context parameterType argument
    pure Call {startPosition, function, argument}
  -- One of the types that are being checked together, called the way it
  -- is from outside them: what a name for a type stands for calls it so.
  Semantic.Constructor {startPosition, constructorPosition, constructor}
    | Just kind' <- Context.grouped context constructor -> do
        Unify.unify context constructorPosition kind kind'
        pure Constructor {startPosition, constructorPosition, constructor, synonym = pure Solved.NoSynonym}
  -- A name for a type, met inside types that are being checked together.
  -- What the name was found to be cannot be asked: finding that may wait
  -- for these very types, as when
  --
  -- > type Type s = TypeF (Logical s)
  -- > data Logical s = Box (Box s)
  -- > data Box s = Solved (Type s)
  --
  -- So what the name stands for is checked here, as if it were written
  -- out, and the name is kept with what that came to.
  Semantic.Constructor {startPosition, constructorPosition, constructor}
    | Just (_ Synonym.::: Synonym.SynonymBody {parameters, synonym = body}) <-
        Context.lookupSource context constructor -> do
        if length parameters > count then partialSynonym startPosition else pure ()
        if Context.expanding context constructor then cyclicalTypeChecking constructorPosition else pure ()
        universe <- Unify.fresh Core.universe
        wobbly <- for parameters $ \Semantic.TypePattern.TypePattern {name, position} -> do
          level <- Unify.fresh Core.universe
          typex <- Unify.fresh (Core.typeWith level)
          pure Unsolved.TypePattern {name, typex, position}
        target <- Unify.fresh (Core.typeWith universe)
        Unify.unify context constructorPosition kind (foldr ((-#>) . Unsolved.typex) target wobbly)
        let inner = inside constructor wobbly context
        body <- check inner (shiftLogical target) body
        let synonym = do
              body <- solve inner body
              pure $ Solved.Synonym (length parameters) (Core.simplify body)
        pure Constructor {startPosition, constructorPosition, constructor, synonym}
  Semantic.Constructor {startPosition, constructorPosition, constructor} -> do
    kind' <- Context.lookupKind constructorPosition context constructor
    Unify.unify context constructorPosition kind kind'
    synonym <- Context.lookupSynonym context constructor
    if
      | Solved.Synonym expected _ <- synonym,
        expected > count ->
          partialSynonym startPosition
      | otherwise -> pure ()

    pure Constructor {startPosition, constructorPosition, constructor, synonym = pure synonym}
  typex -> check context kind typex

-- |
-- What a name for a type stands for is checked where its parameters are
-- types that are not known yet, and where meeting the name again is known
-- for the circle it is.
inside ::
  Type2.Index scope ->
  Strict.Vector (Unsolved.TypePattern s scope) ->
  Context s scope ->
  Context s (Scope.Local ':+ scope)
inside constructor parameters Context {termEnvironment, localEnvironment, typeEnvironment, equalities} =
  Context
    { termEnvironment = Term.Table.Local termEnvironment,
      localEnvironment = Local.Table.Local (fmap wobbly (Strict.Vector.toLazy parameters)) localEnvironment,
      typeEnvironment = Type.Table.Local typeEnvironment,
      equalities = LocalBinding.Expanding (Shift0.map Shift0.Shift constructor) : (LocalBinding.shiftEquality <$> equalities)
    }
  where
    wobbly Unsolved.TypePattern {name, typex} =
      LocalBinding.Wobbly
        { label = Label.LocalBinding {name},
          wobbly = shiftLogical typex
        }

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Type Position Resolve scope ->
  ST s (Type s scope)
check context@Context {localEnvironment} kind = \case
  Semantic.Variable {startPosition, variable} -> case localEnvironment Local.Table.! variable of
    LocalBinding.Rigid {rigid}
      | rigid <- logicalType rigid -> do
          Unify.unify context startPosition kind rigid
          pure $ Variable {startPosition, variable}
    LocalBinding.Wobbly {wobbly} -> do
      Unify.unify context startPosition kind wobbly
      pure Variable {startPosition, variable}
  typex@Semantic.Constructor {} ->
    checkCount context 0 kind typex
  Semantic.Tuple {startPosition, elements} -> do
    elements <- traverse (check context Core.typex) elements
    Unify.unify context startPosition kind Core.typex
    pure Tuple {startPosition, elements}
  typex@Semantic.Call {} ->
    checkCount context 0 kind typex
  Semantic.Function {startPosition, parameter, operatorPosition, result} -> do
    level <- Unify.fresh Core.universe
    level' <- Unify.fresh Core.universe
    parameter <- check context (Core.typeWith level) parameter
    result <- check context (Core.typeWith level') result
    Unify.unify context operatorPosition (Core.typeWith level') kind
    pure Function {startPosition, operatorPosition, parameter, result}
  Semantic.List {startPosition, element} -> do
    element <- check context Core.typex element
    Unify.unify context startPosition kind Core.typex
    pure List {startPosition, element}
  Semantic.SmallType {startPosition} -> do
    Unify.unify context startPosition kind Core.kind
    pure SmallType {startPosition}
  Semantic.StrictFunction {operatorPosition} ->
    unsupportedFeatureStrictFunctions operatorPosition
  Semantic.LiftedList {startPosition, items} -> do
    inner <- Unify.fresh Core.typex
    Unify.unify context startPosition kind (Core.list inner)
    items <- traverse (check context inner) items
    pure LiftedList {startPosition, items}
  Semantic.Type {startPosition, universe} -> case universe of
    Semantic.Small {} -> do
      Unify.unify context startPosition kind Core.kind
      pure SmallType {startPosition}
    _ -> universeMustBeSmall startPosition
  Semantic.Constraint {startPosition} -> do
    Unify.unify context startPosition kind Core.kind
    pure Constraint {startPosition}
  Semantic.Small {startPosition} -> uncheckable startPosition
  Semantic.Large {startPosition} -> uncheckable startPosition
  Semantic.Universe {startPosition} -> uncheckable startPosition
  Semantic.Levity {startPosition} -> do
    Unify.unify context startPosition kind Core.kind
    pure Levity {startPosition}

solve :: Context s scope -> Type s scope -> Unify.Solve s (Solved.Type Position Check scope)
solve context = \case
  Variable {startPosition, variable} -> do
    pure
      Solved.Variable
        { startPosition,
          variable
        }
  Constructor {startPosition, constructorPosition, constructor, synonym} -> do
    synonym <- synonym
    pure
      Solved.Constructor
        { startPosition,
          constructorPosition,
          constructor,
          synonym
        }
  Tuple {startPosition, elements} -> do
    elements <- traverse (solve context) elements
    pure $ Solved.Tuple {startPosition, elements}
  Call {startPosition, function, argument} -> do
    function <- solve context function
    argument <- solve context argument
    pure $ Solved.Call {startPosition, function, argument}
  Function {startPosition, operatorPosition, parameter, result} -> do
    parameter <- solve context parameter
    result <- solve context result
    pure Solved.Function {startPosition, operatorPosition, parameter, result}
  List {startPosition, element} -> do
    element <- solve context element
    pure Solved.List {startPosition, element}
  LiftedList {startPosition, items} -> do
    items <- traverse (solve context) items
    pure $ Solved.LiftedList {startPosition, items}
  SmallType {startPosition} -> pure Solved.SmallType {startPosition}
  Constraint {startPosition} -> pure Solved.Constraint {startPosition}
  Levity {startPosition} -> pure Solved.Levity {startPosition}
