module Semantic.Check.Temporary.TypeDefinition where

import Control.Monad.ST (ST)
import Core.Tree.Type ((-#>))
import qualified Core.Tree.Type as Core
import qualified Data.Vector.Strict as Strict
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeatureGADTs)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Constraint (Constraint)
import qualified Semantic.Check.Temporary.Constraint as Constraint
import Semantic.Check.Temporary.Constructor (Constructor (..), Shape (..))
import qualified Semantic.Check.Temporary.Constructor as Constructor
import Semantic.Check.Temporary.Entry (Entry (..))
import Semantic.Check.Temporary.Method (Method)
import qualified Semantic.Check.Temporary.Method as Method
import qualified Semantic.Check.Temporary.Scheme as Scheme
import Semantic.Check.Temporary.StrictnessAnnotation (StrictnessAnnotation (..))
import qualified Semantic.Check.Temporary.Type as Type
import Semantic.Check.Temporary.TypePattern (TypePattern (..))
import qualified Semantic.Check.Temporary.TypePattern as TypePattern
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Constructor (Syntax (Standard))
import qualified Semantic.Tree.Constraints as Semantic (Constraints (..))
import qualified Semantic.Tree.GADTConstructor as Semantic (GADTConstructor (..))
import qualified Semantic.Tree.Scheme as Semantic (Scheme (..))
import Semantic.Tree.Selector (Selector)
import qualified Semantic.Tree.Type as Semantic (Type (..))
import qualified Semantic.Tree.TypeDefinition as Semantic (TypeDefinition (..))
import qualified Semantic.Tree.TypeDefinition as Solved
import qualified Semantic.Tree.TypePattern as Semantic (TypePattern (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

data TypeDefinition s scope
  = ADT
      { brand :: !Brand,
        position :: !Position,
        parameters :: !(Strict.Vector (TypePattern s scope)),
        constructors :: !(Strict.Vector (Constructor s (Scope.Local ':+ scope))),
        selectors :: !(Strict.Vector Selector)
      }
  | -- | A type declared with @where@: each constructor is given with its
    -- whole type, and has type variables of its own.
    GADT
      { brand :: !Brand,
        position :: !Position,
        parameters :: !(Strict.Vector (TypePattern s scope)),
        owned :: !(Strict.Vector (Owned s scope))
      }
  | Class
      { position :: !Position,
        parameter :: !(TypePattern s scope),
        constraints :: !(Strict.Vector (Constraint s scope)),
        methods :: !(Strict.Vector (Method s (Scope.Local ':+ scope)))
      }

-- |
-- A constructor with the type variables that are its own, which are what
-- is in scope for it.
data Owned s scope = Owned
  { variables :: !(Strict.Vector (TypePattern s scope)),
    constructor :: !(Constructor s (Scope.Local ':+ scope))
  }

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.TypeDefinition Resolve scope ->
  ST s (TypeDefinition s scope)
check context kind = \case
  Semantic.ADT
    { position,
      brand,
      constructors,
      selectors,
      parameters
    } ->
      do
        parameters <- traverse fresh parameters
        let kind' = foldr ((-#>) . TypePattern.typex) Core.typex parameters
        Unify.unify context position kind' kind
        context <- pure $ Scheme.augment parameters context
        constructors <- traverse (Constructor.check context) constructors
        pure
          ADT
            { brand,
              position,
              constructors,
              parameters,
              selectors
            }
  Semantic.Class
    { position,
      methods,
      constraints,
      parameter
    } -> do
      parameter <- fresh parameter
      let kind' = TypePattern.typex parameter -#> Core.constraint
      Unify.unify context position kind' kind
      context <- pure $ Scheme.augment (Strict.Vector.singleton parameter) context
      constraints <- traverse (Constraint.check context) constraints
      methods <- traverse (Method.check context) methods
      pure
        Class
          { position,
            parameter,
            constraints,
            methods
          }
  Semantic.GADT {position, brand, parameters, gadtConstructors} -> do
    parameters <- traverse fresh parameters
    let kind' = foldr ((-#>) . TypePattern.typex) Core.typex parameters
    Unify.unify context position kind' kind
    owned <- traverse (own context) gadtConstructors
    pure GADT {brand, position, parameters, owned}
  where
    fresh Semantic.TypePattern {name, position} = do
      level <- Unify.fresh Core.universe
      typex <- Unify.fresh (Core.typeWith level)
      pure
        TypePattern
          { name,
            typex,
            position
          }

-- |
-- A constructor that is given with its whole type:
--
-- > C :: a -> !b -> T a (Maybe b)
--
-- The variables of the type are its own. What comes before the arrows is
-- what it holds, strictly where that is marked, and what the type ends in
-- is the type it makes a value of, which has to be the type being declared
-- applied to arguments. Those arguments are what is new: they are of the
-- constructor's choosing, where a constructor declared the other way always
-- makes the type applied to its parameters.
own :: Context s scope -> Semantic.GADTConstructor Resolve scope -> ST s (Owned s scope)
own context Semantic.GADTConstructor {position, name, typex} = do
  let Semantic.Scheme {parameters, constraints, result = signature} = typex
  -- A constructor that requires something of its types is not done yet.
  case constraints of
    Semantic.None -> pure ()
    Semantic.Constraints _ -> unsupportedFeatureGADTs position
  variables <- for parameters $ \Semantic.TypePattern {name, position} -> do
    level <- Unify.fresh Core.universe
    typex <- Unify.fresh (Core.typeWith level)
    pure TypePattern {name, typex, position}
  let inner = Scheme.augment variables context
      (held, made) = taken signature
  entries <- for held $ \(strict, startPosition, typex) -> do
    entry <- Type.check inner Core.typex typex
    pure Entry {startPosition, entry, strict}
  made <- Type.check inner Core.typex made
  pure
    Owned
      { variables,
        constructor =
          Constructor
            { position,
              name,
              syntax = Standard,
              entries = Strict.Vector.fromList entries,
              shape =
                Indexed
                  { variables = length variables,
                    arguments = Strict.Vector.fromList (applied made [])
                  }
            }
      }
  where
    -- What a constructor's type says it holds, and what is left.
    taken = \case
      Semantic.Function {startPosition, parameter, result} ->
        let (held, made) = taken result
         in ((Lazy, startPosition, parameter) : held, made)
      Semantic.StrictFunction {startPosition, parameter, result} ->
        let (held, made) = taken result
         in ((Strict, startPosition, parameter) : held, made)
      made -> ([], made)
    -- What a type is applied to.
    applied typex arguments = case typex of
      Type.Call {function, argument} -> applied function (argument : arguments)
      _ -> arguments

solve :: Context s scope -> TypeDefinition s scope -> Unify.Solve s (Solved.TypeDefinition Check scope)
solve context = \case
  -- Once it is checked, a type declared with @where@ is a type like any
  -- other, some of whose constructors say what type they make.
  GADT {brand, position, parameters, owned} -> do
    parameters <- traverse TypePattern.solve parameters
    constructors <- for owned $ \Owned {variables, constructor} -> do
      variables <- traverse TypePattern.solve variables
      Constructor.solve (Scheme.augmentSolve variables context) constructor
    pure Solved.ADT {position, brand, parameters, constructors, selectors = Strict.Vector.empty}
  ADT {brand, position, parameters, constructors, selectors} -> do
    parameters <- traverse TypePattern.solve parameters
    context <- pure $ Scheme.augmentSolve parameters context
    constructors <- traverse (Constructor.solve context) constructors
    pure Solved.ADT {position, brand, parameters, constructors, selectors}
  Class {parameter, position, constraints, methods} -> do
    parameter <- TypePattern.solve parameter
    context <- pure $ Scheme.augmentSolve (Strict.Vector.singleton parameter) context
    constraints <- traverse (Constraint.solve context) constraints
    methods <- traverse (Method.solve context) methods
    pure Solved.Class {position, parameter, constraints, methods}
