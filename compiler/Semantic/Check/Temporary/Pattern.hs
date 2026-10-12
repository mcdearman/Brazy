module Semantic.Check.Temporary.Pattern where

import Control.Monad (foldM, when)
import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import qualified Core.Tree.Forall as Core (mono)
import qualified Core.Tree.Type as Core (char, list, typex)
import Core.Tree.TypeDeclaration (assumeData)
import Core.Type.Functor (shiftLogical)
import Data.Foldable (for_, toList, traverse_)
import Data.Map (Map)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( mismatchedConstructorArguments,
    refiningBinding,
  )
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context (..))
import Semantic.Check.DataInstance (DataInstance (DataInstance))
import qualified Semantic.Check.DataInstance as DataInstance
import qualified Semantic.Check.Simple.Data as Simple.Data
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import Semantic.Check.Temporary.PatternField (Field)
import qualified Semantic.Check.Temporary.PatternField as Field
import Semantic.Check.TermBinding (TermBinding (TermBinding), Type (..))
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Table.Local as Local
import qualified Semantic.Index.Table.Term as Term
import qualified Semantic.Index.Table.Type as Type
import Semantic.Index.Term (Bound)
import qualified Semantic.Index.Term as Bound (Bound (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Pattern as Semantic
import qualified Semantic.Tree.Pattern as Solved
import Semantic.Check.LocalBinding (Equality, shiftEquality)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.StringLiteral (StringLiteral)
import Syntax.Variable (Variable)
import Prelude hiding (Bool (False, True))
import qualified Prelude

data Pattern s scope
  = Wildcard
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position)
      }
  | Constructor
      { typex :: !(Unify.Type s scope),
        -- | What matching on the constructor tells of the type variables
        -- of the type that is matched on.
        equalities :: ![Equality s scope],
        -- | The types the constructor hides.
        hidden :: ![Unify.Type s scope],
        constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern s scope)),
        single :: !Prelude.Bool,
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Record
      { typex :: !(Unify.Type s scope),
        -- | What matching on the constructor tells of the type variables
        -- of the type that is matched on.
        equalities :: ![Equality s scope],
        -- | The types the constructor hides.
        hidden :: ![Unify.Type s scope],
        constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field s scope)),
        single :: !Prelude.Bool,
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Integer
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        startPosition :: !Position,
        integer :: !Integer,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Float
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        startPosition :: !Position,
        float :: !Rational,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Character
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        startPosition :: !Position,
        irrefutable :: !Prelude.Bool,
        character :: !Char
      }
  | String
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        startPosition :: !Position,
        irrefutable :: !Prelude.Bool,
        string :: !StringLiteral
      }
  | List
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        startPosition :: !Position,
        irrefutable :: !Prelude.Bool,
        items :: !(Strict.Vector1 (Pattern s scope))
      }

(!) :: Pattern s scope -> Bound -> Unify.Type s scope
patternx ! Bound.At = typex patternx
patternx ! Bound.Select index bound = case patternx of
  Constructor {patterns} -> patterns Strict.Vector.! index ! bound
  Record {fields} -> fields Strict.Vector.! index Field.! bound
  List {items} -> items Strict.Vector.! index ! bound
  Integer {} -> error "bad index"
  Float {} -> error "bad index"
  Character {} -> error "bad index"
  String {} -> error "bad index"
  Wildcard {} -> error "bad index"

augment :: Pattern s scope -> Context s scope -> Context s (Scope.Pattern ':+ scope)
augment patternx Context {termEnvironment, localEnvironment, typeEnvironment, equalities} =
  Context
    { termEnvironment = Term.Pattern (augmentPattern patternx) termEnvironment,
      localEnvironment = Local.Pattern localEnvironment,
      typeEnvironment = Type.Pattern typeEnvironment,
      equalities = shiftEquality <$> (learnt patternx ++ equalities)
    }

-- |
-- Everything that matching a pattern tells of type variables, from the
-- pattern and from the patterns inside it.
learnt :: Pattern s scope -> [Equality s scope]
learnt = \case
  Wildcard {} -> []
  Constructor {equalities, patterns} -> foldMap learnt patterns ++ equalities
  Record {equalities, fields} -> foldMap Field.learnt fields ++ equalities
  List {items} -> foldMap learnt items
  Integer {} -> []
  Float {} -> []
  Character {} -> []
  String {} -> []

augmentPattern :: Pattern s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
augmentPattern patternx = Term.Bound {at, select}
  where
    at = TermBinding $ pure $ Wobbly (shiftLogical (Core.mono $ typex patternx))
    select = case patternx of
      Wildcard {} -> Strict.Vector.empty
      List {items} -> Strict.Vector.map augmentPattern items
      Constructor {patterns} -> fmap augmentPattern patterns
      Record {fields} -> fmap Field.augmentField fields
      Integer {} -> Strict.Vector.empty
      Float {} -> Strict.Vector.empty
      Character {} -> Strict.Vector.empty
      String {} -> Strict.Vector.empty

-- |
-- The types that the constructors of a pattern hide, each with where its
-- constructor is.
owned :: Pattern s scope -> [(Position, Unify.Type s scope)]
owned = \case
  Wildcard {} -> []
  Constructor {constructorPosition, hidden, patterns} ->
    [(constructorPosition, typex) | typex <- hidden] ++ foldMap owned patterns
  Record {constructorPosition, hidden, fields} ->
    [(constructorPosition, typex) | typex <- hidden] ++ foldMap Field.owned fields
  List {items} -> foldMap owned items
  Integer {} -> []
  Float {} -> []
  Character {} -> []
  String {} -> []

-- |
-- Check that what a pattern's constructors hide is no part of some types.
-- These are the types that are seen from outside what the pattern is
-- matched for: the type of what is matched, and the type of what is made
-- of it.
sealed :: Context s scope -> Pattern s scope -> [Unify.Type s scope] -> ST s ()
sealed context patternx types = case owned patternx of
  [] -> pure ()
  own@((position, _) : _) -> traverse_ (Unify.sealed context position (snd <$> own)) types

-- |
-- Check that a pattern may be matched late, as the pattern of a binding is:
-- only when one of its variables is asked for. That cannot be for a pattern
-- that hides a type or tells something of one, because what is hidden or
-- told would have to be known wherever the variables are.
bindable :: Position -> Pattern s scope -> ST s ()
bindable position patternx = case (owned patternx, learnt patternx) of
  ([], []) -> pure ()
  _ -> refiningBinding position

-- |
-- Set the type of what is matched against the type a constructor makes.
-- For a constructor that says what type it makes, this may tell something
-- of the type's variables. For any other the two are simply the same.
matched ::
  Context s scope ->
  Position ->
  DataInstance s scope ->
  Type2.Index scope ->
  ConstructorInstance.ConstructorInstance s scope ->
  Unify.Type s scope ->
  ST s [Equality s scope]
matched context position datax typeIndex instancex typex
  | ConstructorInstance.indexed instancex = do
      -- It is a value of the type to begin with, given something.
      Unify.unify context position typex (DataInstance.baseType datax typeIndex)
      let places =
            zip3
              (toList (ConstructorInstance.refined instancex))
              (toList (DataInstance.given datax))
              (toList (ConstructorInstance.result instancex))
      -- Where the constructor gives the type a variable of its own, that
      -- variable is what the type has there.
      for_ [(given, made) | (Prelude.False, given, made) <- places] $ \(given, made) ->
        Unify.unify context position given made
      -- Anywhere else, what the type has there is known to be what the
      -- constructor gives it.
      let refine found (given, made) = do
            more <- Unify.refine (knowing found context) position given made
            pure (more ++ found)
      foldM refine [] [(given, made) | (Prelude.True, given, made) <- places]
  | otherwise = [] <$ Unify.unify context position typex (DataInstance.constructed instancex typeIndex)

-- |
-- A context in which more is known of type variables.
knowing :: [Equality s scope] -> Context s scope -> Context s scope
knowing found context@Context {equalities} = context {equalities = found ++ equalities}

check :: Context s scope -> Unify.Type s scope -> Semantic.Pattern Resolve scope -> ST s (Pattern s scope)
check context@Context {typeEnvironment} typex = \case
  Semantic.Wildcard {names} -> pure Wildcard {typex, names}
  Semantic.Constructor
    { irrefutable,
      constructorPosition,
      constructor,
      patterns,
      names,
      single
    } ->
      do
        let Constructor.Index typeIndex constructorIndex = constructor
        datax <- do
          let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
          Builtin.index pure get typeIndex
        datax@DataInstance {constructors} <-
          Simple.Data.instanciate ConstructorInstance.Match context constructorPosition datax
        let instancex = constructors Strict.Vector.! constructorIndex
            entries = ConstructorInstance.types instancex
            constructorInfo = ConstructorInstance.info instancex
            hidden = ConstructorInstance.hidden instancex
        ConstructorInstance.mark context instancex
        equalities <- matched context constructorPosition datax typeIndex instancex typex
        when (length entries /= length patterns) $ mismatchedConstructorArguments constructorPosition
        patterns <- sequence $ Strict.Vector.zipWith (check (knowing equalities context)) entries patterns
        pure $
          Constructor
            { typex,
              equalities,
              hidden,
              constructorPosition,
              names,
              irrefutable,
              constructor,
              patterns,
              single,
              constructorInfo
            }
  Semantic.Record
    { irrefutable,
      constructorPosition,
      constructor,
      fields,
      names,
      single
    } -> do
      let Constructor.Index typeIndex constructorIndex = constructor
      datax <- do
        let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
        Builtin.index pure get typeIndex
      datax@DataInstance {constructors} <-
        Simple.Data.instanciate ConstructorInstance.Match context constructorPosition datax
      let instancex = constructors Strict.Vector.! constructorIndex
          entries = ConstructorInstance.types instancex
          constructorInfo = ConstructorInstance.info instancex
          hidden = ConstructorInstance.hidden instancex
          lookup index = entries Strict.Vector.! index
      ConstructorInstance.mark context instancex
      equalities <- matched context constructorPosition datax typeIndex instancex typex
      fields <- traverse (Field.check (knowing equalities context) lookup) fields
      pure $
        Record
          { typex,
            equalities,
            hidden,
            names,
            constructorPosition,
            irrefutable,
            constructor,
            fields,
            single,
            constructorInfo
          }
  Semantic.List {irrefutable, names, startPosition, items} -> do
    element <- Unify.fresh Core.typex
    items <- traverse (check context element) items
    Unify.unify context startPosition typex (Core.list element)
    pure $ List {typex, names, startPosition, irrefutable, items}
  Semantic.Character {irrefutable, names, startPosition, character} -> do
    Unify.unify context startPosition typex Core.char
    pure $ Character {typex, names, startPosition, irrefutable, character}
  Semantic.String {irrefutable, names, startPosition, string} -> do
    Unify.unify context startPosition typex (Core.list Core.char)
    pure $ String {typex, names, startPosition, irrefutable, string}
  Semantic.Integer {irrefutable, names, startPosition, integer} -> do
    evidence <- Unify.constrain context startPosition Type2.Num typex
    equal <- Unify.constrain context startPosition Type2.Eq typex
    pure Integer {typex, names, irrefutable, startPosition, integer, evidence, equal}
  Semantic.Float {irrefutable, names, startPosition, float} -> do
    evidence <- Unify.constrain context startPosition Type2.Fractional typex
    equal <- Unify.constrain context startPosition Type2.Eq typex
    pure Float {typex, names, irrefutable, startPosition, float, evidence, equal}

solve :: Pattern s scope -> Unify.Solve s (Solved.Pattern Check scope)
solve = \case
  Wildcard {names} -> pure Solved.Wildcard {names}
  Constructor
    { irrefutable,
      names,
      single,
      constructorPosition,
      constructor,
      patterns,
      constructorInfo
    } -> do
      patterns <- traverse solve patterns
      constructorInfo <- ConstructorInfo.solve constructorInfo
      pure
        Solved.Constructor
          { irrefutable,
            names,
            constructorPosition,
            constructor,
            patterns,
            single,
            constructorInfo = Solved constructorInfo
          }
  Record
    { irrefutable,
      names,
      constructorPosition,
      constructor,
      fields,
      single,
      constructorInfo
    } -> do
      fields <- traverse Field.solve fields
      constructorInfo <- ConstructorInfo.solve constructorInfo
      pure
        Solved.Record
          { irrefutable,
            names,
            constructorPosition,
            constructor,
            fields,
            single,
            constructorInfo = Solved constructorInfo
          }
  List {irrefutable, startPosition, names, items} -> do
    items <- traverse solve items
    pure Solved.List {irrefutable, startPosition, names, items}
  Integer {irrefutable, names, startPosition, integer, evidence, equal} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    equal <- Unify.solveEvidence startPosition equal
    pure
      Solved.Integer
        { irrefutable,
          names,
          startPosition,
          integer,
          evidence = Solved evidence,
          equal = Solved equal
        }
  Float {irrefutable, names, startPosition, float, evidence, equal} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    equal <- Unify.solveEvidence startPosition equal
    pure
      Solved.Float
        { irrefutable,
          names,
          float,
          startPosition,
          evidence = Solved evidence,
          equal = Solved equal
        }
  Character {irrefutable, names, startPosition, character} ->
    pure $
      Solved.Character
        { irrefutable,
          names,
          startPosition,
          character
        }
  String {irrefutable, names, startPosition, string} ->
    pure $
      Solved.String
        { irrefutable,
          names,
          startPosition,
          string
        }
