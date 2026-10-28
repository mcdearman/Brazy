module Semantic.Check.Temporary.Pattern where

import Control.Monad (foldM, when, zipWithM)
import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import qualified Core.Tree.Class as Class (Class (..))
import qualified Core.Tree.Constraint as Constraint (ConstraintF (..))
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import qualified Core.Tree.Forall as Core (mono)
import qualified Core.Tree.Forall as Forall (simplify)
import Semantic.Check.Scheme (instanciate)
import qualified Semantic.Check.TypeAnnotation as Annotation
import qualified Core.Tree.Type as Core (char, list, typex)
import Core.Tree.TypeDeclaration (assumeClass, assumeData)
import Core.Type.Functor (shiftLogical)
import Data.Foldable (for_, toList, traverse_)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Vector.Strict as Strict (Vector)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( derivingRequired,
    mismatchedConstructorArguments,
    refiningBinding,
    unsupportedFeaturePolymorphicComponents,
  )
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import Semantic.Check.Context (Context (..))
import Semantic.Check.Instanciate.Data (DataInstance (DataInstance))
import qualified Semantic.Check.Instanciate.Data as DataInstance
import Semantic.Check.Instanciate.Entry (EntryInstance (..))
import qualified Semantic.Check.Simple.Data as Simple.Data
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import Semantic.Check.Binding.Term (TermBinding (TermBinding), Type (..))
import qualified Semantic.Check.Binding.Type as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Table.Local as Local
import qualified Semantic.Index.Table.Term as Term
import qualified Semantic.Index.Table.Type as Type
import Semantic.Index.Term (Bound)
import qualified Semantic.Index.Term as Bound (Bound (..))
import qualified Semantic.Index.Term as Index (Index (Pattern))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Pattern (Failable (Unused))
import qualified Semantic.Tree.Pattern as Semantic
import qualified Semantic.Tree.Pattern as Solved
import Semantic.Check.Binding.Local (Equality (Given), shiftEquality)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.StringLiteral (StringLiteral)
import Syntax.Variable (Variable)
import Prelude hiding (Bool (False, True))
import qualified Prelude

data Pattern s scope
  = Wildcard
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        -- | Whether what it is matched against is evaluated first: a bang
        -- pattern.
        forced :: !Prelude.Bool
      }
  | -- | A name for something a constructor holds whose type has a @forall@
    -- of its own. Nothing else can be matched against such a thing.
    Polymorphic
      { position :: !Position,
        scheme :: !(Unify.Forall s scope),
        names :: !(Map Variable Position)
      }
  | Match
      { names :: !(Map Variable Position),
        typex :: !(Unify.Type s scope),
        irrefutable :: !Prelude.Bool,
        match :: !(Bindings s scope)
      }

data Bindings s scope
  = Constructor
      { -- | What matching on the constructor tells of the type variables
        -- of the type that is matched on.
        equalities :: ![Equality s scope],
        -- | The types the constructor hides.
        hidden :: ![Unify.Type s scope],
        -- | What is known to hold of types where the match is made: what
        -- the constructor requires of its types.
        required :: ![Required s scope],
        -- | How many things the constructor holds evidence for.
        evident :: !Int,
        constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern s scope)),
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Record
      { -- | What matching on the constructor tells of the type variables
        -- of the type that is matched on.
        equalities :: ![Equality s scope],
        -- | The types the constructor hides.
        hidden :: ![Unify.Type s scope],
        -- | What is known to hold of types where the match is made: what
        -- the constructor requires of its types.
        required :: ![Required s scope],
        -- | How many things the constructor holds evidence for.
        evident :: !Int,
        constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        -- | How many things the constructor holds, apart from evidence.
        arity :: !Int,
        fields :: !(Strict.Vector (Field s scope)),
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Integer
      { startPosition :: !Position,
        integer :: !Integer,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Float
      { startPosition :: !Position,
        float :: !Rational,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Character
      { startPosition :: !Position,
        character :: !Char
      }
  | String
      { startPosition :: !Position,
        string :: !StringLiteral
      }
  | List
      { startPosition :: !Position,
        items :: !(Strict.Vector1 (Pattern s scope))
      }

-- |
-- Something that holds of a type inside a match on a constructor that
-- requires it. The value that is matched holds the evidence: this says
-- which of the things it holds evidence for, counted from the first, and
-- then which class that one's class requires in turn, and so on, if it is
-- not that one itself.
data Required s scope = Required
  { classx :: !(Type2.Index scope),
    subject :: !(Unify.Type s scope),
    slot :: !Int,
    supers :: ![Int]
  }

-- |
-- Everything that holds when some things are required: each of them, and
-- whatever the class of each requires of the same type.
requirements :: Context s scope -> [(Type2.Index scope, Unify.Type s scope)] -> ST s [Required s scope]
requirements Context {typeEnvironment} asked = concat <$> zipWithM top [0 ..] asked
  where
    top slot (classx, subject) = entailed slot [] classx subject
    entailed slot supers classx subject = do
      Class.Class {constraints} <- do
        let get index = assumeClass <$> TypeBinding.content (typeEnvironment Type.! index)
        Builtin.index pure get classx
      children <- for (zip [0 ..] $ toList constraints) $ \(index, Constraint.Constraint {classx = super, arguments}) ->
        if null arguments
          then entailed slot (supers ++ [index]) super subject
          else pure []
      pure (Required {classx, subject, slot, supers} : concat children)

-- |
-- What holds inside a match on a pattern, with the evidence named as the
-- match names it: after what the pattern says its constructor holds comes
-- the evidence the constructor holds.
given :: (Bound -> Bound) -> Pattern s scope -> [Equality s (Scope.Pattern ':+ scope)]
given path = \case
  Wildcard {} -> []
  Polymorphic {} -> []
  Match {match} -> case match of
    Constructor {required, patterns} ->
      evidence path (length patterns) required
        ++ concat (zipWith (\index -> given (path . Bound.Select index)) [0 ..] (toList patterns))
    Record {required, fields} ->
      evidence path (length fields) required
        ++ concat (zipWith (\index Field {field} -> given (path . Bound.Select index) field) [0 ..] (toList fields))
    List {items} -> concat (zipWith (\index -> given (path . Bound.Select index)) [0 ..] (toList items))
    Integer {} -> []
    Float {} -> []
    Character {} -> []
    String {} -> []
  where
    evidence :: (Bound -> Bound) -> Int -> [Required s scope] -> [Equality s (Scope.Pattern ':+ scope)]
    evidence path base required = do
      Required {classx, subject, slot, supers} <- required
      let held = Evidence.Held {term = Index.Pattern (path (Bound.Select (base + slot) Bound.At))}
          super base index = Evidence.Super {base, index}
      pure (Given (shift classx) (shiftLogical subject) (foldl super held supers))

data Field s scope = Field
  { index :: !Int,
    field :: !(Pattern s scope)
  }

(!) :: Pattern s scope -> Bound -> Unify.Type s scope
Polymorphic {position} ! _ = unsupportedFeaturePolymorphicComponents position
patternx ! Bound.At = typex patternx
patternx ! Bound.Select index bound = case patternx of
  Wildcard {} -> error "bad index"
  Polymorphic {} -> error "bad index"
  Match {match} -> case match of
    Constructor {patterns} -> patterns Strict.Vector.! index ! bound
    Record {fields} -> fields Strict.Vector.! index !! bound
      where
        Field {field} !! bound = field ! bound
    List {items} -> items Strict.Vector.! index ! bound
    Integer {} -> error "bad index"
    Float {} -> error "bad index"
    Character {} -> error "bad index"
    String {} -> error "bad index"

augment :: Pattern s scope -> Context s scope -> Context s (Scope.Pattern ':+ scope)
augment patternx Context {termEnvironment, localEnvironment, typeEnvironment, equalities, defaulting} =
  Context
    { termEnvironment = Term.Pattern (augmentPattern patternx) termEnvironment,
      localEnvironment = Local.Pattern localEnvironment,
      typeEnvironment = Type.Pattern typeEnvironment,
      equalities = fmap shiftEquality (learnt patternx ++ equalities) ++ given id patternx,
      defaulting
    }

-- |
-- Everything that matching a pattern tells of type variables, from the
-- pattern and from the patterns inside it.
learnt :: Pattern s scope -> [Equality s scope]
learnt = \case
  Wildcard {} -> []
  Polymorphic {} -> []
  Match {match} -> case match of
    Constructor {equalities, patterns} -> foldMap learnt patterns ++ equalities
    Record {equalities, fields} -> foldMap (\Field {field} -> learnt field) fields ++ equalities
    List {items} -> foldMap learnt items
    Integer {} -> []
    Float {} -> []
    Character {} -> []
    String {} -> []

augmentPattern :: Pattern s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
augmentPattern patternx = Term.Bound {at, select}
  where
    at = TermBinding $ pure $ Wobbly $ case patternx of
      Polymorphic {scheme} -> shiftLogical scheme
      _ -> shiftLogical (Core.mono $ typex patternx)
    select = case patternx of
      Wildcard {} -> Strict.Vector.empty
      Polymorphic {} -> Strict.Vector.empty
      Match {match} -> case match of
        List {items} -> Strict.Vector.map augmentPattern items
        Constructor {patterns} -> fmap augmentPattern patterns
        Record {fields} -> fmap augmentField fields
          where
            augmentField Field {field} = augmentPattern field
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
  Polymorphic {} -> []
  Match {match} -> case match of
    Constructor {constructorPosition, hidden, patterns} ->
      [(constructorPosition, typex) | typex <- hidden] ++ foldMap owned patterns
    Record {constructorPosition, hidden, fields} ->
      [(constructorPosition, typex) | typex <- hidden] ++ foldMap (\Field {field} -> owned field) fields
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
            -- What it tells of the types this constructor hides is so
            -- wherever they are, and is not kept as something known here.
            taken <- traverse (Unify.absorb (ConstructorInstance.hidden instancex)) more
            pure ([equality | (equality, Prelude.False) <- zip more taken] ++ found)
      -- And the types the constructor says are the same, are.
      foldM refine [] ([(given, made) | (Prelude.True, given, made) <- places] ++ ConstructorInstance.equated instancex)
  | otherwise = [] <$ Unify.unify context position typex (DataInstance.constructed instancex typeIndex)

-- |
-- The context in which what a constructor holds is looked at, where a value
-- of a type is taken apart that is known to be made with that constructor.
-- An instance that is derived looks at what each constructor holds this way.
within ::
  Context s scope ->
  Position ->
  DataInstance s scope ->
  Type2.Index scope ->
  ConstructorInstance.ConstructorInstance s scope ->
  ST s (Context s scope)
within context position datax typeIndex instancex
  -- The evidence a value holds would have to be named by what is derived.
  | not (null (ConstructorInstance.asked instancex)) = derivingRequired position
  | otherwise = do
      equalities <- matched context position datax typeIndex instancex (DataInstance.baseType datax typeIndex)
      pure (knowing equalities context)

-- |
-- A context in which more is known of type variables.
knowing :: [Equality s scope] -> Context s scope -> Context s scope
knowing found Context {termEnvironment, localEnvironment, typeEnvironment, equalities, defaulting} =
  Context {termEnvironment, localEnvironment, typeEnvironment, equalities = found ++ equalities, defaulting}

-- |
-- Check the pattern for something a constructor holds.
held :: Context s scope -> EntryInstance s scope -> Semantic.Pattern Position Resolve scope -> ST s (Pattern s scope)
held context EntryInstance {quantified = Prelude.False, entry} patternx = check context entry patternx
held _ EntryInstance {position, scheme} Semantic.Wildcard {names} = pure Polymorphic {position, scheme, names}
held _ EntryInstance {position} _ = unsupportedFeaturePolymorphicComponents position

-- |
-- Check the patterns for the things a constructor holds, first to last.
-- What one tells of types is known for the ones after it: two of them may
-- each tell something of the same type, and the second is then read by
-- what the first said.
inOrder ::
  Context s scope ->
  [(EntryInstance s scope, Semantic.Pattern Position Resolve scope)] ->
  ST s [Pattern s scope]
inOrder context = \case
  [] -> pure []
  (entry, patternx) : rest -> do
    patternx <- held context entry patternx
    rest <- inOrder (knowing (learnt patternx) context) rest
    pure (patternx : rest)

check :: Context s scope -> Unify.Type s scope -> Semantic.Pattern Position Resolve scope -> ST s (Pattern s scope)
check context@Context {typeEnvironment} typex = \case
  -- The type a pattern says is what it is matched against, a type variable
  -- of its own being any type, and the pattern inside is matched as it is.
  Semantic.Signature {names = outer, patternx, signatureStart, signature} -> do
    annotation <- Annotation.checkAnnotation context signature
    (said, _) <- instanciate context signatureStart (Forall.simplify annotation)
    Unify.unify context signatureStart typex said
    patternx <- check context typex patternx
    pure patternx {names = outer <> names patternx}
  Semantic.Wildcard {names, forced} -> pure Wildcard {typex, names, forced}
  Semantic.Match {names, irrefutable, match} -> do
    match <- case match of
      Semantic.Constructor
        { constructorPosition,
          constructor,
          patterns
        } ->
          do
            let Constructor.Index typeIndex constructorIndex = constructor
            datax <- do
              let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
              Builtin.index pure get typeIndex
            datax@DataInstance {constructors} <-
              Simple.Data.instanciate ConstructorInstance.Match context constructorPosition datax
            let instancex = constructors Strict.Vector.! constructorIndex
                entries = ConstructorInstance.entries instancex
                constructorInfo = ConstructorInstance.info instancex
                hidden = ConstructorInstance.hidden instancex
                evident = length (ConstructorInstance.asked instancex)
            ConstructorInstance.mark context instancex
            equalities <- matched context constructorPosition datax typeIndex instancex typex
            required <- requirements context (ConstructorInstance.asked instancex)
            when (length entries /= length patterns) $ mismatchedConstructorArguments constructorPosition
            patterns <-
              fmap Strict.Vector.fromList $
                inOrder (knowing equalities context) (zip (toList entries) (toList patterns))
            pure $
              Constructor
                { equalities,
                  hidden,
                  required,
                  evident,
                  constructorPosition,
                  constructor,
                  patterns,
                  constructorInfo
                }
      Semantic.Record
        { constructorPosition,
          constructor,
          fields
        } -> do
          let Constructor.Index typeIndex constructorIndex = constructor
          datax <- do
            let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
            Builtin.index pure get typeIndex
          datax@DataInstance {constructors} <-
            Simple.Data.instanciate ConstructorInstance.Match context constructorPosition datax
          let instancex = constructors Strict.Vector.! constructorIndex
              entries = ConstructorInstance.entries instancex
              constructorInfo = ConstructorInstance.info instancex
              hidden = ConstructorInstance.hidden instancex
              evident = length (ConstructorInstance.asked instancex)
              lookup index = entries Strict.Vector.! index
          ConstructorInstance.mark context instancex
          equalities <- matched context constructorPosition datax typeIndex instancex typex
          required <- requirements context (ConstructorInstance.asked instancex)
          let checkField Semantic.Field {index, field} = do
                field <- held (knowing equalities context) (lookup index) field
                pure Field {index, field}
          fields <- traverse checkField fields
          pure $
            Record
              { equalities,
                hidden,
                required,
                evident,
                arity = length entries,
                constructorPosition,
                constructor,
                fields,
                constructorInfo
              }
      Semantic.List {startPosition, items} -> do
        element <- Unify.fresh Core.typex
        items <- traverse (check context element) items
        Unify.unify context startPosition typex (Core.list element)
        pure $ List {startPosition, items}
      Semantic.Character {startPosition, character} -> do
        Unify.unify context startPosition typex Core.char
        pure $ Character {startPosition, character}
      Semantic.String {startPosition, string} -> do
        Unify.unify context startPosition typex (Core.list Core.char)
        pure $ String {startPosition, string}
      Semantic.Integer {startPosition, integer} -> do
        evidence <- Unify.constrain context startPosition Type2.Num typex
        equal <- Unify.constrain context startPosition Type2.Eq typex
        pure Integer {startPosition, integer, evidence, equal}
      Semantic.Float {startPosition, float} -> do
        evidence <- Unify.constrain context startPosition Type2.Fractional typex
        equal <- Unify.constrain context startPosition Type2.Eq typex
        pure Float {startPosition, float, evidence, equal}
    pure Match {typex, names, irrefutable, match}

-- |
-- A match on something a value holds that the pattern does not say: the
-- evidence, which is held after everything else.
unnamed :: Int -> Solved.Field Position Check scope
unnamed index = Solved.Field {index, field = Solved.Wildcard {names = Map.empty, forced = Prelude.False}}

solve :: Pattern s scope -> Unify.Solve s (Solved.Pattern Position Check scope)
solve = \case
  Wildcard {names, forced} -> pure Solved.Wildcard {names, forced}
  Polymorphic {names} -> pure Solved.Wildcard {names, forced = Prelude.False}
  Match {names, irrefutable, match} -> do
    match <- case match of
      Constructor
        { constructorPosition,
          constructor,
          patterns,
          evident,
          constructorInfo
        } -> do
          patterns <- traverse solve patterns
          -- The evidence the value holds is matched as well, so that it has
          -- a name: it comes after everything the pattern says.
          patterns <- pure (patterns <> Strict.Vector.replicate evident Solved.Wildcard {names = Map.empty, forced = Prelude.False})
          constructorInfo <- ConstructorInfo.solve constructorInfo
          pure
            Solved.Constructor
              { constructorPosition,
                constructor,
                patterns,
                constructorInfo = Solved constructorInfo,
                failable = Unused
              }
      Record
        { constructorPosition,
          constructor,
          fields,
          evident,
          arity,
          constructorInfo
        } -> do
          let solveField Field {index, field} = do
                field <- solve field
                pure Solved.Field {index, field}
          fields <- traverse solveField fields
          fields <- pure $ fields <> Strict.Vector.fromList (unnamed <$> [arity .. arity + evident - 1])
          constructorInfo <- ConstructorInfo.solve constructorInfo
          pure
            Solved.Record
              { constructorPosition,
                constructor,
                fields,
                constructorInfo = Solved constructorInfo,
                failable = Unused
              }
      List {startPosition, items} -> do
        items <- traverse solve items
        pure Solved.List {startPosition, items}
      Integer {startPosition, integer, evidence, equal} -> do
        evidence <- Unify.solveEvidence startPosition evidence
        equal <- Unify.solveEvidence startPosition equal
        pure
          Solved.Integer
            { startPosition,
              integer,
              evidence = Solved evidence,
              equal = Solved equal
            }
      Float {startPosition, float, evidence, equal} -> do
        evidence <- Unify.solveEvidence startPosition evidence
        equal <- Unify.solveEvidence startPosition equal
        pure
          Solved.Float
            { float,
              startPosition,
              evidence = Solved evidence,
              equal = Solved equal
            }
      Character {startPosition, character} ->
        pure $
          Solved.Character
            { startPosition,
              character
            }
      String {startPosition, string} ->
        pure $
          Solved.String
            { startPosition,
              string
            }
    pure Solved.Match {names, irrefutable, match}
