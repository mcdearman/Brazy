{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Pattern where

import qualified Core.Tree.Evidence as Simple
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Info.Constructor (ConstructorInfo)
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve, Stage)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Scheme (Scheme)
import Syntax.Position (Position)
import Syntax.StringLiteral (StringLiteral)
import Syntax.Variable (Variable)
import Prelude hiding (Bool (False, True), Either (Left, Right), head, tail)
import qualified Prelude

data Pattern position stage scope
  = Wildcard
      { names :: !(Map Variable position),
        -- | Whether what it is matched against is evaluated: a bang
        -- pattern, @!x@ or @!_@.
        forced :: !Prelude.Bool
      }
  | Match
      { names :: !(Map Variable position),
        irrefutable :: !Prelude.Bool,
        match :: !(Bindings position stage scope)
      }
  | -- | A pattern that says its type, @(x :: t)@. The checker reads the
    -- type and leaves the pattern inside, so only the resolver makes these.
    Signature
      { names :: !(Map Variable position),
        patternx :: !(Pattern position stage scope),
        signatureStart :: !position,
        signature :: !(Scheme position stage scope)
      }
  deriving (Show)

instance Shift0.Functor (Pattern position stage) where
  map = Shift.mapDefault

instance Shift.Functor (Pattern position stage) where
  map category = \case
    Wildcard {names, forced} -> Wildcard {names, forced}
    Match {names, irrefutable, match} ->
      Match
        { names,
          irrefutable,
          match = Shift.map category match
        }
    Signature {names, patternx, signatureStart, signature} ->
      Signature
        { names,
          patternx = Shift.map category patternx,
          signatureStart,
          signature = Shift.map category signature
        }

data Bindings position stage scope
  = Constructor
      { constructorPosition :: !position,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern position stage scope)),
        failable :: !(Failable stage),
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Record
      { constructorPosition :: !position,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field position stage scope)),
        failable :: !(Failable stage),
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Integer
      { startPosition :: !position,
        integer :: !Integer,
        evidence :: !(Inferred Simple.Evidence stage scope),
        equal :: !(Inferred Simple.Evidence stage scope)
      }
  | Float
      { startPosition :: !position,
        float :: !Rational,
        evidence :: !(Inferred Simple.Evidence stage scope),
        equal :: !(Inferred Simple.Evidence stage scope)
      }
  | Character
      { startPosition :: !position,
        character :: !Char
      }
  | String
      { startPosition :: !position,
        string :: !StringLiteral
      }
  | List
      { startPosition :: !position,
        items :: !(Strict.Vector1 (Pattern position stage scope))
      }
  deriving (Show)

instance Shift0.Functor (Bindings position stage) where
  map = Shift.mapDefault

instance Shift.Functor (Bindings position stage) where
  map category = \case
    Constructor {constructorPosition, constructor, patterns, failable, constructorInfo} ->
      Constructor
        { constructorPosition,
          constructor = Shift.map category constructor,
          patterns = fmap (Shift.map category) patterns,
          failable,
          constructorInfo = Shift.map category constructorInfo
        }
    Record {constructorPosition, constructor, fields, failable, constructorInfo} ->
      Record
        { constructorPosition,
          constructor = Shift.map category constructor,
          fields = fmap (Shift.map category) fields,
          failable,
          constructorInfo = Shift.map category constructorInfo
        }
    Integer {startPosition, integer, evidence, equal} ->
      Integer
        { startPosition,
          integer,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Float {startPosition, float, evidence, equal} ->
      Float
        { startPosition,
          float,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Character {startPosition, character} ->
      Character {startPosition, character}
    String {startPosition, string} ->
      String
        { startPosition,
          string
        }
    List {startPosition, items} ->
      List
        { startPosition,
          items = fmap (Shift.map category) items
        }

data Failable stage where
  Exhaustive :: Failable Resolve
  Failable :: Failable Resolve
  Unused :: Failable Check

instance Show (Failable stage) where
  showsPrec _ = \case
    Exhaustive -> showString "Exhaustive"
    Failable -> showString "Failable"
    Unused -> showString "Unused"

data Field position stage scope = Field
  { index :: !Int,
    field :: !(Pattern position stage scope)
  }
  deriving (Show)

instance Shift0.Functor (Field position stage) where
  map = Shift.mapDefault

instance Shift.Functor (Field position stage) where
  map category = \case
    Field {index, field} -> Field {index, field = Shift.map category field}

lazy :: Pattern position stage scope -> Pattern position stage scope
lazy = \case
  Wildcard {names, forced} -> Wildcard {names, forced}
  Match {names, match} -> Match {names, irrefutable = Prelude.True, match}
  Signature {names, patternx, signatureStart, signature} -> Signature {names, patternx = lazy patternx, signatureStart, signature}

type NeverFails :: (Type -> Stage -> Environment -> Type) -> Constraint
class NeverFails patternx where
  neverFails :: patternx position Resolve scope -> Prelude.Bool

instance NeverFails Pattern where
  neverFails = \case
    Wildcard {} -> Prelude.True
    Signature {patternx} -> neverFails patternx
    Match {irrefutable, match}
      | irrefutable -> Prelude.True
      | otherwise -> neverFails match

instance NeverFails Bindings where
  neverFails = \case
    Constructor {failable, patterns} -> case failable of
      Exhaustive -> all neverFails patterns
      Failable -> Prelude.False
    Record {failable, fields} -> case failable of
      Exhaustive -> all neverFails fields
      Failable -> Prelude.False
    Integer {} -> Prelude.False
    Float {} -> Prelude.False
    Character {} -> Prelude.False
    String {} -> Prelude.False
    List {} -> Prelude.False

instance NeverFails Field where
  neverFails Field {field} = neverFails field

type Anonymize :: (Type -> Stage -> Environment -> Type) -> Constraint
class Anonymize patternx where
  anonymize :: patternx position resolve scope -> patternx () resolve scope
  anonymize = within Prelude.False

  -- | The same, for a pattern that may be inside one that is lazy. Nothing
  -- of what a lazy pattern is matched against is looked at until something
  -- it binds is wanted, and that goes for the patterns inside it: they are
  -- lazy too. Inside a lazy pattern a bang only says what a name is when it
  -- is wanted, and that is evaluated then anyway.
  within :: Prelude.Bool -> patternx position resolve scope -> patternx () resolve scope

instance Anonymize Pattern where
  within outer = \case
    Wildcard {names, forced} -> Wildcard {names = Map.map (const ()) names, forced = forced && not outer}
    Match {names, irrefutable, match} ->
      Match
        { names = Map.map (const ()) names,
          irrefutable = lazily,
          match = within lazily match
        }
      where
        lazily = outer || irrefutable
    Signature {} -> error "a pattern the checker does not leave"

instance Anonymize Bindings where
  within outer = \case
    Constructor {constructor, patterns, failable, constructorInfo} ->
      Constructor
        { constructorPosition = (),
          constructor,
          failable,
          patterns = within outer <$> patterns,
          constructorInfo
        }
    Record {constructor, fields, failable, constructorInfo} ->
      Record
        { constructorPosition = (),
          constructor,
          failable,
          fields = within outer <$> fields,
          constructorInfo
        }
    Integer {integer, evidence, equal} ->
      Integer
        { startPosition = (),
          integer,
          evidence,
          equal
        }
    Float {float, evidence, equal} ->
      Float
        { startPosition = (),
          float,
          evidence,
          equal
        }
    Character {character} ->
      Character
        { startPosition = (),
          character
        }
    String {string} ->
      String
        { startPosition = (),
          string
        }
    List {items} ->
      List
        { startPosition = (),
          items = within outer <$> items
        }

instance Anonymize Field where
  within outer Field {index, field} =
    Field
      { index,
        field = within outer field
      }

variable :: Position -> Variable -> Pattern Position stage scope
variable position name = Wildcard {names = Map.singleton name position, forced = Prelude.False}
