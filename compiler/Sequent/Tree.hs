{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- The sequent calculus: the first IR of the native pipeline, after Core.
--
-- It is a focused, call by value λμμ̃ calculus with no types. A program is
-- made of producers, which make values, consumers, which use them, and
-- statements, which are what runs. The one way to compute is a cut: a
-- producer set against a consumer.
--
-- Haskell is lazy and this calculus is not, so laziness is written out. A
-- variable stands for either a value or a thunk. Forcing it evaluates a
-- thunk, once, and leaves a value alone. Every thunk that is made and every
-- force is visible here, so that a later pass can remove the ones it can
-- prove are not needed.
module Sequent.Tree where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Generate.Global (Global)
import Sequent.Primitive (Primitive)

-- |
-- How a binder is evaluated. This follows the strictness design in the
-- roadmap.
data Strategy
  = -- | Evaluated at first use and then kept: the variable may hold a thunk.
    ByNeed
  | -- | Evaluated when bound: the variable always holds a value.
    ByValue
  | -- | Evaluated at every use and never kept. Nothing makes these yet.
    ByName
  deriving (Eq, Show)

data Binder = Binder
  { name :: !Text,
    strategy :: !Strategy
  }

data Literal
  = -- | An @Int@ or a @Char@.
    Int !Int
  | Integer !Integer
  | Text !Text
  | -- | A @Double@, or a @Float@ as the @Double@ it is exactly.
    Double !Double
  deriving (Eq, Show)

data Producer
  = Variable !Text
  | -- | A definition at the top of the program.
    Global !Global
  | Literal !Literal
  | -- | A constructor's tag and its fields. The fields are not forced.
    Constructor !Int [Producer]
  | -- | @μα. s@: run the statement, with α naming whatever consumes this.
    Mu !Text Statement
  | -- | A function: its parameters, the consumer of its result and its body.
    -- A function is a value.
    Function [Binder] !Text Statement
  | -- | Something not yet evaluated. Forcing it runs the statement, whose
    -- consumer is given the value. The value is kept, so it runs once.
    Thunk !Text Statement

data Consumer
  = Covariable !Text
  | -- | @μ̃x. s@: name the value and go on.
    MuTilde !Binder Statement
  | -- | Take a constructed value apart by its tag. If no alternative has
    -- the tag, the last statement runs.
    Case [Alternative] (Maybe Statement)
  | -- | Call a function with all of its arguments, the result going to a
    -- consumer.
    Apply [Producer] Consumer
  | -- | Evaluate a thunk, the value going to a consumer. A value goes
    -- straight there.
    Force Consumer

data Alternative = Alternative !Int [Binder] Statement

data Statement
  = Cut Producer Consumer
  | -- | A primitive of values that are already evaluated.
    Primitive !Primitive [Producer] Consumer
  | -- | Functions and thunks that may refer to each other, and then a
    -- statement in their scope.
    LetRec [(Text, Producer)] Statement
  | -- | No pattern matched.
    Abort
  | -- | The program is over.
    Halt

-- |
-- A program: its definitions, each a function or a thunk, and the statement
-- that starts it.
data Program = Program
  { definitions :: Map Global Producer,
    entry :: Statement
  }

-- |
-- What stands between the parts of the name of a definition that was inside
-- another: the name of the outer one, this, and its own name. It cannot be
-- written in a program, so such a name is never a program's own. It is shown
-- as a dot.
separator :: Char
separator = '\x1f'

-- |
-- The variables and covariables something uses and does not bind.
class Free a where
  free :: a -> Set Text

instance (Free a) => Free [a] where
  free = foldMap free

instance (Free a) => Free (Maybe a) where
  free = foldMap free

bound :: [Binder] -> Set Text
bound binders = Set.fromList [name | Binder {name} <- binders]

instance Free Producer where
  free = \case
    Variable name -> Set.singleton name
    Global _ -> Set.empty
    Literal _ -> Set.empty
    Constructor _ fields -> free fields
    Mu covariable statement -> Set.delete covariable (free statement)
    Function parameters covariable body -> Set.delete covariable (free body) Set.\\ bound parameters
    Thunk covariable body -> Set.delete covariable (free body)

instance Free Consumer where
  free = \case
    Covariable name -> Set.singleton name
    MuTilde Binder {name} statement -> Set.delete name (free statement)
    Case alternatives otherwise -> free alternatives <> free otherwise
    Apply arguments consumer -> free arguments <> free consumer
    Force consumer -> free consumer

instance Free Alternative where
  free (Alternative _ binders statement) = free statement Set.\\ bound binders

instance Free Statement where
  free = \case
    Cut producer consumer -> free producer <> free consumer
    Primitive _ arguments consumer -> free arguments <> free consumer
    LetRec bindings statement ->
      (free (map snd bindings) <> free statement) Set.\\ Set.fromList (map fst bindings)
    Abort -> Set.empty
    Halt -> Set.empty

-- |
-- The definitions something mentions.
class Globals a where
  globals :: a -> Set Global

instance (Globals a) => Globals [a] where
  globals = foldMap globals

instance (Globals a) => Globals (Maybe a) where
  globals = foldMap globals

instance Globals Producer where
  globals = \case
    Variable _ -> Set.empty
    Global global -> Set.singleton global
    Literal _ -> Set.empty
    Constructor _ fields -> globals fields
    Mu _ statement -> globals statement
    Function _ _ body -> globals body
    Thunk _ body -> globals body

instance Globals Consumer where
  globals = \case
    Covariable _ -> Set.empty
    MuTilde _ statement -> globals statement
    Case alternatives otherwise -> globals alternatives <> globals otherwise
    Apply arguments consumer -> globals arguments <> globals consumer
    Force consumer -> globals consumer

instance Globals Alternative where
  globals (Alternative _ _ statement) = globals statement

instance Globals Statement where
  globals = \case
    Cut producer consumer -> globals producer <> globals consumer
    Primitive _ arguments consumer -> globals arguments <> globals consumer
    LetRec bindings statement -> globals (map snd bindings) <> globals statement
    Abort -> Set.empty
    Halt -> Set.empty

-- |
-- A number made an Int, which is 63 bits wherever the program runs: what
-- does not fit comes round, as arithmetic on an Int does. It is worked out
-- on an Integer, so that the compiler gives the same number whether it was
-- built by GHC, whose Int is wider, or by itself.
int63 :: Integer -> Int
int63 number = fromInteger (((number + half) `mod` (2 * half)) - half)
  where
    half = 2 ^ (62 :: Int)

intMinBound, intMaxBound :: Int
intMinBound = int63 (negate (2 ^ (62 :: Int)))
intMaxBound = int63 (2 ^ (62 :: Int) - 1)
