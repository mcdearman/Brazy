{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- AxCut: the second IR of the native pipeline, which a machine can run as it
-- stands.
--
-- It is the sequent calculus with every cut reduced until a variable stands
-- on one side of it, after Schuster, Müller, Ostermann and Brachthäuser,
-- "Compiling Classical Sequent Calculus to Stock Hardware" (OOPSLA 2025).
-- There are no expressions and there is no cut. There are only statements,
-- each of which is a cut with the rule it meets folded in.
--
-- There is one environment: a row of values. A statement works on the end of
-- the row and says what it leaves there. Nothing in a statement says where a
-- value is; the names are for reading, and for checking that the lowering
-- kept the row in order.
--
-- To the paper's statements are added what a lazy language needs. A thunk is
-- an object with one method, to force it, which the machine replaces with
-- its value. 'Force' evaluates the last value if it is a thunk. 'Rec' makes
-- objects that capture each other.
--
-- At this stage 'Substitute' may copy and drop values freely. Making every
-- name used exactly once, so that references can be counted, is a later pass.
module AxCut.Tree where

import Data.Map (Map)
import Data.Text (Text)
import Generate.Global (Global)
import Sequent.Primitive (Primitive)
import Sequent.Tree (Literal)

data Statement
  = -- | Make the environment again. Each pair is a value of the new
    -- environment and the value of the old one that it is.
    Substitute [(Text, Text)] Statement
  | -- | Construct a value with a tag from the last values, as many as the
    -- count.
    Let !Text !Int !Int Statement
  | -- | Take the last value apart by its tag: its fields take its place. If
    -- no clause has the tag, the value is dropped and the last statement
    -- runs.
    Switch [Clause] (Maybe Statement)
  | -- | Make an object that captures the last values.
    New !Text Object Statement
  | -- | Call the method of the last value, which is an object. The rest of
    -- the environment is the method's arguments.
    Invoke !Method
  | -- | The environment is a consumer and then something that may be a
    -- thunk. Evaluate it and give the value to the consumer.
    Force
  | Literal !Text !Literal Statement
  | -- | A definition at the top of the program, and how many arguments it
    -- takes at once if it is known to be a function. Where that is known,
    -- applying the value to that many is going to the definition's code.
    Global !Text !Global !(Maybe Int) Statement
  | -- | A primitive of the last values, as many as it takes.
    Primitive !Text !Primitive Statement
  | -- | Objects that may capture each other. The environment ends with what
    -- each captures of it, in the order of the objects.
    Rec [Recursive] Statement
  | Abort
  | Halt

data Clause = Clause !Int [Text] Statement

data Kind
  = Closure
  | Thunk
  | -- | A consumer that is only ever given values where it is made, and
    -- never held: it is not made, and what gives it a value goes to it.
    Join
  deriving (Eq, Show)

data Method
  = -- | Of a consumer: take a value.
    Return
  | -- | Of a function: take the arguments and the consumer of the result.
    Apply
  | -- | Of a function that is not known: take each of the arguments in
    -- turn, giving each to what the one before gave, and then the consumer
    -- of the last result. A function that takes that many at once takes
    -- them at once.
    Spread
  | -- | Of a thunk: take the consumer of its value.
    Forced
  deriving (Eq, Show)

-- |
-- An object: what it captures and its one method. The method's body runs
-- with its parameters and then what was captured.
data Object = Object
  { kind :: !Kind,
    captured :: [Text],
    method :: !Method,
    parameters :: [Text],
    body :: Statement,
    -- | Of a function that gives back a function, and so on, each of one
    -- parameter: a second method that takes all of their parameters at
    -- once, and then the consumer of the last result. It runs with those
    -- and then what was captured.
    whole :: Maybe Wholly
  }

data Wholly = Wholly
  { wholeParameters :: [Text],
    wholeBody :: Statement
  }

-- |
-- An object among objects that capture each other. Its captures are some of
-- the environment and then the others it names.
data Recursive = Recursive
  { name :: !Text,
    object :: Object,
    others :: [Text]
  }

-- |
-- A program: its definitions, each an object that captures nothing, and the
-- statement that starts it with an empty environment.
data Program = Program
  { definitions :: Map Global Object,
    entry :: Statement
  }
