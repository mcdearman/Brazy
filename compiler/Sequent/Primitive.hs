-- |
-- The primitives of the native pipeline. A primitive takes values that are
-- already evaluated and gives one back. These are what the runtime has to
-- provide; everything else in the @Hazy@ module is written in terms of them
-- in "Sequent.Runtime".
module Sequent.Primitive where

import Data.Text (Text)

data Primitive
  = -- | A field of a constructed value, counting from zero.
    Field !Int
  | -- | Stop the program with a message. Gives nothing back.
    Error
  | PutStrLn
  | TextSnoc
  | TextNull
  | TextHead
  | TextTail
  | TextAppend
  | TextEqual
  | TextCompare
  | TextLength
  | TextTake
  | TextDrop
  | TextIsPrefixOf
  | GeneralCategory
  | IntEqual
  | IntLessThenEqual
  | IntAdd
  | IntMinus
  | IntMultiply
  | IntNegate
  | IntAbs
  | IntSignum
  | IntQuot
  | IntRem
  | IntToInteger
  | IntegerCastToInt
  | IntegerTruncateToInt
  | IntegerEqual
  | IntegerLessThenEqual
  | IntegerAdd
  | IntegerMinus
  | IntegerMultiply
  | IntegerNegate
  | IntegerAbs
  | IntegerSignum
  | IntegerQuot
  | IntegerRem
  | -- | Something the runtime does under a name of its own, given so many
    -- values: the world outside a program, and what changes. Nothing is
    -- known of one of these but that it is done where it is written.
    Foreign !Text !Int
  deriving (Eq, Show)

-- |
-- The primitives the runtime has under names of their own, each with how
-- many values it is given. The native runtime calls one @hazy_prim_@ and
-- then its name.
outside :: [(String, Int)]
outside =
  [ ("new_ref", 1),
    ("read_ref", 1),
    ("write_ref", 2),
    ("same_ref", 2),
    ("arg_count", 0),
    ("arg", 1),
    ("get_env", 1),
    ("executable_path", 0),
    ("exit", 1),
    ("open", 2),
    ("close", 1),
    ("write", 2),
    ("read_all", 1),
    ("read_line", 1),
    ("is_terminal", 1),
    ("list_directory", 1),
    ("create_directory", 1),
    ("path_kind", 1),
    ("call_process", 2),
    ("clock", 0),
    ("os", 0),
    ("catch_push", 2),
    ("catch_pop", 0)
  ]

arity :: Primitive -> Int
arity = \case
  Field _ -> 1
  Error -> 1
  PutStrLn -> 1
  TextSnoc -> 2
  TextNull -> 1
  TextHead -> 1
  TextTail -> 1
  TextAppend -> 2
  TextEqual -> 2
  TextCompare -> 2
  TextLength -> 1
  TextTake -> 2
  TextDrop -> 2
  TextIsPrefixOf -> 2
  GeneralCategory -> 1
  IntEqual -> 2
  IntLessThenEqual -> 2
  IntAdd -> 2
  IntMinus -> 2
  IntMultiply -> 2
  IntNegate -> 1
  IntAbs -> 1
  IntSignum -> 1
  IntQuot -> 2
  IntRem -> 2
  IntToInteger -> 1
  IntegerCastToInt -> 1
  IntegerTruncateToInt -> 1
  IntegerEqual -> 2
  IntegerLessThenEqual -> 2
  IntegerAdd -> 2
  IntegerMinus -> 2
  IntegerMultiply -> 2
  IntegerNegate -> 1
  IntegerAbs -> 1
  IntegerSignum -> 1
  IntegerQuot -> 2
  IntegerRem -> 2
  Foreign _ count -> count
