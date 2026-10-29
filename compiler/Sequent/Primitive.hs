-- |
-- The primitives of the native pipeline. A primitive takes values that are
-- already evaluated and gives one back. These are what the runtime has to
-- provide; everything else in the @Hazy@ module is written in terms of them
-- in "Sequent.Runtime".
module Sequent.Primitive where

import qualified Core.Foreign as Foreign
import Data.Text (Text, pack)
import Generate.Global (Global)

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
  | -- | Division that rounds down, and what is left over from it.
    IntDiv
  | IntMod
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
  | -- | A function of C, called as its type says.
    Call !Foreign.Call
  | -- | Where C calls a definition by a name, as a foreign export says,
    -- written where the definition starts. Running it does nothing.
    Export !Foreign.Call !Global
  | -- | A function of C made of a function of the program, as a wrapper
    -- does, given the code that applies the function to what C gives and
    -- the function. It gives the address.
    Wrap !Foreign.Call
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
    ("array_new", 2),
    ("array_read", 2),
    ("array_index", 2),
    ("array_write", 3),
    ("array_length", 1),
    ("float_from_integer", 2),
    ("float_from_int", 2),
    ("float_unary", 3),
    ("float_binary", 4),
    ("float_compare", 2),
    ("float_test", 3),
    ("float_mantissa", 2),
    ("float_exponent", 2),
    ("float_encode", 3),
    ("float_truncate", 1),
    ("integer_bits", 3),
    ("integer_shift", 2),
    ("integer_pop_count", 1),
    ("integer_show", 1),
    ("arg_count", 0),
    ("arg", 1),
    ("get_env", 1),
    ("set_args", 1),
    ("prog_name", 0),
    ("set_prog_name", 1),
    ("set_env", 2),
    ("unset_env", 1),
    ("environment", 0),
    ("executable_path", 0),
    ("exit", 1),
    ("open", 2),
    ("close", 1),
    ("write", 2),
    ("read_all", 1),
    ("read_line", 1),
    ("read_char", 1),
    ("peek_char", 1),
    ("is_eof", 1),
    ("file_state", 1),
    ("file_size", 1),
    ("set_file_size", 2),
    ("seek", 3),
    ("tell", 1),
    ("flush", 1),
    ("last_failure", 0),
    ("file_name", 1),
    ("remove_file", 1),
    ("is_terminal", 1),
    ("list_directory", 1),
    ("create_directory", 1),
    ("path_kind", 1),
    ("call_process", 2),
    ("clock", 0),
    ("os", 0),
    ("catch_push", 2),
    ("catch_pop", 0),
    ("catch_abandon", 1),
    ("catching", 0),
    ("uncaught", 2),
    ("pack_new", 0),
    ("pack_push", 2),
    ("pack_append", 2),
    ("pack_done", 1),
    ("callback_result", 1)
  ]

-- |
-- Whether what a primitive gives back may be yet to be evaluated: what a
-- place holds, which is whatever it was given. Everything else gives a
-- value.
holding :: Primitive -> Bool
holding = \case
  Field _ -> True
  Foreign name _ -> name `elem` map pack ["read_ref", "array_read", "array_index"]
  _ -> False

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
  IntDiv -> 2
  IntMod -> 2
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
  Call Foreign.Call {arguments} -> length arguments
  Export _ _ -> 0
  Wrap _ -> 2
