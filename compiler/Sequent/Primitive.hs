-- |
-- The primitives of the native pipeline. A primitive takes values that are
-- already evaluated and gives one back. These are what the runtime has to
-- provide; everything else in the @Hazy@ module is written in terms of them
-- in "Sequent.Runtime".
module Sequent.Primitive where

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
  deriving (Eq, Show)

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
