{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- |
-- This is the module for accessing primitives defined in Javascript
module Hazy where

import {-# BUILTIN #-} Hazy.Builtin
import Hazy.Prelude (GeneralCategory, IO, Text)

errorText :: Text -> a
pack :: [Char] -> Text
unpack :: Text -> [Char]
putStrLnText :: Text -> IO ()
traceText :: Text -> a -> a
generalCategory :: Char -> GeneralCategory
primIntToChar :: Int -> Char
primCharToInt :: Char -> Int
primEqualInt, primLessThenEqualInt :: Int -> Int -> Bool
primIntMinBound, primIntMaxBound :: Int
primIntAdd, primIntMinus, primIntMultiply :: Int -> Int -> Int
primIntNegate :: Int -> Int
primIntAbs, primIntSignum :: Int -> Int
primEqualInteger, primLessThenEqualInteger :: Integer -> Integer -> Bool
primIntToInteger :: Int -> Integer
primIntQuot, primIntRem :: Int -> Int -> Int
primIntDiv, primIntMod :: Int -> Int -> Int
primIntegerCastToInt, primIntegerTruncateToInt :: Integer -> Int
primIntegerEqual, primIntegerLessThenEqual :: Integer -> Integer -> Bool
primIntegerAdd, primIntegerMinus, primIntegerMultiply :: Integer -> Integer -> Integer
primIntegerNegate :: Integer -> Integer
primIntegerAbs, primIntegerSignum :: Integer -> Integer
primIntegerQuot, primIntegerRem :: Integer -> Integer -> Integer
primTextNull :: Text -> Bool
primTextHead :: Text -> Char
primTextTail :: Text -> Text
primTextSnoc :: Text -> Char -> Text
primTextAppend :: Text -> Text -> Text
primTextEqual :: Text -> Text -> Bool

-- Which of two texts comes first, character by character: below zero, zero
-- or above it.
primTextCompare :: Text -> Text -> Int
primTextLength :: Text -> Int

-- So many characters from the start of a text, and what is left without
-- them.
primTextTake :: Int -> Text -> Text
primTextDrop :: Int -> Text -> Text
primTextIsPrefixOf :: Text -> Text -> Bool

-- Texts one after another as one text.
primTextConcat :: [Text] -> Text
primSTPure :: a -> ST s a
primSTBind :: ST s a -> (a -> ST s b) -> ST s b

-- A value as it is, at any type. This is as safe as whoever uses it.
primUnsafeCoerce :: a -> b

-- A place that holds a value, which can be changed for another. The type of
-- the place is whatever its user says: the library has the types that are
-- safe.
primNewRef :: a -> ST s r
primReadRef :: r -> ST s a
primWriteRef :: r -> a -> ST s ()
primSameRef :: r -> r -> Bool

-- Places one after another, each holding a value, evaluated or not: so many
-- of them, each holding one value to begin with. One that is no longer
-- changed is read without an action. The types are whatever their user
-- says, as for a place.
primArrayNew :: Int -> a -> ST s m
primArrayRead :: m -> Int -> ST s a
primArrayWrite :: m -> Int -> a -> ST s ()
primArrayIndex :: v -> Int -> a
primArrayLength :: v -> Int

-- Floating point. Each is told its precision, 32 for a Float and 64 for a
-- Double, and the type is whatever its user says. The functions and the
-- operators are known by number: see "Hazy.Prelude".
primFloatFromInteger :: Int -> Integer -> a
primFloatUnary :: Int -> Int -> a -> a
primFloatBinary :: Int -> Int -> a -> a -> a
primFloatCompare :: a -> a -> Int
primFloatTest :: Int -> Int -> a -> Bool
primFloatMantissa :: Int -> a -> Integer
primFloatExponent :: Int -> a -> Int
primFloatEncode :: Int -> Integer -> Int -> a
primFloatTruncate :: a -> Integer
primIntegerBits :: Int -> Integer -> Integer -> Integer
primIntegerShift :: Integer -> Int -> Integer
primIntegerPopCount :: Integer -> Int
primIntegerShow :: Integer -> Text

-- The world outside a program. A file that is open is known by a number, and
-- 0, 1 and 2 are what the program reads, writes and complains to.
primArgCount :: ST s Int
primArg :: Int -> ST s Text
primGetEnv :: Text -> ST s Text
primSetArgs :: Text -> ST s ()
primProgName :: ST s Text
primSetProgName :: Text -> ST s ()
primSetEnv :: Text -> Text -> ST s ()
primUnsetEnv :: Text -> ST s ()
primEnvironment :: ST s Text
primExecutablePath :: ST s Text
primExit :: Int -> ST s a
primOpen :: Text -> Int -> ST s Int
primClose :: Int -> ST s ()
primWrite :: Int -> Text -> ST s ()
primReadAll :: Int -> ST s Text
primReadLine :: Int -> ST s Text
primReadChar :: Int -> ST s Int
primPeekChar :: Int -> ST s Int
primIsEOF :: Int -> ST s Bool
primFileState :: Int -> ST s Int
primFileSize :: Int -> ST s Integer
primSetFileSize :: Int -> Integer -> ST s ()
primSeek :: Int -> Int -> Integer -> ST s ()
primTell :: Int -> ST s Integer
primFlush :: Int -> ST s ()
primLastFailure :: ST s Text
primFileName :: Int -> ST s Text
primRemoveFile :: Text -> ST s Int
primIsTerminal :: Int -> ST s Int
primListDirectory :: Text -> ST s Text
primCreateDirectory :: Text -> ST s ()
primPathKind :: Text -> ST s Int
primCallProcess :: Text -> Text -> ST s Int
primClock :: ST s Int
primOS :: Text

-- A value thrown past everything that is waiting for a result, to whichever
-- catch was begun last and is not over.
primThrow :: a -> b
primCatch :: ST s a -> (e -> ST s a) -> ST s a
