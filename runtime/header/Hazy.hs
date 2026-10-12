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

-- The world outside a program. A file that is open is known by a number, and
-- 0, 1 and 2 are what the program reads, writes and complains to.
primArgCount :: ST s Int
primArg :: Int -> ST s Text
primGetEnv :: Text -> ST s Text
primExecutablePath :: ST s Text
primExit :: Int -> ST s a
primOpen :: Text -> Int -> ST s Int
primClose :: Int -> ST s ()
primWrite :: Int -> Text -> ST s ()
primReadAll :: Int -> ST s Text
primReadLine :: Int -> ST s Text
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
