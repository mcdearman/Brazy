module Text.Read
  ( Read (..),
    ReadS,
    reads,
    read,
    readParen,
    lex,
    module Text.ParserCombinators.ReadPrec,
    Lexeme (..),
    lexP,
    parens,
    readListDefault,
    readListPrecDefault,
    readEither,
    readMaybe,
  )
where

import Data.Char (isSpace)
import Hazy.Prelude (placeholder)
import Text.ParserCombinators.ReadPrec (ReadPrec)
import Text.Read.Lex (Lexeme (..))

lexP :: ReadPrec Lexeme
lexP = placeholder

parens :: ReadPrec a -> ReadPrec a
parens = placeholder

readListDefault :: (Read a) => ReadS [a]
readListDefault = placeholder

readListPrecDefault :: (Read a) => ReadPrec [a]
readListPrecDefault = placeholder

readEither :: (Read a) => String -> Either String a
readEither text = case [value | (value, rest) <- reads text, all isSpace rest] of
  [value] -> Right value
  [] -> Left "Prelude.read: no parse"
  _ -> Left "Prelude.read: ambiguous parse"

readMaybe :: (Read a) => String -> Maybe a
readMaybe text = case readEither text of
  Right value -> Just value
  Left _ -> Nothing
