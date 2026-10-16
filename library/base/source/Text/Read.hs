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
import Text.ParserCombinators.ReadPrec (Prec, ReadPrec, choice, get, lift, look, minPrec, pfail, prec, readP_to_Prec, readPrec_to_P, readPrec_to_S, readS_to_Prec, reset, step, (+++), (<++))
import Text.Read.Lex (Lexeme (..))
import qualified Text.Read.Lex as Lex

-- | The next word.
lexP :: ReadPrec Lexeme
lexP = lift Lex.lex

-- | In any number of brackets, or none; what is in them is read at no
-- precedence.
parens :: ReadPrec a -> ReadPrec a
parens p = optional
  where
    optional = p +++ mandatory
    mandatory = do
      open <- lexP
      case open of
        Punc "(" -> pure ()
        _ -> pfail
      x <- reset optional
      close <- lexP
      case close of
        Punc ")" -> pure x
        _ -> pfail

-- The class here has no readPrec of its own, so a list is read as the
-- class reads one.
readListDefault :: (Read a) => ReadS [a]
readListDefault = readList

readListPrecDefault :: (Read a) => ReadPrec [a]
readListPrecDefault = readS_to_Prec (\_ -> readList)

readEither :: (Read a) => String -> Either String a
readEither text = case [value | (value, rest) <- reads text, all isSpace rest] of
  [value] -> Right value
  [] -> Left "Prelude.read: no parse"
  _ -> Left "Prelude.read: ambiguous parse"

readMaybe :: (Read a) => String -> Maybe a
readMaybe text = case readEither text of
  Right value -> Just value
  Left _ -> Nothing
