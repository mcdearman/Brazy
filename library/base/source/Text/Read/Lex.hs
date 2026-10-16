-- |
-- The words of Haskell as GHC's base reads them.
module Text.Read.Lex
  ( Lexeme (..),
    Number,
    numberToInteger,
    numberToFixed,
    numberToRational,
    numberToRangedRational,
    lex,
    expect,
    hsLex,
    lexChar,
    readBinP,
    readIntP,
    readOctP,
    readDecP,
    readHexP,
    isSymbolChar,
  )
where

import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit, isHexDigit, isOctDigit, isPunctuation, isSymbol, lexLitChar, readLitChar)
import Data.Ratio ((%))
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Prelude hiding (lex)

data Lexeme
  = -- | A character in quotes.
    Char Char
  | -- | Characters in double quotes.
    String String
  | -- | Punctuation, and reserved symbols such as @=@ and @->@.
    Punc String
  | -- | A name, or a reserved word.
    Ident String
  | -- | A symbol.
    Symbol String
  | Number Number
  | EOF
  deriving (Eq, Show)

-- |
-- A number as it was written: in a base, by its digits; or in tens, with
-- what is after the point and the exponent, if it has them.
data Number
  = MkNumber Int [Int]
  | MkDecimal [Int] (Maybe [Int]) (Maybe Integer)
  deriving (Eq, Show)

value :: Int -> [Int] -> Integer
value base = foldl (\n d -> n * toInteger base + toInteger d) 0

numberToInteger :: Number -> Maybe Integer
numberToInteger = \case
  MkNumber base digits -> Just (value base digits)
  MkDecimal digits Nothing Nothing -> Just (value 10 digits)
  _ -> Nothing

numberToRational :: Number -> Rational
numberToRational = \case
  MkNumber base digits -> value base digits % 1
  MkDecimal whole fraction exponent ->
    let part = case fraction of
          Nothing -> value 10 whole % 1
          Just digits -> value 10 (whole ++ digits) % (10 ^ length digits)
     in case exponent of
          Nothing -> part
          Just e
            | e >= 0 -> part * (10 ^ e % 1)
            | otherwise -> part * (1 % (10 ^ negate e))

-- | The number, unless its exponent is far past the range given.
numberToRangedRational :: (Int, Int) -> Number -> Maybe Rational
numberToRangedRational (low, high) number@(MkDecimal _ _ (Just e))
  | e > toInteger high + 3 = Nothing
  | e < toInteger low - 3 - 400 = Just 0
  | otherwise = Just (numberToRational number)
numberToRangedRational _ number = Just (numberToRational number)

-- | The number as a whole part and a fraction of so many places, if it is
-- one.
numberToFixed :: Integer -> Number -> Maybe (Integer, Integer)
numberToFixed _ (MkNumber base digits) = Just (value base digits, 0)
numberToFixed _ (MkDecimal whole Nothing Nothing) = Just (value 10 whole, 0)
numberToFixed _ (MkDecimal whole (Just fraction) Nothing) = Just (value 10 whole, value 10 fraction)
numberToFixed _ _ = Nothing

isSymbolChar :: Char -> Bool
isSymbolChar c = not (isPuncChar c) && case c of
  _ | c `elem` "!#$%&*+./<=>?@\\^|-~:" -> True
    | c < '\x80' -> False
    | otherwise -> isSymbol c || isPunctuation c

isPuncChar :: Char -> Bool
isPuncChar c = c `elem` ",;()[]{}`"

-- | The next word, after any space.
lex :: ReadP Lexeme
lex = ReadP.skipSpaces >> lexToken

-- | The next word, as it was written.
hsLex :: ReadP String
hsLex = do
  ReadP.skipSpaces
  (written, _) <- ReadP.gather lexToken
  return written

expect :: Lexeme -> ReadP ()
expect wanted = do
  found <- lex
  if found == wanted then return () else ReadP.pfail

lexToken :: ReadP Lexeme
lexToken =
  ReadP.choice
    [ do
        s <- ReadP.look
        if null s then return EOF else ReadP.pfail,
      lexLiteral,
      Punc . (: []) <$> ReadP.satisfy isPuncChar,
      lexSymbol,
      lexIdent,
      Number <$> lexNumber
    ]

-- A character or a string in quotes, as the Prelude reads them.
lexLiteral :: ReadP Lexeme
lexLiteral = do
  s <- ReadP.look
  case s of
    '\'' : _ -> Char <$> ReadP.readS_to_P (reads :: ReadS Char)
    '"' : _ -> String <$> ReadP.readS_to_P (reads :: ReadS String)
    _ -> ReadP.pfail

-- | One character of a literal, as it is written in one.
lexChar :: ReadP Char
lexChar = do
  s <- ReadP.look
  case s of
    '\\' : _ -> do
      written <- ReadP.readS_to_P lexLitChar
      case readLitChar written of
        [(c, "")] -> return c
        _ -> ReadP.pfail
    _ -> ReadP.get

lexSymbol :: ReadP Lexeme
lexSymbol = do
  s <- ReadP.munch1 isSymbolChar
  return $
    if s `elem` reserved
      then Punc s
      else Symbol s
  where
    reserved = ["..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

lexIdent :: ReadP Lexeme
lexIdent = do
  c <- ReadP.satisfy (\c -> isAlpha c || c == '_')
  s <- ReadP.munch (\c -> isAlphaNum c || c == '_' || c == '\'')
  return (Ident (c : s))

lexNumber :: ReadP Number
lexNumber = based ReadP.<++ decimal
  where
    based = do
      _ <- ReadP.char '0'
      (base, isDigit') <-
        ReadP.choice
          [ (16, isHexDigit) <$ (ReadP.char 'x' ReadP.+++ ReadP.char 'X'),
            (8, isOctDigit) <$ (ReadP.char 'o' ReadP.+++ ReadP.char 'O'),
            (2, (`elem` "01")) <$ (ReadP.char 'b' ReadP.+++ ReadP.char 'B')
          ]
      digits <- ReadP.munch1 isDigit'
      return (MkNumber base (map digitToInt digits))
    decimal = do
      whole <- ReadP.munch1 isDigit
      -- As much of a number as can be read is: no part is left for after.
      fraction <- greedy $ do
        _ <- ReadP.char '.'
        map digitToInt <$> ReadP.munch1 isDigit
      exponent <- greedy $ do
        _ <- ReadP.char 'e' ReadP.+++ ReadP.char 'E'
        sign <- ((negate <$ ReadP.char '-') ReadP.+++ (id <$ ReadP.char '+')) ReadP.<++ return id
        digits <- ReadP.munch1 isDigit
        return (sign (read digits))
      return (MkDecimal (map digitToInt whole) fraction exponent)
    greedy p = (Just <$> p) ReadP.<++ return Nothing

readIntP :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
readIntP base isDigit' digit = do
  digits <- ReadP.munch1 isDigit'
  return (foldl (\n c -> n * base + fromIntegral (digit c)) 0 digits)

readBinP, readOctP, readDecP, readHexP :: (Eq a, Num a) => ReadP a
readBinP = readIntP 2 (`elem` "01") digitToInt
readOctP = readIntP 8 isOctDigit digitToInt
readDecP = readIntP 10 isDigit digitToInt
readHexP = readIntP 16 isHexDigit digitToInt
