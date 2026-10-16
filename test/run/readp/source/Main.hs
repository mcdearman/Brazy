module Main where

import Data.Char (isDigit, isSpace)
import Data.Ratio ((%))
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (lexP, parens, readEither)
import qualified Text.Read.Lex as Lex

-- Parsers of ReadP, ReadPrec and the lexer, as GHC's base has them.

number :: ReadP Int
number = read <$> munch1 isDigit

expression :: ReadP Int
expression = chainl1 term (((+) <$ char '+') +++ ((-) <$ char '-'))
  where
    term = chainl1 factor ((*) <$ char '*')
    factor = number +++ between (char '(') (char ')') expression

main :: IO ()
main = do
  print (readP_to_S expression "1+2*3-(4-1)*2")
  print (readP_to_S (many (char 'a')) "aaa")
  print (readP_to_S (munch (== 'a')) "aab", readP_to_S (string "ab" <++ string "a") "abc")
  print (readP_to_S (sepBy number (char ',') <* eof) "1,22,333")
  print (readP_to_S (option 0 number) "x", readP_to_S (count 2 get) "xyz", readP_to_S look "rest")
  print (readP_to_S (gather (skipSpaces >> number)) "  42!")
  print (readP_to_S (manyTill get (string "--")) "abc--def")
  print (readP_to_S (endBy number (char ';')) "1;2;", readP_to_S (chainr1 number ((^) <$ char '^')) "2^3^2")
  print (readP_to_S (readS_to_P (reads :: ReadS Int)) "17 tail", readP_to_S (choice [pfail, number]) "5")
  print (readP_to_S (skipMany1 (satisfy isSpace) >> many1 get) "  ab")
  -- The lexer.
  print (readP_to_S Lex.lex " foo 'x' \"s\\n\" 0x1F 3.5e2 ( -> :: +++ `")
  print (readP_to_S (manyTill Lex.lex (Lex.expect Lex.EOF)) "f (x, y) = [1..n] -- c")
  print (readP_to_S Lex.hsLex "  12.5e-1 rest")
  print (fmap (\(l, s) -> (l, s)) (readP_to_S (Lex.expect (Lex.Ident "let")) "let x"))
  print (readP_to_S (Lex.readHexP :: ReadP Int) "ff", readP_to_S (Lex.readOctP :: ReadP Int) "17", readP_to_S (Lex.readDecP :: ReadP Integer) "99")
  -- ReadPrec.
  print (ReadPrec.readPrec_to_S (parens lexP) 0 "((ident))")
  print (ReadPrec.readPrec_to_S (ReadPrec.prec 5 (ReadPrec.lift number)) 3 "8", ReadPrec.readPrec_to_S (ReadPrec.prec 5 (ReadPrec.lift number)) 7 "8")
  print (ReadPrec.readPrec_to_S (ReadPrec.step ReadPrec.get) 0 "q", ReadPrec.readPrec_to_S (ReadPrec.choice [ReadPrec.pfail, ReadPrec.look]) 0 "all")
  print (readEither "12" :: Either String Int, readEither "x" :: Either String Int)
  -- % is infixl 7, under ^.
  print (2 ^ 3 % 5 :: Rational, Lex.numberToRational <$> [n | (Lex.Number n, _) <- readP_to_S Lex.lex "1.25e1"])
