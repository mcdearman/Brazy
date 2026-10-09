-- |
-- How the prompt reads an entry as it is typed: whether Enter submits it or
-- carries it on, and how far in the next line starts. This follows Meadow's
-- and Powder's prompts.
--
-- Nothing here knows the grammar. Whether some text parses is asked of the
-- compiler, and the rest is decided from a rough reading of the text into
-- words, operators and brackets.
module Repl.Entry
  ( needsContinuation,
    autoIndent,
    lineBreak,
    wordStart,
    tokens,
    names,
  )
where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, nub)

-- |
-- One level of indentation.
unit :: Int
unit = 2

-- |
-- Whether an entry should keep taking lines and not be submitted yet.
--
-- * A @:@ command is always finished.
-- * On the first line, submit as soon as it parses, so that @1 + 2@ takes a
--   single Enter.
-- * Once the entry has several lines, a blank line ends it whatever it
--   holds. Short of that it is submitted early if it parses and nothing is
--   left dangling: no open bracket, and no operator or keyword at the end
--   that wants more.
-- * A type signature parses by itself but is half a declaration, so an entry
--   that opens with one runs until the blank line, to take every equation
--   under it. So does an entry whose last line is a guard.
needsContinuation :: (String -> Bool) -> String -> Bool
needsContinuation parses input
  | ":" `isPrefixOf` dropWhile isSpace input = False
  | multiline && all isSpace (last rows) = False
  | any opensSignature (take 1 coded ++ take 1 (reverse coded)) = True
  | not multiline = not (parses input)
  | take 1 (last' coded) == ["|"] = True
  | depth (tokens input) > 0 || dangling (tokens input) = True
  | otherwise = not (parses input)
  where
    rows = splitLines input
    multiline = length rows > 1
    -- The lines that hold code, each as its tokens.
    coded = filter (not . null) (map tokens rows)
    last' list = if null list then [] else last list

-- |
-- Whether a line is a signature with nothing defined yet: names, and then
-- their type. Anything else before a @::@ is an expression given a type.
opensSignature :: [String] -> Bool
opensSignature line = named line && "=" `notElem` line
  where
    named = \case
      "(" : _ : ")" : rest -> more rest
      name : rest | identifier name -> more rest
      _ -> False
    more = \case
      "," : rest -> named rest
      "::" : _ -> True
      _ -> False

identifier :: String -> Bool
identifier = \case
  first : _ -> isWordCharacter first
  [] -> False

splitLines :: String -> [String]
splitLines text = case break (== '\n') text of
  (line, _ : rest) -> line : splitLines rest
  (line, []) -> [line]

isWordCharacter :: Char -> Bool
isWordCharacter character = isAlphaNum character || character == '_' || character == '\''

isOperatorCharacter :: Char -> Bool
isOperatorCharacter = (`elem` "!#$%&*+./<=>?@\\^|-~:")

-- |
-- A rough reading of some text: words, runs of operator characters, string
-- and character literals, and single brackets and separators. Comments are
-- left out.
tokens :: String -> [String]
tokens = \case
  [] -> []
  '{' : '-' : rest -> tokens (block (1 :: Int) rest)
  '"' : rest ->
    let (literal, after) = string '"' rest
     in ('"' : literal) : tokens after
  '\'' : '\\' : character : '\'' : rest -> ['\'', '\\', character, '\''] : tokens rest
  '\'' : character : '\'' : rest -> ['\'', character, '\''] : tokens rest
  text@(first : rest)
    | isSpace first -> tokens rest
    | isWordCharacter first ->
        let (word, after) = span isWordCharacter text
         in word : tokens after
    | isOperatorCharacter first ->
        let (operator, after) = span isOperatorCharacter text
         in if all (== '-') operator && length operator >= 2
              then tokens (dropWhile (/= '\n') after)
              else operator : tokens after
    | otherwise -> [first] : tokens rest
  where
    -- The rest of a string literal, and what is after it.
    string quote = \case
      '\\' : escaped : rest ->
        let (literal, after) = string quote rest
         in ('\\' : escaped : literal, after)
      character : rest
        | character == quote -> ([character], rest)
        | otherwise ->
            let (literal, after) = string quote rest
             in (character : literal, after)
      [] -> ([], [])
    -- What is after a block comment, which may hold others.
    block level = \case
      '-' : '}' : rest
        | level == 1 -> rest
        | otherwise -> block (level - 1) rest
      '{' : '-' : rest -> block (level + 1) rest
      _ : rest -> block level rest
      [] -> []

-- |
-- How many brackets are still open.
depth :: [String] -> Int
depth list = count ["(", "[", "{"] - count [")", "]", "}"]
  where
    count brackets = length (filter (`elem` brackets) list)

-- |
-- Whether the last token of a line wants something after it.
dangling :: [String] -> Bool
dangling line = case reverse line of
  [] -> False
  token : _ ->
    all isOperatorCharacter token
      || token == ","
      || token `elem` ["let", "in", "if", "then", "else", "case", "of", "do", "where"]

-- |
-- The last line of some text that holds code.
above :: String -> String
above before = case filter (not . all isSpace) (splitLines before) of
  [] -> ""
  rows -> last rows

-- |
-- The whitespace a continuation line opens with, given what is before the
-- cursor: the indent of the line above, and a level more if that line opened
-- something.
autoIndent :: String -> String
autoIndent before = replicate (base + if opens then unit else 0) ' '
  where
    line = above before
    base = length (takeWhile (== ' ') line)
    opens = depth (tokens line) > 0 || dangling (tokens line)

-- |
-- What a line break key puts at the cursor. Enter breaks the line only while
-- the entry is unfinished and otherwise gives 'Nothing', to submit it. The
-- keys that ask for a line break by name always get one.
--
-- A break asked for by name after a whole line carries the entry on: after
-- the head of a definition, for the guards that stand a level in under it,
-- and after an equation, for the next one at the margin.
lineBreak :: (String -> Bool) -> Bool -> String -> Int -> Maybe String
lineBreak parses always line position
  | not always && not (needsContinuation parses line) = Nothing
  | always && null indent && not (all isSpace before) && "=" `notElem` tokens (above before) =
      Just ('\n' : replicate unit ' ')
  | otherwise = Just ('\n' : indent)
  where
    before = take position line
    indent = autoIndent before

-- |
-- Where the word that ends at a position starts.
wordStart :: String -> Int -> Int
wordStart line position =
  position - length (takeWhile isWordCharacter (reverse (take position line)))

-- |
-- The names some declarations define, as far as the text shows: the word
-- each unindented line starts with.
names :: String -> [String]
names text = nub $ do
  row@(first : _) <- splitLines text
  if isSpace first
    then []
    else case tokens row of
      keyword : name : _
        | keyword `elem` ["data", "newtype", "type", "class"], identifier name -> [name]
      keyword : _
        | keyword `elem` ["import", "instance", "infix", "infixl", "infixr", "deriving", "default"] -> []
      name : _ | identifier name -> [name]
      _ -> []
