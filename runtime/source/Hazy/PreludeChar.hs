module Hazy.PreludeChar where

import Hazy
import Hazy.Helper
import Hazy.Prelude

data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned

deriving instance Enum GeneralCategory

instance Eq GeneralCategory where
  (==) = enumEqual

instance Ord GeneralCategory where
  compare = enumCompare

instance Bounded GeneralCategory where
  minBound = UppercaseLetter
  maxBound = NotAssigned

instance Read GeneralCategory where
  readsPrec _ = readParen False $ \string -> do
    (token, string) <- lex string
    case token of
      "UppercaseLetter" -> [(UppercaseLetter, string)]
      "LowercaseLetter" -> [(LowercaseLetter, string)]
      "TitlecaseLetter" -> [(TitlecaseLetter, string)]
      "ModifierLetter" -> [(ModifierLetter, string)]
      "OtherLetter" -> [(OtherLetter, string)]
      "NonSpacingMark" -> [(NonSpacingMark, string)]
      "SpacingCombiningMark" -> [(SpacingCombiningMark, string)]
      "EnclosingMark" -> [(EnclosingMark, string)]
      "DecimalNumber" -> [(DecimalNumber, string)]
      "LetterNumber" -> [(LetterNumber, string)]
      "OtherNumber" -> [(OtherNumber, string)]
      "ConnectorPunctuation" -> [(ConnectorPunctuation, string)]
      "DashPunctuation" -> [(DashPunctuation, string)]
      "OpenPunctuation" -> [(OpenPunctuation, string)]
      "ClosePunctuation" -> [(ClosePunctuation, string)]
      "InitialQuote" -> [(InitialQuote, string)]
      "FinalQuote" -> [(FinalQuote, string)]
      "OtherPunctuation" -> [(OtherPunctuation, string)]
      "MathSymbol" -> [(MathSymbol, string)]
      "CurrencySymbol" -> [(CurrencySymbol, string)]
      "ModifierSymbol" -> [(ModifierSymbol, string)]
      "OtherSymbol" -> [(OtherSymbol, string)]
      "Space" -> [(Space, string)]
      "LineSeparator" -> [(LineSeparator, string)]
      "ParagraphSeparator" -> [(ParagraphSeparator, string)]
      "Control" -> [(Control, string)]
      "Format" -> [(Format, string)]
      "Surrogate" -> [(Surrogate, string)]
      "PrivateUse" -> [(PrivateUse, string)]
      "NotAssigned" -> [(NotAssigned, string)]
      _ -> []

instance Show GeneralCategory where
  showsPrec _ = \case
    UppercaseLetter -> showString "UppercaseLetter"
    LowercaseLetter -> showString "LowercaseLetter"
    TitlecaseLetter -> showString "TitlecaseLetter"
    ModifierLetter -> showString "ModifierLetter"
    OtherLetter -> showString "OtherLetter"
    NonSpacingMark -> showString "NonSpacingMark"
    SpacingCombiningMark -> showString "SpacingCombiningMark"
    EnclosingMark -> showString "EnclosingMark"
    DecimalNumber -> showString "DecimalNumber"
    LetterNumber -> showString "LetterNumber"
    OtherNumber -> showString "OtherNumber"
    ConnectorPunctuation -> showString "ConnectorPunctuation"
    DashPunctuation -> showString "DashPunctuation"
    OpenPunctuation -> showString "OpenPunctuation"
    ClosePunctuation -> showString "ClosePunctuation"
    InitialQuote -> showString "InitialQuote"
    FinalQuote -> showString "FinalQuote"
    OtherPunctuation -> showString "OtherPunctuation"
    MathSymbol -> showString "MathSymbol"
    CurrencySymbol -> showString "CurrencySymbol"
    ModifierSymbol -> showString "ModifierSymbol"
    OtherSymbol -> showString "OtherSymbol"
    Space -> showString "Space"
    LineSeparator -> showString "LineSeparator"
    ParagraphSeparator -> showString "ParagraphSeparator"
    Control -> showString "Control"
    Format -> showString "Format"
    Surrogate -> showString "Surrogate"
    PrivateUse -> showString "PrivateUse"
    NotAssigned -> showString "NotAssigned"

isSpace :: Char -> Bool
isSpace = \case
  '\t' -> True
  '\n' -> True
  '\r' -> True
  '\f' -> True
  '\v' -> True
  char | Space <- generalCategory char -> True
  _ -> False

isAlpha :: Char -> Bool
isAlpha char = case generalCategory char of
  UppercaseLetter -> True
  LowercaseLetter -> True
  TitlecaseLetter -> True
  ModifierLetter -> True
  OtherLetter -> True
  _ -> False

isDigit :: Char -> Bool
isDigit char = char >= '0' && char <= '9'

isAlphaNum :: Char -> Bool
isAlphaNum char = case generalCategory char of
  UppercaseLetter -> True
  LowercaseLetter -> True
  TitlecaseLetter -> True
  ModifierLetter -> True
  OtherLetter -> True
  DecimalNumber -> True
  LetterNumber -> True
  OtherNumber -> True
  _ -> False

-- A character as it is written inside a literal, as the Haskell report has
-- it. What follows an escape is kept from being read as part of it.
showLitChar :: Char -> ShowS
showLitChar char
  | code > 127 = \rest -> '\\' : showInt code (protect rest)
  | code == 127 = showString "\\DEL"
  | code == 92 = showString "\\\\"
  | code >= 32 = \rest -> char : rest
  | code == 14 = \rest -> case rest of
      'H' : _ -> "\\SO\\&" ++ rest
      _ -> "\\SO" ++ rest
  | otherwise = \rest -> '\\' : (names !! code ++ rest)
  where
    code = fromEnum char
    protect rest = case rest of
      digit : _ | isDigit digit -> '\\' : '&' : rest
      _ -> rest
    names =
      [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "a", "b", "t", "n", "v", "f", "r", "SO", "SI",
        "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US"
      ]

-- The escapes that are names, each with the character it stands for. One
-- that begins another comes after it.
escapes :: [(String, Int)]
escapes =
  [ ("NUL", 0), ("SOH", 1), ("STX", 2), ("ETX", 3), ("EOT", 4), ("ENQ", 5), ("ACK", 6), ("a", 7), ("b", 8),
    ("t", 9), ("n", 10), ("v", 11), ("f", 12), ("r", 13), ("SO", 14), ("SI", 15), ("DLE", 16), ("DC1", 17),
    ("DC2", 18), ("DC3", 19), ("DC4", 20), ("NAK", 21), ("SYN", 22), ("ETB", 23), ("CAN", 24), ("EM", 25),
    ("SUB", 26), ("ESC", 27), ("FS", 28), ("GS", 29), ("RS", 30), ("US", 31), ("SP", 32), ("DEL", 127),
    ("\\", 92), ("'", 39), ("\"", 34)
  ]

-- The named escape some text begins with, after its backslash: the name,
-- the character and what follows.
escaped :: String -> [(String, Char, String)]
escaped string = case [(name, toEnum code, rest) | (name, code) <- escapes, Just rest <- [after name string]] of
  found : _ -> [found]
  [] -> []
  where
    after [] rest = Just rest
    after (a : as) (b : bs) | a == b = after as bs
    after _ _ = Nothing

readLitChar :: ReadS Char
readLitChar = \case
  '\\' : string -> case escaped string of
    (_, char, rest) : _ -> [(char, rest)]
    [] -> do
      (code, string) <- readDec string
      [(toEnum code, string)]
  c : string -> [(c, string)]
  [] -> []

lexLitChar :: ReadS String
lexLitChar = \case
  '\\' : string -> case escaped string of
    (name, _, rest) : _ -> [('\\' : name, rest)]
    [] -> do
      (digits, string) <- lexDigits string
      [('\\' : digits, string)]
  char : string -> [(char : [], string)]
  _ -> []
