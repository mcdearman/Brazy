module Data.Char
  ( Char,
    isControl,
    isSpace,
    isLower,
    isUpper,
    isAlpha,
    isAlphaNum,
    isPrint,
    isDigit,
    isOctDigit,
    isHexDigit,
    isLetter,
    isMark,
    isNumber,
    isPunctuation,
    isSymbol,
    isSeparator,
    isAscii,
    isLatin1,
    isAsciiUpper,
    isAsciiLower,
    GeneralCategory (..),
    generalCategory,
    toUpper,
    toLower,
    toTitle,
    digitToInt,
    intToDigit,
    ord,
    chr,
    showLitChar,
    lexLitChar,
    readLitChar,
  )
where

import Hazy.Prelude
  ( Char,
    GeneralCategory (..),
    generalCategory,
    isAlpha,
    isAlphaNum,
    isDigit,
    isSpace,
    lexLitChar,
    placeholder,
    readLitChar,
    showLitChar,
  )

isControl :: Char -> Bool
isControl c = case generalCategory c of
  Control -> True
  _ -> False

isLower :: Char -> Bool
isLower c = case generalCategory c of
  LowercaseLetter -> True
  _ -> False

isUpper :: Char -> Bool
isUpper c = case generalCategory c of
  UppercaseLetter -> True
  TitlecaseLetter -> True
  _ -> False

isPrint :: Char -> Bool
isPrint c = case generalCategory c of
  LineSeparator -> False
  ParagraphSeparator -> False
  Control -> False
  Format -> False
  Surrogate -> False
  PrivateUse -> False
  NotAssigned -> False
  _ -> True

isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'

isLetter :: Char -> Bool
isLetter = isAlpha

isMark :: Char -> Bool
isMark c = case generalCategory c of
  NonSpacingMark -> True
  SpacingCombiningMark -> True
  EnclosingMark -> True
  _ -> False

isNumber :: Char -> Bool
isNumber c = case generalCategory c of
  DecimalNumber -> True
  LetterNumber -> True
  OtherNumber -> True
  _ -> False

isPunctuation :: Char -> Bool
isPunctuation c = case generalCategory c of
  ConnectorPunctuation -> True
  DashPunctuation -> True
  OpenPunctuation -> True
  ClosePunctuation -> True
  InitialQuote -> True
  FinalQuote -> True
  OtherPunctuation -> True
  _ -> False

isSymbol :: Char -> Bool
isSymbol c = case generalCategory c of
  MathSymbol -> True
  CurrencySymbol -> True
  ModifierSymbol -> True
  OtherSymbol -> True
  _ -> False

isSeparator :: Char -> Bool
isSeparator c = case generalCategory c of
  Space -> True
  LineSeparator -> True
  ParagraphSeparator -> True
  _ -> False

isAscii :: Char -> Bool
isAscii c = ord c < 128

isLatin1 :: Char -> Bool
isLatin1 c = ord c < 256

isAsciiUpper :: Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

isAsciiLower :: Char -> Bool
isAsciiLower c = c >= 'a' && c <= 'z'

toUpper :: Char -> Char
toUpper c
  | c >= 'a' && c <= 'z' = chr (ord c - 32)
  | code >= 224 && code <= 254 && code /= 247 = chr (code - 32)
  | otherwise = c
  where
    code = ord c

toLower :: Char -> Char
toLower c
  | c >= 'A' && c <= 'Z' = chr (ord c + 32)
  | code >= 192 && code <= 222 && code /= 215 = chr (code + 32)
  | otherwise = c
  where
    code = ord c

toTitle :: Char -> Char
toTitle = toUpper

digitToInt :: Char -> Int
digitToInt c
  | isDigit c = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise = error "Char.digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit n
  | n >= 0 && n <= 9 = chr (ord '0' + n)
  | n >= 10 && n <= 15 = chr (ord 'a' + n - 10)
  | otherwise = error "Char.intToDigit: not a digit"

ord :: Char -> Int
ord = fromEnum

chr :: Int -> Char
chr = toEnum
