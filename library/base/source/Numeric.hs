module Numeric
  ( showSigned,
    showIntAtBase,
    showInt,
    showBin,
    showHex,
    showOct,
    showEFloat,
    showFFloat,
    showGFloat,
    showFFloatAlt,
    showGFloatAlt,
    showFloat,
    showHFloat,
    floatToDigits,
    readSigned,
    readInt,
    readBin,
    readDec,
    readOct,
    readHex,
    readFloat,
    lexDigits,
    fromRat,
    Floating,
  )
where

import Data.Char (digitToInt, intToDigit, isHexDigit, isOctDigit)
import Hazy.PreludeNumeric
  ( floatToDigits,
    fromRat,
    lexDigits,
    readDec,
    readFloat,
    readSigned,
    showEFloat,
    showFFloat,
    showFFloatAlt,
    showFloat,
    showGFloat,
    showGFloatAlt,
    showSigned,
  )

showIntAtBase :: (Integral a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base digit number rest
  | number < 0 = error "Numeric.showIntAtBase: negative number"
  | otherwise = go (quotRem number base) rest
  where
    go (n, d) text =
      let text' = digit (fromIntegral d) : text
       in text' `seq` if n == 0 then text' else go (quotRem n base) text'

showInt :: (Integral a) => a -> ShowS
showInt = showIntAtBase 10 intToDigit

showBin :: (Integral a) => a -> ShowS
showBin = showIntAtBase 2 intToDigit

showHex :: (Integral a) => a -> ShowS
showHex = showIntAtBase 16 intToDigit

showOct :: (Integral a) => a -> ShowS
showOct = showIntAtBase 8 intToDigit

showHFloat :: (RealFloat a) => a -> ShowS
-- | A number as hexadecimal: its first binary digit, the rest four to a
-- hexadecimal digit, and its power of two, as GHC shows it.
showHFloat = showString . shown
  where
    shown x
      | isNaN x = "NaN"
      | isInfinite x = (if x < 0 then "-" else "") ++ "Infinity"
      | x < 0 || isNegativeZero x = '-' : positive (negate x)
      | otherwise = positive x
    -- The binary digits of the number, from decodeFloat, are those of
    -- floatToDigits in a radix of two, which has only tens here.
    positive x
      | x == 0 = "0x0p+0"
      | otherwise = case decodeFloat x of
          (m, e) -> case reverse (dropWhile (== 0) (reverse (binary m))) of
            d : ds -> "0x" ++ show d ++ fraction ds ++ "p" ++ show (e + length (binary m) - 1)
            [] -> errorWithoutStackTrace "showHFloat: no digits"
    binary :: Integer -> [Int]
    binary 0 = []
    binary n = binary (n `div` 2) ++ [fromInteger (n `mod` 2)]
    fraction digits
      | all (== 0) digits = ""
      | otherwise = '.' : hexadecimal digits
    hexadecimal = \case
      [] -> ""
      [a] -> digit a 0 0 0 ""
      [a, b] -> digit a b 0 0 ""
      [a, b, c] -> digit a b c 0 ""
      a : b : c : d : rest -> digit a b c d (hexadecimal rest)
    digit a b c d = showHex (8 * a + 4 * b + 2 * c + d)

readInt :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDigit digit text =
  [ (foldl1 (\n d -> n * radix + d) (map (fromIntegral . digit) ds), rest)
  | (ds@(_ : _), rest) <- [span isDigit text]
  ]

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

readOct :: (Eq a, Num a) => ReadS a
readOct = readInt 8 isOctDigit digitToInt

readHex :: (Eq a, Num a) => ReadS a
readHex = readInt 16 isHexDigit digitToInt

