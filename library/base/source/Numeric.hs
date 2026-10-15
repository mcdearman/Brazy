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
import Hazy.Prelude (placeholder)
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
showHFloat = placeholder

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

