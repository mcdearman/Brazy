module Hazy.PreludeNumeric where

import Hazy.Prelude

showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned f d x
  | x >= 0 = f x
  | otherwise = showParen (d > 6) $ showString "-" . f (-x)

showInt :: (Integral a) => a -> ShowS
showInt 0 = showString "0"
showInt x
  | x < 0 = error "Numeric: showInt doesn't work with negative numbers"
  | otherwise = \string -> reverse (digits x) ++ string
  where
    digits 0 = []
    digits x = digit (x `rem` 10) : digits (x `quot` 10)

    digit = \case
      0 -> '0'
      1 -> '1'
      2 -> '2'
      3 -> '3'
      4 -> '4'
      5 -> '5'
      6 -> '6'
      7 -> '7'
      8 -> '8'
      9 -> '9'
      _ -> error "Numeric.showInt: (`quot` 10) not in 0-9 range"

readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
  where
    -- As the Report has it. The number is a whole token, so the space
    -- before it is passed over, as it is before any token.
    read' r = read'' r ++ [(-x, t) | ("-", s) <- lex r, (x, t) <- read'' s]
    read'' r = [(n, s) | (str, s) <- lex r, (n, "") <- readPos str]

-- A whole number as a program has it, with its sign: in tens, or in
-- sixteens, eights or twos after @0x@, @0o@ or @0b@.
readWhole :: ReadS Integer
readWhole = readParen False signed
  where
    signed text = case dropWhile isSpace text of
      '-' : rest -> [(negate number, after) | (number, after) <- unsigned (dropWhile isSpace rest)]
      rest -> unsigned rest
    unsigned text = case text of
      '0' : letter : rest
        | letter == 'x' || letter == 'X', found@(_ : _) <- digits 16 rest -> found
        | letter == 'o' || letter == 'O', found@(_ : _) <- digits 8 rest -> found
        | letter == 'b' || letter == 'B', found@(_ : _) <- digits 2 rest -> found
      _ -> digits 10 text
    digits base text = case span (\c -> value c < base) text of
      ([], _) -> []
      (found, after) -> [(foldl (\number c -> number * base + value c) 0 found, after)]
    value c
      | c >= '0' && c <= '9' = toInteger (fromEnum c - fromEnum '0')
      | c >= 'a' && c <= 'f' = toInteger (fromEnum c - fromEnum 'a' + 10)
      | c >= 'A' && c <= 'F' = toInteger (fromEnum c - fromEnum 'A' + 10)
      | otherwise = 16

readDec :: (Eq a, Num a) => ReadS a
readDec string = do
  (digits, string) <- lexDigits string
  let number = \case
        (digit : digits) -> base + 10 * number digits
          where
            base = case digit of
              '0' -> 0
              '1' -> 1
              '2' -> 2
              '3' -> 3
              '4' -> 4
              '5' -> 5
              '6' -> 6
              '7' -> 7
              '8' -> 8
              '9' -> 9
        [] -> 0
  [(number (reverse digits), string)]

showFloat :: (RealFloat a) => a -> ShowS
showFloat = placeholder

readFloat :: (RealFrac a) => ReadS a
readFloat = placeholder

lexDigits :: ReadS String
lexDigits = \case
  (n : string) | n >= '0' && n <= '9' -> do
    let lexDigits = \case
          (n : string) | n >= '0' && n <= '9' -> do
            (ns, string) <- lexDigits string
            [(n : ns, string)]
          string -> [("", string)]
    (ns, string) <- lexDigits string
    [(n : ns, string)]
  _ -> []
