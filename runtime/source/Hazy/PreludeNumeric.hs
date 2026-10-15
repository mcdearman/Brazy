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

-- Floating point as text, as the Haskell report has it and as GHC does it,
-- so that a number is shown the same by both. ------------------------------

-- How a number is written: with an exponent, without one, or whichever of
-- the two GHC would choose.
data FFFormat = FFExponent | FFFixed | FFGeneric

showFloat :: (RealFloat a) => a -> ShowS
showFloat x = showString (formatRealFloat FFGeneric Nothing x)

showEFloat, showFFloat, showGFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showEFloat decimals x = showString (formatRealFloat FFExponent decimals x)
showFFloat decimals x = showString (formatRealFloat FFFixed decimals x)
showGFloat decimals x = showString (formatRealFloat FFGeneric decimals x)

showFFloatAlt, showGFloatAlt :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloatAlt decimals x = showString (formatRealFloatAlt FFFixed decimals True x)
showGFloatAlt decimals x = showString (formatRealFloatAlt FFGeneric decimals True x)

formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat format decimals x = formatRealFloatAlt format decimals False x

-- The alternative form always has a point.
formatRealFloatAlt :: (RealFloat a) => FFFormat -> Maybe Int -> Bool -> a -> String
formatRealFloatAlt format decimals alternate x
  | isNaN x = "NaN"
  | isInfinite x = if x < 0 then "-Infinity" else "Infinity"
  | x < 0 || isNegativeZero x = '-' : written format (floatToDigits 10 (negate x))
  | otherwise = written format (floatToDigits 10 x)
  where
    written format (is, e) =
      let ds = map digitOf is
       in case format of
            FFGeneric -> written (if e < 0 || e > 7 then FFExponent else FFFixed) (is, e)
            FFExponent -> case decimals of
              Nothing ->
                let shownE = show (e - 1)
                 in case ds of
                      "0" -> "0.0e0"
                      [d] -> d : ".0e" ++ shownE
                      d : ds' -> d : '.' : ds' ++ "e" ++ shownE
                      [] -> error "formatRealFloat: no digits"
              Just dec
                | dec <= 0 -> case is of
                    [0] -> "0e0"
                    _ ->
                      let (ei, is') = roundTo 10 1 is
                       in case map digitOf (if ei > 0 then init is' else is') of
                            n : _ -> n : 'e' : show (e - 1 + ei)
                            [] -> error "formatRealFloat: no digits"
                | otherwise ->
                    let dec' = max dec 1
                     in case is of
                          [0] -> '0' : '.' : take dec' (repeat '0') ++ "e0"
                          _ ->
                            let (ei, is') = roundTo 10 (dec' + 1) is
                             in case map digitOf (if ei > 0 then init is' else is') of
                                  d : ds' -> d : '.' : ds' ++ 'e' : show (e - 1 + ei)
                                  [] -> error "formatRealFloat: no digits"
            FFFixed ->
              let mk0 ls = if null ls then "0" else ls
               in case decimals of
                    Nothing
                      | e <= 0 -> "0." ++ replicate (negate e) '0' ++ ds
                      | otherwise ->
                          let f n s rs
                                | n == 0 = mk0 (reverse s) ++ '.' : mk0 rs
                                | otherwise = case rs of
                                    "" -> f (n - 1) ('0' : s) ""
                                    r : rs' -> f (n - 1) (r : s) rs'
                           in f e "" ds
                    Just dec ->
                      let dec' = max dec 0
                       in if e >= 0
                            then
                              let (ei, is') = roundTo 10 (dec' + e) is
                                  (ls, rs) = splitAt (e + ei) (map digitOf is')
                               in mk0 ls ++ (if null rs && not alternate then "" else '.' : rs)
                            else
                              let (ei, is') = roundTo 10 dec' (replicate (negate e) 0 ++ is)
                               in case map digitOf (if ei > 0 then is' else 0 : is') of
                                    d : ds' -> d : (if null ds' && not alternate then "" else '.' : ds')
                                    [] -> error "formatRealFloat: no digits"

-- A digit as its character.
digitOf :: Int -> Char
digitOf digit = toEnum (fromEnum '0' + digit)

-- Some digits rounded to so many, ties to even, and whether that carried
-- one more digit in front.
roundTo :: Int -> Int -> [Int] -> (Int, [Int])
roundTo base d is =
  case f d True is of
    x@(0, _) -> x
    (1, xs) -> (1, 1 : xs)
    _ -> error "roundTo: bad value"
  where
    b2 = base `quot` 2
    f n _ [] = (0, replicate n 0)
    f 0 e (x : xs)
      | x == b2 && e && all (== 0) xs = (0, [])
      | otherwise = (if x >= b2 then 1 else 0, [])
    f n _ (i : xs)
      | i' == base = (1, 0 : is')
      | otherwise = (0, i' : is')
      where
        (c, is') = f (n - 1) (even i) xs
        i' = c + i

-- The fewest digits that are read as the number again, and where the
-- point goes: the number is @0.d1d2...@ times the base to that power. This
-- is the algorithm of Burger and Dybvig, as the report and GHC have it.
floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
floatToDigits _ 0 = ([0], 0)
floatToDigits base x =
  let b = floatRadix x
      p = floatDigits x
      (minExp0, _) = floatRange x
      minExp = minExp0 - p
      (f0, e0) = decodeFloat x
      (f, e) =
        let n = minExp - e0
         in if n > 0 then (f0 `quot` expt b n, e0 + n) else (f0, e0)
      (r, s, mUp, mDn) =
        if e >= 0
          then
            let be = expt b e
             in if f == expt b (p - 1)
                  then (f * be * b * 2, 2 * b, be * b, be)
                  else (f * be * 2, 2, be, be)
          else
            if e > minExp && f == expt b (p - 1)
              then (f * b * 2, expt b (negate e + 1) * 2, b, 1)
              else (f * 2, expt b (negate e) * 2, 1, 1)
      k :: Int
      k =
        let k0 :: Int
            k0
              | b == 2 && base == 10 =
                  let lx = p - 1 + e0
                      k1 = (lx * 8651) `quot` 28738
                   in if lx >= 0 then k1 + 1 else k1
              | otherwise = error "floatToDigits: only in tens, of a radix of two"
            fixup n
              | n >= 0 = if r + mUp <= expt base n * s then n else fixup (n + 1)
              | otherwise = if expt base (negate n) * (r + mUp) <= s then n else fixup (n + 1)
         in fixup k0
      gen ds rn sN mUpN mDnN =
        let (dn, rn') = (rn * base) `quotRem` sN
            mUpN' = mUpN * base
            mDnN' = mDnN * base
         in case (rn' < mDnN', rn' + mUpN' > sN) of
              (True, False) -> dn : ds
              (False, True) -> dn + 1 : ds
              (True, True) -> if rn' * 2 < sN then dn : ds else dn + 1 : ds
              (False, False) -> gen (dn : ds) rn' sN mUpN' mDnN'
      rds
        | k >= 0 = gen [] r (s * expt base k) mUp mDn
        | otherwise =
            let bk = expt base (negate k)
             in gen [] (r * bk) s (mUp * bk) (mDn * bk)
   in (map fromIntegral (reverse rds), k)

expt :: Integer -> Int -> Integer
expt base n = base ^ n

-- A number from its text, as the report has it, and @NaN@ and @Infinity@
-- as GHC has them.
readFloat :: (RealFrac a) => ReadS a
readFloat r =
  [(fromRational ((n % 1) * 10 ^^ (k - d)), t) | (n, d, s) <- readFix r, (k, t) <- readExp s]
    ++ [(0 / 0, t) | ("NaN", t) <- lex r]
    ++ [(1 / 0, t) | ("Infinity", t) <- lex r]
  where
    readFix r = [(read (ds ++ ds'), length ds', t) | (ds, d) <- lexDigits r, (ds', t) <- lexFrac d]
    lexFrac ('.' : ds) = lexDigits ds
    lexFrac s = [("", s)]
    readExp (e : s) | e == 'e' || e == 'E' = readExp' s
    readExp s = [(0, s)]
    readExp' ('-' : s) = [(negate k, t) | (k, t) <- readDec s]
    readExp' ('+' : s) = readDec s
    readExp' s = readDec s

-- A fraction as the nearest number of floating point, ties to even.
fromRat :: (RealFloat a) => Rational -> a
fromRat x
  | x == 0 = encodeFloat 0 0
  | x < 0 = negate (positive (negate x))
  | otherwise = positive x
  where
    -- The quotient is worked out to a few bits more than the precision,
    -- with one more bit that says whether anything was left over: then
    -- 'encodeFloat' rounds it as it would the whole of it.
    positive x = result
      where
        n = numerator x
        d = denominator x
        shift = floatDigits result + 4 - (bitLength n - bitLength d)
        (q, r)
          | shift >= 0 = (n * 2 ^ shift) `quotRem` d
          | otherwise = n `quotRem` (d * 2 ^ negate shift)
        result = encodeFloat (2 * q + (if r == 0 then 0 else 1)) (negate shift - 1)

bitLength :: Integer -> Int
bitLength = go 0
  where
    go count 0 = count
    go count m = go (count + 1) (m `quot` 2)
