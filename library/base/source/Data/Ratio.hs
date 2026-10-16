module Data.Ratio
  ( Rational,
    Ratio,
    (%),
    numerator,
    denominator,
    approxRational,
  )
where

import Hazy.Prelude
  ( Ratio,
    denominator,
    numerator,
    (%),
  )

-- The simplest fraction within the second of the first, as the report has
-- it.
approxRational :: (RealFrac a) => a -> a -> Rational
approxRational rat eps = simplest (toRational rat - toRational eps) (toRational rat + toRational eps)
  where
    simplest x y
      | y < x = simplest y x
      | x == y = x
      | x > 0 = simplest' n d n' d'
      | y < 0 = negate (simplest' (negate n') d' (negate n) d)
      | otherwise = 0 % 1
      where
        n = numerator x
        d = denominator x
        n' = numerator y
        d' = denominator y
    simplest' n d n' d'
      | r == 0 = q % 1
      | q /= q' = (q + 1) % 1
      | otherwise = (q * n'' + d'') % n''
      where
        (q, r) = quotRem n d
        (q', r') = quotRem n' d'
        nd'' = simplest' d' r' d r
        n'' = numerator nd''
        d'' = denominator nd''
