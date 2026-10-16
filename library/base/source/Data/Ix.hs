-- |
-- Types whose values can number the places of an array, as the Haskell
-- 2010 report has them. The class is built in, so that it can be derived.
module Data.Ix
  ( Ix (range, index, inRange, rangeSize),
  )
where

import Hazy.Prelude (Ix (..))
