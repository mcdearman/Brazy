-- |
-- Types whose values can number the places of an array, as the Haskell
-- 2010 report has them.
module Data.Ix
  ( Ix (range, index, inRange, rangeSize),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)

class (Ord a) => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int
  rangeSize bounds@(_, high)
    | inRange bounds high = index bounds high + 1
    | otherwise = 0

-- What GHC says of an index that is not in its range.
outOfRange :: (Show a) => String -> (a, a) -> a -> b
outOfRange name bounds i =
  errorWithoutStackTrace
    ( "Ix{"
        ++ name
        ++ "}.index: Index "
        ++ showsPrec 11 i ""
        ++ " out of range ("
        ++ showsPrec 11 bounds ""
        ++ ")"
    )

-- The instances of the types that are numbers or are counted.
countedRange :: (Enum a) => (a, a) -> [a]
countedRange (low, high) = [low .. high]

countedIndex :: (Show a, Ord a, Integral b) => String -> (a -> b) -> (a, a) -> a -> Int
countedIndex name number bounds@(low, high) i
  | low <= i && i <= high = fromIntegral (number i - number low)
  | otherwise = outOfRange name bounds i

countedInRange :: (Ord a) => (a, a) -> a -> Bool
countedInRange (low, high) i = low <= i && i <= high

instance Ix Char where
  range = countedRange
  index = countedIndex "Char" fromEnum
  inRange = countedInRange

instance Ix Int where
  range = countedRange
  index = countedIndex "Int" id
  inRange = countedInRange

instance Ix Integer where
  range = countedRange
  index = countedIndex "Integer" id
  inRange = countedInRange

instance Ix Bool where
  range = countedRange
  index = countedIndex "Bool" fromEnum
  inRange = countedInRange

instance Ix Ordering where
  range = countedRange
  index = countedIndex "Ordering" fromEnum
  inRange = countedInRange

instance Ix () where
  range _ = [()]
  index _ () = 0
  inRange _ () = True

instance Ix Word where
  range = countedRange
  index = countedIndex "Word" toInteger
  inRange = countedInRange

instance Ix Word8 where
  range = countedRange
  index = countedIndex "Word8" toInteger
  inRange = countedInRange

instance Ix Word16 where
  range = countedRange
  index = countedIndex "Word16" toInteger
  inRange = countedInRange

instance Ix Word32 where
  range = countedRange
  index = countedIndex "Word32" toInteger
  inRange = countedInRange

instance Ix Word64 where
  range = countedRange
  index = countedIndex "Word64" toInteger
  inRange = countedInRange

instance Ix Int8 where
  range = countedRange
  index = countedIndex "Int8" toInteger
  inRange = countedInRange

instance Ix Int16 where
  range = countedRange
  index = countedIndex "Int16" toInteger
  inRange = countedInRange

instance Ix Int32 where
  range = countedRange
  index = countedIndex "Int32" toInteger
  inRange = countedInRange

instance Ix Int64 where
  range = countedRange
  index = countedIndex "Int64" toInteger
  inRange = countedInRange

-- Tuples are numbered with the last part changing fastest.
instance (Ix a, Ix b) => Ix (a, b) where
  range ((l1, l2), (u1, u2)) = [(i1, i2) | i1 <- range (l1, u1), i2 <- range (l2, u2)]
  index bounds@((l1, l2), (u1, u2)) i@(i1, i2)
    | inRange bounds i = index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2
    | otherwise = errorWithoutStackTrace "Error in array index"
  inRange ((l1, l2), (u1, u2)) (i1, i2) = inRange (l1, u1) i1 && inRange (l2, u2) i2

instance (Ix a, Ix b, Ix c) => Ix (a, b, c) where
  range ((l1, l2, l3), (u1, u2, u3)) =
    [(i1, i2, i3) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3)]
  index bounds@((l1, l2, l3), (u1, u2, u3)) i@(i1, i2, i3)
    | inRange bounds i =
        (index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3)
          + index (l3, u3) i3
    | otherwise = errorWithoutStackTrace "Error in array index"
  inRange ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3

instance (Ix a, Ix b, Ix c, Ix d) => Ix (a, b, c, d) where
  range ((l1, l2, l3, l4), (u1, u2, u3, u4)) =
    [ (i1, i2, i3, i4)
    | i1 <- range (l1, u1),
      i2 <- range (l2, u2),
      i3 <- range (l3, u3),
      i4 <- range (l4, u4)
    ]
  index bounds@((l1, l2, l3, l4), (u1, u2, u3, u4)) i@(i1, i2, i3, i4)
    | inRange bounds i =
        ( (index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3)
            + index (l3, u3) i3
        )
          * rangeSize (l4, u4)
          + index (l4, u4) i4
    | otherwise = errorWithoutStackTrace "Error in array index"
  inRange ((l1, l2, l3, l4), (u1, u2, u3, u4)) (i1, i2, i3, i4) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4

instance (Ix a, Ix b, Ix c, Ix d, Ix e) => Ix (a, b, c, d, e) where
  range ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) =
    [ (i1, i2, i3, i4, i5)
    | i1 <- range (l1, u1),
      i2 <- range (l2, u2),
      i3 <- range (l3, u3),
      i4 <- range (l4, u4),
      i5 <- range (l5, u5)
    ]
  index bounds@((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) i@(i1, i2, i3, i4, i5)
    | inRange bounds i =
        ( ( (index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3)
              + index (l3, u3) i3
          )
            * rangeSize (l4, u4)
            + index (l4, u4) i4
        )
          * rangeSize (l5, u5)
          + index (l5, u5) i5
    | otherwise = errorWithoutStackTrace "Error in array index"
  inRange ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) (i1, i2, i3, i4, i5) =
    inRange (l1, u1) i1
      && inRange (l2, u2) i2
      && inRange (l3, u3) i3
      && inRange (l4, u4) i4
      && inRange (l5, u5) i5
