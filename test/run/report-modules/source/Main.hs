module Main where

import Control.Applicative (Const (..), ZipList (..), liftA2)
import Data.Array
import Data.Bits
import Data.Complex
import Data.Functor.Const (Const (..))
import Data.Ix
import Data.Word (Word8)

-- The library modules of the Haskell 2010 report that were missing.

data Colour = Red | Green | Blue
  deriving (Eq, Ord, Show, Ix)

data Point = Point Int Bool
  deriving (Eq, Ord, Show, Ix)

data Cell = Cell {row :: Int, column :: Char}
  deriving (Eq, Ord, Show, Ix)

main :: IO ()
main = do
  -- Data.Ix, and what is derived.
  print (range ((0, 'a'), (1, 'c')) :: [(Int, Char)])
  print (index ((0, 0), (2, 3)) ((1, 2) :: (Int, Int)) :: Int, inRange (1, 5) (6 :: Int), rangeSize ((1, 1), (3, 4) :: (Int, Int)))
  print (range (Red, Blue), index (Red, Blue) Blue, inRange (Green, Blue) Red, rangeSize (Green, Blue))
  print (range (Point 0 False, Point 1 True), index (Point 0 False, Point 2 True) (Point 1 True))
  print (range (Cell 1 'x', Cell 2 'y'), inRange (Cell 1 'a', Cell 3 'c') (Cell 2 'd'))
  print (range (LT, GT), range ((), ()), index (False, True) True, range ('x', 'z'))
  -- Data.Array.
  let squares = array (1, 5) [(i, i * i) | i <- [1 .. 5]] :: Array Int Int
  print squares
  print (squares ! 3, bounds squares, elems squares, indices squares)
  print (squares // [(2, 0), (4, 0)])
  print (accumArray (+) 0 (0, 3) [(0, 1), (2, 5), (0, 2), (3, 1)] :: Array Int Int)
  print (accum (*) squares [(1, 10), (5, 2)])
  let grid = listArray ((0, 0), (1, 2)) "abcdef" :: Array (Int, Int) Char
  print (grid ! (1, 0), assocs grid)
  print (ixmap (0, 2) (\i -> (1, i)) grid)
  print (fmap (* 2) squares, sum squares, maximum squares, length squares)
  print (squares == squares, compare squares (squares // [(1, 0)]))
  print (read "array (0,1) [(0,'p'),(1,'q')]" :: Array Int Char)
  print (listArray (Red, Blue) [10, 20, 30 :: Int] ! Green)
  -- A table worked out from itself, as arrays are for.
  let fibs = listArray (0, 30) [if i < 2 then toInteger i else fibs ! (i - 1) + fibs ! (i - 2) | i <- [0 .. 30]] :: Array Int Integer
  print (fibs ! 30)
  -- Data.Bits on Integer.
  print ((12 :: Integer) .&. 10, (12 :: Integer) .|. 10, xor (12 :: Integer) 10, complement (5 :: Integer))
  print (shiftL (1 :: Integer) 100, shiftR (2 ^ 100 :: Integer) 98, shiftR (-9 :: Integer) 1, shift (-1 :: Integer) 3)
  print (popCount (255 :: Integer), popCount (-8 :: Integer), testBit (5 :: Integer) 2, testBit (-1 :: Integer) 1000)
  print (setBit (0 :: Integer) 4, clearBit (31 :: Integer) 0, complementBit (5 :: Integer) 1, bit 3 :: Integer)
  print ((-12 :: Integer) .&. 7, (-12 :: Integer) .|. 3, xor (-1 :: Integer) 5, zeroBits :: Integer)
  -- On words and Bool.
  print (complement (0 :: Word8), shiftL (200 :: Word8) 1, rotateL (129 :: Word8) 1, rotateR (1 :: Word8) 1, popCount (255 :: Word8))
  print (finiteBitSize (0 :: Word8), countLeadingZeros (1 :: Word8), countTrailingZeros (8 :: Word8), isSigned (0 :: Word8))
  print (True .&. False, True .|. False, xor True True, complement False, popCount True)
  print (toIntegralSized (300 :: Integer) :: Maybe Word8, toIntegralSized (200 :: Integer) :: Maybe Word8)
  -- Data.Complex.
  let z = 3 :+ 4 :: Complex Double
  print (z, realPart z, imagPart z, magnitude z, conjugate z)
  print (z + (1 :+ 1), z * (0 :+ 1), z / (1 :+ 1), negate z, abs z)
  print (sqrt ((-4) :+ 0 :: Complex Double), realPart (exp (0 :+ pi) :: Complex Double))
  print (mkPolar 2 0 :: Complex Double, cis 0 :: Complex Double, phase ((-1) :+ 0 :: Complex Double))
  print (fst (polar ((-2) :+ 0 :: Complex Double)), fromIntegral (3 :: Int) :: Complex Double)
  print (read "1.5 :+ (-2.0)" :: Complex Double, showsPrec 7 z "")
  -- Control.Applicative's types, which Data.Functor.Const gives as well.
  print (getZipList (liftA2 (+) (ZipList [1, 2, 3]) (ZipList [10, 20 :: Int])), ZipList "ab", getZipList (pure 'x' <* ZipList "abc"))
  print (getConst (traverse (\x -> Const [x]) [1, 2, 3 :: Int]), Const 'c' :: Const Char Int, fmap (+ 1) (Const "k" :: Const String Int))
