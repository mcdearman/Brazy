module Main (main) where

-- Ix derived from the class, whatever the module has in scope: the Prelude
-- and Data.Ix only qualified, and names of the class's own defined here.

import qualified Data.Array as A
import qualified Data.Ix as Ix
import Prelude ()
import qualified Prelude as P

data Colour = Red | Green | Blue
  deriving (P.Eq, P.Ord, P.Show, Ix.Ix)

data Point = Point P.Int P.Bool
  deriving (P.Eq, P.Ord, P.Show, Ix.Ix)

data Cell = Cell {row :: P.Int, column :: P.Char, layer :: Colour}
  deriving (P.Eq, P.Ord, P.Show, Ix.Ix)

newtype Wrapped = Wrapped P.Integer
  deriving (P.Eq, P.Ord, P.Show, Ix.Ix)

data Unit = Unit
  deriving (P.Eq, P.Ord, P.Show, Ix.Ix)

range :: P.Int
range = 1

index :: P.Int
index = 2

inRange :: P.Int
inRange = 3

main :: P.IO ()
main = do
  P.print (Ix.range (Red, Blue), Ix.index (Green, Blue) Blue, Ix.inRange (Green, Blue) Red, Ix.rangeSize (Red, Blue))
  P.print (Ix.range (Point 0 P.False, Point 1 P.True), Ix.index (Point 0 P.False, Point 2 P.True) (Point 1 P.True))
  P.print (Ix.range (Cell 1 'a' Red, Cell 1 'b' Green), Ix.index (Cell 0 'a' Red, Cell 2 'c' Blue) (Cell 1 'b' Green))
  P.print (Ix.inRange (Cell 0 'a' Red, Cell 2 'c' Blue) (Cell 1 'd' Red), Ix.rangeSize (Cell 0 'a' Red, Cell 2 'c' Blue))
  P.print (Ix.range (Wrapped 3, Wrapped 5), Ix.index (Wrapped 10, Wrapped 20) (Wrapped 15), Ix.range (Unit, Unit))
  let board = A.listArray (Red, Blue) "rgb"
  P.print (board A.! Green, A.bounds board)
  P.print (range P.+ index P.+ inRange)
