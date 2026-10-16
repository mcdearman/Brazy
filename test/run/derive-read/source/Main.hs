module Main (main) where

-- Read derived from the class, whatever the module has in scope: the
-- Prelude only qualified, as in Freddy's example, and names of its own that
-- a written-out instance would have used.

import Prelude ()
import qualified Prelude as P

data MyType a = MyType a
  deriving (P.Read, P.Show, P.Functor)

data Shape = Circle P.Double | Rect {width :: P.Int, height :: P.Int} | Dot
  deriving (P.Read, P.Show)

data V = P.Int :+ P.Int | Neg V
  deriving (P.Read, P.Show)

infixl 6 :+

data Op a = a `Then` a
  deriving (P.Read, P.Show)

data Pair a b = Pair a (P.Maybe b)
  deriving (P.Read, P.Show)

newtype Name = Name {unName :: P.String}
  deriving (P.Read, P.Show)

lex :: P.Int
lex = 0

readParen :: P.Int
readParen = 0

readsPrec :: P.Int
readsPrec = 0

main :: P.IO ()
main = do
  P.print (P.read "MyType 3" :: MyType P.Int, P.fmap (P.+ 1) (P.read " ( MyType (-2) ) " :: MyType P.Int))
  P.print (P.read "[Circle 1.5, Rect {width = 2, height = -3}, Dot, (Dot)]" :: [Shape])
  P.print (P.read "Rect { width = 4 , height = 5 }" :: Shape)
  P.print (P.reads "Rect {width = 1, height = 2} rest" :: [(Shape, P.String)])
  P.print (P.read "1 :+ 2" :: V, P.read "Neg (3 :+ (-4))" :: V, P.read "(Neg (Neg (0 :+ 0)))" :: V)
  P.print (P.read "'a' `Then` 'b'" :: Op P.Char)
  P.print (P.read "Pair \"x y\" (Just [1,2])" :: Pair P.String [P.Int], P.read "Name {unName = \"n\"}" :: Name)
  P.print (P.reads "Circle" :: [(Shape, P.String)], P.reads "Square 1" :: [(Shape, P.String)])
  P.print (lex P.+ readParen P.+ readsPrec)
