module Main where

import Data.Functor.Classes (Show1 (..), showsPrec1)
import Data.Functor.Identity (Identity (..))

-- A class may require something of its parameter given every type: what
-- can be asked to show what it holds can be shown, whenever what it holds
-- can. So a type variable that is only known to have that class is enough
-- to show something of its type.

data Tree loeb a = Tree a (loeb Int)

instance (Show1 loeb, Show a) => Show (Tree loeb a) where
  showsPrec d (Tree a held) =
    showParen (d > 10) (showString "Tree " . showsPrec 11 a . showString " " . showsPrec 11 held)

-- Inside two of them.
inside :: (Show1 loeb, Show1 solve, Show a) => loeb (solve a) -> String
inside held = show held ++ showsPrec1 11 held ""

-- An instance that a deriving clause asks for wants it too.
data Wrap loeb a = Wrap (loeb a) deriving (Show)

both :: (Show1 loeb) => Wrap loeb Int -> String
both = show

-- The class for a type of the program's own: the type has to be one that
-- can be shown when what it holds can.
data Two a = Two a a deriving (Show)

instance Show1 Two where
  liftShowsPrec showsValue _ d (Two a b) =
    showParen (d > 10) (showString "Two " . showsValue 11 a . showString " " . showsValue 11 b)

shown :: (Show1 f) => f Char -> String
shown = show

main :: IO ()
main = do
  print (Tree 'x' (Just 3), Tree True [4], Tree () (Two 5 6))
  putStrLn (inside (Just [True]) ++ inside [Just 'x'])
  putStrLn (both (Wrap (Just 3)) ++ both (Wrap [4]))
  putStrLn (shown (Two 'a' 'b') ++ show (runIdentity (Identity 'i')))
