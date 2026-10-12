module Data.Functor.Classes where

import Data.Functor.Identity (Identity (..))

class Show1 f where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
  liftShowList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
  liftShowList showsValue showValues = \case
    [] -> showString "[]"
    first : rest ->
      let item = liftShowsPrec showsValue showValues 0
          others = foldr (\next after -> showChar ',' . item next . after) (showChar ']') rest
       in showChar '[' . item first . others

showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = liftShowsPrec showsPrec showList

instance Show1 Identity where
  liftShowsPrec showsValue _ d (Identity a) =
    showParen (d > 10) (showString "Identity " . showsValue 11 a)

instance Show1 Maybe where
  liftShowsPrec showsValue _ d = \case
    Nothing -> showString "Nothing"
    Just a -> showParen (d > 10) (showString "Just " . showsValue 11 a)

instance Show1 [] where
  liftShowsPrec _ showValues _ = showValues
