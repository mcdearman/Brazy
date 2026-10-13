module Main where

import Data.Kind (Type)

data Scope = Global | Scope :+ Scope

type One :: Scope -> Type
data One scope where
  Declaration :: !Int -> One (s ':+ scopes)
  Shift :: !(One scopes) -> One (s ':+ scopes)
  Top :: !Int -> !Int -> One 'Global

instance Eq (One scope) where
  Declaration a == Declaration b = a == b
  Shift a == Shift b = a == b
  Top a b == Top c d = a == c && b == d
  _ == _ = False

instance Ord (One scope) where
  Declaration a `compare` Declaration b = a `compare` b
  Declaration {} `compare` Shift {} = LT
  Shift {} `compare` Declaration {} = GT
  Shift a `compare` Shift b = a `compare` b
  Top a b `compare` Top c d = (a, b) `compare` (c, d)

type Two :: Scope -> Type
data Two scope
  = Index !(One scope)
  | Lifted !Int
  | Bool
  | Char
  | Arrow
  | Tuple !Int
  | Integer
  | Show
  | Every
  deriving (Show, Eq, Ord)

instance Show (One scope) where
  showsPrec _ _ = showString "one"

shift :: Two scope -> Two ('Global ':+ scope)
shift = \case
  Index one -> Index (Shift one)
  Lifted n -> Lifted n
  Bool -> Bool
  Char -> Char
  Arrow -> Arrow
  Tuple n -> Tuple n
  Integer -> Integer
  Show -> Show
  Every -> Every

samples :: [Two 'Global]
samples = [Index (Top 1 2), Index (Top 1 3), Lifted 4, Bool, Char, Arrow, Tuple 2, Tuple 3, Integer, Show, Every]

main :: IO ()
main = do
  print [a == b | a <- samples, b <- samples]
  print [compare a b | a <- samples, b <- samples]
  let shifted = map shift samples
      visiting = [(shift Show, shift (Index (Top 1 2))), (shift Every, shift (Tuple 2))]
  print [(a, b) `elem` visiting | a <- [shift Show, shift Every], b <- shifted]
  print (map (`elem` shifted) shifted, (Show, Index (Top 1 2)) == (Show, Index (Top 1 2)))
  print (maximum samples, minimum samples, Index (Top 0 0) < Lifted 0, Tuple 9 < Integer)
