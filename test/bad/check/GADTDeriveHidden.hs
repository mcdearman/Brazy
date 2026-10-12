module GADTDeriveHidden where

data Some where
  Some :: a -> Some
  deriving (Show)
