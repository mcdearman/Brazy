module HiddenEscape where

data Some where
  Some :: a -> Some

out (Some a) = a
