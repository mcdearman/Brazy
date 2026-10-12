module HiddenResult where

data Some where
  Some :: a -> Some

out :: Some -> ()
out (Some a) = a
