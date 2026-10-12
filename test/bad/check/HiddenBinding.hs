module HiddenBinding where

data Some where
  Some :: a -> (a -> ()) -> Some

out :: Some -> ()
out some = f a
  where
    Some a f = some
