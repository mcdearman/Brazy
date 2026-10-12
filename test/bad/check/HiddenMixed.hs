module HiddenMixed where

data Some where
  Some :: a -> (a -> ()) -> Some

mix :: Some -> Some -> ()
mix (Some a _) (Some _ g) = g a
