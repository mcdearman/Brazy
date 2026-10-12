module HiddenEscapeCase where

data Some where
  Some :: a -> Some

out :: Some -> ()
out some = (\_ -> ()) (case some of Some a -> a)
