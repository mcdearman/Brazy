module HiddenEscapeOuter where

data Some where
  Some :: a -> Some

same :: a -> a -> ()
same _ _ = ()

out :: Some -> ()
out some = go ()
  where
    go x = case some of Some a -> same x a
