module ContextNotGiven where

class Marked a where
  mark :: a -> ()

class Other a where
  other :: a -> ()

data Some where
  Some :: (Marked a) => a -> Some

out :: Some -> ()
out (Some a) = other a
