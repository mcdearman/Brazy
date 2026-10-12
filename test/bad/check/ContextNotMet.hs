module ContextNotMet where

class Marked a

data Some where
  Some :: (Marked a) => a -> Some

some :: Some
some = Some ()
