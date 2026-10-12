module PolymorphicRequires where

class Marked a where
  mark :: a -> ()

data Any = Any (forall a. a -> ())

wrong :: Any
wrong = Any mark
