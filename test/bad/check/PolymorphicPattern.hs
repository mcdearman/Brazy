module PolymorphicPattern where

data Pair = Pair (forall a. (a, a))

wrong :: Pair -> ()
wrong (Pair (_, _)) = ()
