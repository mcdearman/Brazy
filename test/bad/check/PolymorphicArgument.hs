module PolymorphicArgument where

data Twice = Twice (forall a. a -> (a, a))

wrong :: Twice
wrong = Twice (\x -> ((), x))
