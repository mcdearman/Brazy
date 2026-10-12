module PolymorphicThrough where

data Any = Any (forall a. a -> ())

twice :: (a -> a) -> a -> a
twice f x = f (f x)

wrong :: Any
wrong = Any `twice` (\_ -> ())
