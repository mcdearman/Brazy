module DeriveContext where

data Wrap f a = Wrap (f a) deriving (Eq)

data Opaque a = Opaque

wrong :: Bool
wrong = (Wrap Opaque :: Wrap Opaque ()) == Wrap Opaque
