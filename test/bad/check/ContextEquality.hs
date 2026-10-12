module ContextEquality where

data Value a where
  Unit :: (a ~ ()) => Value a

wrong :: Value (Value ())
wrong = Unit
