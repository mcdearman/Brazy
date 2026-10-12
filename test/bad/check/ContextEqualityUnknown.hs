module ContextEqualityUnknown where

data Same a b where
  Same :: (a ~ b) => Same a b

cast :: Same a b -> a -> b
cast _ x = x
