module ForeignInteger where

-- An Integer is not passed to C.
foreign import ccall "labs" big :: Integer -> Integer
