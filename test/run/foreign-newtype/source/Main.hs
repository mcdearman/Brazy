module Main where

import Data.Int (Int64)

-- A newtype of a type C is given is given as what it holds, through any
-- number of newtypes, and whatever they are given.

newtype Celsius = Celsius Double
  deriving (Show)

newtype Wrap a = Wrap a

newtype Twice = Twice (Wrap Int64)

foreign import ccall "sqrt" root :: Celsius -> Celsius

foreign import ccall "llabs" absolute :: Wrap Int64 -> Wrap Int64

foreign import ccall "llabs" absoluteTwice :: Twice -> IO Twice

main :: IO ()
main = do
  print (root (Celsius 16))
  let Wrap n = absolute (Wrap (-7))
  print n
  Twice (Wrap m) <- absoluteTwice (Twice (Wrap (-9)))
  print m
