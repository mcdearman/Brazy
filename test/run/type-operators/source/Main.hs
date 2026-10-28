{-# LANGUAGE TypeOperators #-}
module Main where

import Ops (type (+), (:+:) (..), (:*:) (..), type (~>) (..), Swap)

swap :: a :+: b -> b :+: a
swap (L a) = R a
swap (R b) = L b

toEither :: a :+: b -> a + b
toEither (L a) = Left a
toEither (R b) = Right b

-- It groups to the right: Int :+: (Bool :+: Char).
nested :: [Int :+: Bool :+: Char]
nested = [L 1, R (L True), R (R 'c')]

-- :*: binds more tightly than :+:.
mixed :: Int :*: Bool :+: Char
mixed = L (1 :*: True)

-- In brackets, as any type constructor.
prefix :: (:+:) Int Bool
prefix = R False

apply :: Maybe ~> [] -> [Int]
apply (Natural f) = f (Just 3)

swapped :: Swap Int Bool
swapped = L True

main :: IO ()
main = do
  print (swap (L 'x' :: Char :+: Int), toEither (R 2 :: Bool :+: Int))
  print (nested, mixed, prefix)
  print (apply (Natural (maybe [] (\x -> [x, x]))), swapped)
