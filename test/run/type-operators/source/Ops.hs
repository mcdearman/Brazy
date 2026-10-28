{-# LANGUAGE TypeOperators #-}
module Ops (type (+), (:+:) (..), (:*:) (..), type (~>) (..), Swap) where

-- Type operators of each kind, declared infix and in brackets, with what
-- binds them.

infixr 5 :+:

data a :+: b = L a | R b
  deriving (Show)

infixr 6 :*:

data (:*:) a b = a :*: b
  deriving (Show)

-- A synonym named by a variable's symbol, which binds as any type does.
type a + b = Either a b

-- A newtype named by a symbol with a tilde in it.
newtype f ~> g = Natural (f Int -> g Int)

-- A synonym written with type operators.
type Swap a b = b :+: a
