-- |
-- A function from @f a@ to @g a@ for every @a@, wrapped so that it can be
-- passed as an argument, as "Data.Functor2" does.
module Data.NaturalTransformation where

newtype NaturalTransformation f g = Morph (forall a. f a -> g a)
