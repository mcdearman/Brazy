module Data.Functor
  ( Functor (..),
    ($>),
    (<$>),
    (<&>),
    void,
  )
where

infixl 4 $>

($>) :: (Functor f) => f a -> b -> f b
x $> y = fmap (const y) x

infixl 1 <&>

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
x <&> f = fmap f x

void :: (Functor f) => f a -> f ()
void = fmap (const ())
