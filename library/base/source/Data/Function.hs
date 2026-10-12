module Data.Function
  ( id,
    const,
    (.),
    flip,
    ($),
    (&),
    fix,
    on,
  )
where

infixl 1 &

(&) :: a -> (a -> b) -> b
a & f = f a

fix :: (a -> a) -> a
fix f = let a = f a in a

infixl 0 `on`

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on combine f a b = combine (f a) (f b)
