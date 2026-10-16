module Data.Tuple
  ( Solo (..),
    fst,
    snd,
    curry,
    uncurry,
    swap,
  )
where

data Solo a = Solo a

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
