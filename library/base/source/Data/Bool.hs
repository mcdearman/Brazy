module Data.Bool
  ( Bool (..),
    (&&),
    (||),
    not,
    otherwise,
    bool,
  )
where

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t
