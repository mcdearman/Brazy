module Text.Show
  ( ShowS,
    Show (..),
    shows,
    showChar,
    showString,
    showParen,
    showListWith,
  )
where

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _ [] s = "[]" ++ s
showListWith showx (x : xs) s = '[' : showx x (showl xs)
  where
    showl [] = ']' : s
    showl (y : ys) = ',' : showx y (showl ys)
