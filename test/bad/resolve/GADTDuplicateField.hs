module GADTDuplicateField where

data T where
  C :: {x :: (), x :: ()} -> T
