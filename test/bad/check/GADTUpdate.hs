module GADTUpdate where

data T a where
  C :: {field :: a} -> T a

change :: T () -> T (T ())
change c = c {field = c}
