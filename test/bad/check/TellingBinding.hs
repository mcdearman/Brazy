module TellingBinding where

data Value a where
  Unit :: () -> Value ()

out :: Value a -> ()
out value = n
  where
    Unit n = value
