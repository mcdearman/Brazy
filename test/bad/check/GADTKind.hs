module GADTKind where

import Data.Kind (Type)

data T :: Type where
  C :: a -> T a
