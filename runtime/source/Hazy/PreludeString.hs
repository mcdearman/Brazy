module Hazy.PreludeString where

import Hazy
import Hazy.Prelude

data Text

instance Eq Text where
  a == b = primTextEqual a b

instance Ord Text where
  compare a b = compare (primTextCompare a b) 0

instance Semigroup Text where
  a <> b = primTextAppend a b

instance Monoid Text where
  mempty = pack ""

instance Read Text where
  readsPrec d string = do
    (literal, string) <- readsPrec d string
    [(pack literal, string)]

instance Show Text where
  showsPrec d = showsPrec d . unpack
