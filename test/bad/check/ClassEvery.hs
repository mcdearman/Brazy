module ClassEvery where

import Data.Functor.Classes (Show1 (..))

data Hidden a = Hidden a

instance Show1 Hidden where
  liftShowsPrec _ _ _ _ = showString "hidden"
