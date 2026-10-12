module Data.Void
  ( Void,
    absurd,
    vacuous,
  )
where

import Hazy.Prelude (placeholder)

data Void

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  showsPrec _ = absurd

absurd :: Void -> a
absurd = placeholder

vacuous :: (Functor f) => f Void -> f a
vacuous = placeholder
