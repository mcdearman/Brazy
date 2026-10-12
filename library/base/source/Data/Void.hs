module Data.Void
  ( Void,
    absurd,
    vacuous,
  )
where


data Void

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  showsPrec _ = absurd

absurd :: Void -> a
absurd = \case {}

vacuous :: (Functor f) => f Void -> f a
vacuous = fmap absurd
