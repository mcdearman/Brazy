module ContextDerive where

class Marked a

data Some where
  Some :: (Marked a) => a -> Some

deriving instance Eq Some
