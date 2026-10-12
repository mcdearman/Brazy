module Control.Monad.Fix (MonadFix (..), fix) where

import Control.Monad.ST (ST)
import Data.Function (fix)
import Data.Functor.Identity (Identity (..))

class (Monad m) => MonadFix m where
  mfix :: (a -> m a) -> m a

instance MonadFix Identity where
  mfix f = Identity (fix (runIdentity . f))

instance MonadFix Maybe where
  mfix f =
    let result = f (unJust result)
        unJust (Just a) = a
        unJust Nothing = error "mfix: Nothing"
     in result

-- These two need the runtime's help, and do not have it yet.
instance MonadFix IO

instance MonadFix (ST s)
