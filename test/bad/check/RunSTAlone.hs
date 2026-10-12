module RunSTAlone where

import Control.Monad.ST (ST, runST)

apply :: (a -> a) -> a -> a
apply f = f

wrong :: ()
wrong = runST `apply` pure ()
