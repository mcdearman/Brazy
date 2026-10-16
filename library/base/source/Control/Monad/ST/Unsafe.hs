module Control.Monad.ST.Unsafe where

import Control.Monad.ST (ST)
import Hazy (primUnsafeCoerce)
import Hazy.Prelude (IO (..), runST)

-- An action that is not run until its result is wanted.
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST action = pure (runST (primUnsafeCoerce action))

unsafeDupableInterleaveST :: ST s a -> ST s a
unsafeDupableInterleaveST = unsafeInterleaveST

unsafeIOToST :: IO a -> ST s a
unsafeIOToST (IO action) = primUnsafeCoerce action

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO action = IO (primUnsafeCoerce action)
