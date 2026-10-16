module Control.Monad.ST
  ( ST,
    runST,
    fixST,
    RealWorld,
    stToIO,
  )
where

import Hazy (primNewRef, primReadRef, primUnsafeCoerce, primWriteRef)
import Hazy.Prelude (IO (..), RealWorld, ST, runST)

-- The result is given to the action that makes it, to be looked at only
-- once it is made.
fixST :: (a -> ST s a) -> ST s a
fixST action = do
  place <- primNewRef (error "Control.Monad.ST.fixST: the result was needed before it was made")
  let result = runST (primUnsafeCoerce (primReadRef place))
  made <- action result
  primWriteRef place made
  pure made

stToIO :: ST RealWorld a -> IO a
stToIO = IO
