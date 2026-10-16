module System.IO.Unsafe
  ( unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
    unsafeFixIO,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Hazy (primUnsafeCoerce)
import Hazy.Prelude (IO (..), runST)

-- An action run where its result is wanted, as if it were no action.
unsafePerformIO :: IO a -> a
unsafePerformIO (IO action) = runST (primUnsafeCoerce action)

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = unsafePerformIO

-- An action that is not run until its result is wanted.
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO action = pure (unsafePerformIO action)

unsafeFixIO :: (a -> IO a) -> IO a
unsafeFixIO action = do
  place <- newIORef (error "System.IO.Unsafe.unsafeFixIO: the result was needed before it was made")
  result <- unsafeInterleaveIO (readIORef place)
  made <- action result
  writeIORef place made
  pure made
