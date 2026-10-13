module System.IO.Unsafe
  ( unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
    unsafeFixIO,
  )
where

import Hazy (primUnsafeCoerce)
import Hazy.Prelude (IO (..), placeholder, runST)

-- An action run where its result is wanted, as if it were no action.
unsafePerformIO :: IO a -> a
unsafePerformIO (IO action) = runST (primUnsafeCoerce action)

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = unsafePerformIO

-- An action that is not run until its result is wanted.
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO action = pure (unsafePerformIO action)

unsafeFixIO :: (a -> IO a) -> IO a
unsafeFixIO = placeholder
