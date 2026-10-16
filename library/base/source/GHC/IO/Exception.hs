-- |
-- The representation of what goes wrong with input and output, as GHC
-- gives it out.
module GHC.IO.Exception
  ( IOException (..),
    IOErrorType (..),
    IOError,
    ioError,
    ioException,
    userError,
  )
where

import Hazy.PreludeIO (IOErrorType (..), IOException (..))

ioException :: IOException -> IO a
ioException = ioError
