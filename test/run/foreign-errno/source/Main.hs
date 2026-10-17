module Main where

import Control.Exception (catch)
import Foreign.C.Error (Errno (..), eEXIST, eNOENT, eOK, errnoToIOError, getErrno, isValidErrno, resetErrno, throwErrnoIfMinus1, throwErrnoIfMinus1_, throwErrnoIfNull)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Error (ioeGetFileName, ioeGetLocation, isAlreadyExistsError, isDoesNotExistError)

-- What C says went wrong: errno, and the IOError of a number.

main :: IO ()
main = do
  resetErrno
  errno <- getErrno
  print (errno == eOK, isValidErrno errno, eNOENT == eNOENT, eNOENT == eEXIST, isValidErrno (Errno (-1)))
  result <- throwErrnoIfMinus1 "never" (pure (5 :: CInt))
  print result
  throwErrnoIfMinus1_ "failing" (pure (-1 :: CInt)) `catch` \problem -> print (ioeGetLocation (problem :: IOError))
  (() <$ throwErrnoIfNull "null" (pure (nullPtr :: Ptr ()))) `catch` \problem -> print (ioeGetLocation (problem :: IOError))
  let missing = errnoToIOError "open" eNOENT Nothing (Just "missing.txt")
      existing = errnoToIOError "create" eEXIST Nothing Nothing
  print (isDoesNotExistError missing, ioeGetFileName missing, ioeGetLocation missing, isAlreadyExistsError existing)
