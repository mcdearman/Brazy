-- |
-- What C says went wrong: the number in @errno@, and the 'IOError' of it.
-- The numbers here are those of Linux and of Windows, which are the same
-- for these; some are other numbers on macOS.
module Foreign.C.Error
  ( Errno (..),
    eOK,
    e2BIG,
    eACCES,
    eAGAIN,
    eBADF,
    eBUSY,
    eCHILD,
    eDOM,
    eEXIST,
    eFAULT,
    eFBIG,
    eINTR,
    eINVAL,
    eIO,
    eISDIR,
    eMFILE,
    eMLINK,
    eNFILE,
    eNODEV,
    eNOENT,
    eNOEXEC,
    eNOMEM,
    eNOSPC,
    eNOTDIR,
    eNOTTY,
    eNXIO,
    ePERM,
    ePIPE,
    eRANGE,
    eROFS,
    eSPIPE,
    eSRCH,
    eXDEV,
    isValidErrno,
    getErrno,
    resetErrno,
    errnoToIOError,
    throwErrno,
    throwErrnoIf,
    throwErrnoIf_,
    throwErrnoIfRetry,
    throwErrnoIfRetry_,
    throwErrnoIfMinus1,
    throwErrnoIfMinus1_,
    throwErrnoIfMinus1Retry,
    throwErrnoIfMinus1Retry_,
    throwErrnoIfNull,
    throwErrnoIfNullRetry,
    throwErrnoPath,
    throwErrnoPathIf,
    throwErrnoPathIf_,
    throwErrnoPathIfNull,
    throwErrnoPathIfMinus1,
    throwErrnoPathIfMinus1_,
  )
where

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import Hazy.PreludeIO (Handle, IOErrorType (..), IOException (..))
import System.IO.Unsafe (unsafePerformIO)

newtype Errno = Errno CInt

instance Eq Errno where
  errno@(Errno a) == errno'@(Errno b) = isValidErrno errno && isValidErrno errno' && a == b

foreign import ccall unsafe "hazy_errno" c_errno :: IO CInt

foreign import ccall unsafe "hazy_set_errno" c_setErrno :: CInt -> IO ()

foreign import ccall unsafe "strerror" c_strerror :: CInt -> IO CString

eOK, e2BIG, eACCES, eAGAIN, eBADF, eBUSY, eCHILD, eDOM, eEXIST, eFAULT, eFBIG, eINTR, eINVAL, eIO, eISDIR, eMFILE, eMLINK, eNFILE, eNODEV, eNOENT, eNOEXEC, eNOMEM, eNOSPC, eNOTDIR, eNOTTY, eNXIO, ePERM, ePIPE, eRANGE, eROFS, eSPIPE, eSRCH, eXDEV :: Errno
eOK = Errno 0
ePERM = Errno 1
eNOENT = Errno 2
eSRCH = Errno 3
eINTR = Errno 4
eIO = Errno 5
eNXIO = Errno 6
e2BIG = Errno 7
eNOEXEC = Errno 8
eBADF = Errno 9
eCHILD = Errno 10
eAGAIN = Errno 11
eNOMEM = Errno 12
eACCES = Errno 13
eFAULT = Errno 14
eBUSY = Errno 16
eEXIST = Errno 17
eXDEV = Errno 18
eNODEV = Errno 19
eNOTDIR = Errno 20
eISDIR = Errno 21
eINVAL = Errno 22
eNFILE = Errno 23
eMFILE = Errno 24
eNOTTY = Errno 25
eFBIG = Errno 27
eNOSPC = Errno 28
eSPIPE = Errno 29
eROFS = Errno 30
eMLINK = Errno 31
ePIPE = Errno 32
eDOM = Errno 33
eRANGE = Errno 34

-- | Whether the number is one C could have set.
isValidErrno :: Errno -> Bool
isValidErrno (Errno n) = n /= -1

getErrno :: IO Errno
getErrno = Errno <$> c_errno

resetErrno :: IO ()
resetErrno = c_setErrno 0

-- |
-- What went wrong, as an 'IOError': of the kind the number is of, and with
-- what C says the number means.
errnoToIOError :: String -> Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError location errno@(Errno n) handle file =
  IOError handle (kind errno) location description file
  where
    description = unsafePerformIO (c_strerror n >>= peekCString)
    kind number
      | number == eOK = OtherError
      | number `elem` [eEXIST] = AlreadyExists
      | number `elem` [eNOENT, eNOTDIR, eNXIO, eNODEV, eSRCH] = NoSuchThing
      | number `elem` [eBUSY] = ResourceBusy
      | number `elem` [eNOMEM, eNOSPC, eMFILE, eNFILE, eFBIG, eAGAIN] = ResourceExhausted
      | number `elem` [eACCES, ePERM, eROFS] = PermissionDenied
      | number `elem` [eINVAL, eDOM, eRANGE, e2BIG] = InvalidArgument
      | number `elem` [eISDIR, eNOTTY, eSPIPE] = InappropriateType
      | number `elem` [eIO, eFAULT, eINTR, eCHILD, eNOEXEC, eBADF, eXDEV, eMLINK] = SystemError
      | otherwise = OtherError

throwErrno :: String -> IO a
throwErrno location = do
  errno <- getErrno
  ioError (errnoToIOError location errno Nothing Nothing)

throwErrnoPath :: String -> FilePath -> IO a
throwErrnoPath location path = do
  errno <- getErrno
  ioError (errnoToIOError location errno Nothing (Just path))

throwErrnoIf :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIf failed location action = do
  result <- action
  if failed result then throwErrno location else pure result

throwErrnoIf_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ failed location action = () <$ throwErrnoIf failed location action

-- | Again, as long as what failed was interrupted.
throwErrnoIfRetry :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry failed location action = do
  result <- action
  if failed result
    then do
      errno <- getErrno
      if errno == eINTR then throwErrnoIfRetry failed location action else throwErrno location
    else pure result

throwErrnoIfRetry_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ failed location action = () <$ throwErrnoIfRetry failed location action

throwErrnoIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1 = throwErrnoIf (== -1)

throwErrnoIfMinus1_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1_ = throwErrnoIf_ (== -1)

throwErrnoIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1Retry = throwErrnoIfRetry (== -1)

throwErrnoIfMinus1Retry_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_ = throwErrnoIfRetry_ (== -1)

throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull = throwErrnoIf (== nullPtr)

throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry = throwErrnoIfRetry (== nullPtr)

throwErrnoPathIf :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIf failed location path action = do
  result <- action
  if failed result then throwErrnoPath location path else pure result

throwErrnoPathIf_ :: (a -> Bool) -> String -> FilePath -> IO a -> IO ()
throwErrnoPathIf_ failed location path action = () <$ throwErrnoPathIf failed location path action

throwErrnoPathIfNull :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull = throwErrnoPathIf (== nullPtr)

throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (== -1)

throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_ = throwErrnoPathIf_ (== -1)
