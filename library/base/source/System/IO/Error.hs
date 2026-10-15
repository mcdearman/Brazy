-- |
-- What goes wrong with input and output, as the Haskell 2010 report and
-- GHC have it.
module System.IO.Error
  ( IOError,
    userError,
    mkIOError,
    annotateIOError,
    isAlreadyExistsError,
    isDoesNotExistError,
    isAlreadyInUseError,
    isFullError,
    isEOFError,
    isIllegalOperation,
    isPermissionError,
    isUserError,
    isResourceVanishedError,
    ioeGetErrorType,
    ioeGetLocation,
    ioeGetErrorString,
    ioeGetHandle,
    ioeGetFileName,
    ioeSetErrorType,
    ioeSetErrorString,
    ioeSetLocation,
    ioeSetHandle,
    ioeSetFileName,
    IOErrorType,
    alreadyExistsErrorType,
    doesNotExistErrorType,
    alreadyInUseErrorType,
    fullErrorType,
    eofErrorType,
    illegalOperationErrorType,
    permissionErrorType,
    userErrorType,
    resourceVanishedErrorType,
    isAlreadyExistsErrorType,
    isDoesNotExistErrorType,
    isAlreadyInUseErrorType,
    isFullErrorType,
    isEOFErrorType,
    isIllegalOperationErrorType,
    isPermissionErrorType,
    isUserErrorType,
    isResourceVanishedErrorType,
    ioError,
    catchIOError,
    tryIOError,
    modifyIOError,
  )
where

import Hazy.PreludeIO
  ( Handle,
    IOErrorType (..),
    IOException (..),
    catch,
  )

mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IOError
mkIOError kind location handle file =
  IOError
    { ioe_handle = handle,
      ioe_type = kind,
      ioe_location = location,
      ioe_description = "",
      ioe_filename = file
    }

-- Where it went wrong, and of what, if that was not known.
annotateIOError :: IOError -> String -> Maybe Handle -> Maybe FilePath -> IOError
annotateIOError problem location handle file =
  problem
    { ioe_handle = orElse handle (ioe_handle problem),
      ioe_location = location,
      ioe_filename = orElse file (ioe_filename problem)
    }
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y

isAlreadyExistsError,
  isDoesNotExistError,
  isAlreadyInUseError,
  isFullError,
  isEOFError,
  isIllegalOperation,
  isPermissionError,
  isUserError,
  isResourceVanishedError ::
    IOError -> Bool
isAlreadyExistsError = isAlreadyExistsErrorType . ioeGetErrorType
isDoesNotExistError = isDoesNotExistErrorType . ioeGetErrorType
isAlreadyInUseError = isAlreadyInUseErrorType . ioeGetErrorType
isFullError = isFullErrorType . ioeGetErrorType
isEOFError = isEOFErrorType . ioeGetErrorType
isIllegalOperation = isIllegalOperationErrorType . ioeGetErrorType
isPermissionError = isPermissionErrorType . ioeGetErrorType
isUserError = isUserErrorType . ioeGetErrorType
isResourceVanishedError = isResourceVanishedErrorType . ioeGetErrorType

ioeGetErrorType :: IOError -> IOErrorType
ioeGetErrorType = ioe_type

ioeGetLocation :: IOError -> String
ioeGetLocation = ioe_location

-- What a user's error says, or else what kind of error it is.
ioeGetErrorString :: IOError -> String
ioeGetErrorString problem
  | isUserErrorType (ioe_type problem) = ioe_description problem
  | otherwise = show (ioe_type problem)

ioeGetHandle :: IOError -> Maybe Handle
ioeGetHandle = ioe_handle

ioeGetFileName :: IOError -> Maybe FilePath
ioeGetFileName = ioe_filename

ioeSetErrorType :: IOError -> IOErrorType -> IOError
ioeSetErrorType problem kind = problem {ioe_type = kind}

ioeSetErrorString :: IOError -> String -> IOError
ioeSetErrorString problem description = problem {ioe_description = description}

ioeSetLocation :: IOError -> String -> IOError
ioeSetLocation problem location = problem {ioe_location = location}

ioeSetHandle :: IOError -> Handle -> IOError
ioeSetHandle problem handle = problem {ioe_handle = Just handle}

ioeSetFileName :: IOError -> FilePath -> IOError
ioeSetFileName problem file = problem {ioe_filename = Just file}

alreadyExistsErrorType,
  doesNotExistErrorType,
  alreadyInUseErrorType,
  fullErrorType,
  eofErrorType,
  illegalOperationErrorType,
  permissionErrorType,
  userErrorType,
  resourceVanishedErrorType ::
    IOErrorType
alreadyExistsErrorType = AlreadyExists
doesNotExistErrorType = NoSuchThing
alreadyInUseErrorType = ResourceBusy
fullErrorType = ResourceExhausted
eofErrorType = EOF
illegalOperationErrorType = IllegalOperation
permissionErrorType = PermissionDenied
userErrorType = UserError
resourceVanishedErrorType = ResourceVanished

isAlreadyExistsErrorType,
  isDoesNotExistErrorType,
  isAlreadyInUseErrorType,
  isFullErrorType,
  isEOFErrorType,
  isIllegalOperationErrorType,
  isPermissionErrorType,
  isUserErrorType,
  isResourceVanishedErrorType ::
    IOErrorType -> Bool
isAlreadyExistsErrorType = (== AlreadyExists)
isDoesNotExistErrorType = (== NoSuchThing)
isAlreadyInUseErrorType = (== ResourceBusy)
isFullErrorType = (== ResourceExhausted)
isEOFErrorType = (== EOF)
isIllegalOperationErrorType = (== IllegalOperation)
isPermissionErrorType = (== PermissionDenied)
isUserErrorType = (== UserError)
isResourceVanishedErrorType = (== ResourceVanished)

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch

tryIOError :: IO a -> IO (Either IOError a)
tryIOError action = catch (fmap Right action) (pure . Left)

-- An error that goes past is changed on its way.
modifyIOError :: (IOError -> IOError) -> IO a -> IO a
modifyIOError change action = catch action (ioError . change)
