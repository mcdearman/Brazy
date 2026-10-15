module Hazy.PreludeIO where

import Hazy
import Hazy.Prelude

type FilePath = String

-- A handle is the number the system knows an open file by.
newtype Handle = Handle Int
  deriving (Eq)

instance Show Handle where
  showsPrec _ (Handle number) = showString "<handle " . shows number . showString ">"

-- |
-- What went wrong with input or output, as GHC has it: what kind of thing,
-- where, in the system's words, and of what handle or file.
data IOException = IOError
  { ioe_handle :: Maybe Handle,
    ioe_type :: IOErrorType,
    ioe_location :: String,
    ioe_description :: String,
    ioe_filename :: Maybe FilePath
  }

type IOError = IOException

instance Eq IOException where
  IOError handle kind location description file == IOError handle' kind' location' description' file' =
    handle == handle'
      && kind == kind'
      && location == location'
      && description == description'
      && file == file'

-- The file, or else the handle; where; what kind; and the system's words.
instance Show IOException where
  showsPrec _ (IOError handle kind location description file) =
    ( case file of
        Just name -> showString name . showString ": "
        Nothing -> case handle of
          Just handle -> showsPrec 11 handle . showString ": "
          Nothing -> id
    )
      . ( case location of
            "" -> id
            _ -> showString location . showString ": "
        )
      . showsPrec 11 kind
      . ( case description of
            "" -> id
            _ -> showString " (" . showString description . showString ")"
        )

data IOErrorType
  = AlreadyExists
  | NoSuchThing
  | ResourceBusy
  | ResourceExhausted
  | EOF
  | IllegalOperation
  | PermissionDenied
  | UserError
  | UnsatisfiedConstraints
  | SystemError
  | ProtocolError
  | OtherError
  | InvalidArgument
  | InappropriateType
  | HardwareFault
  | UnsupportedOperation
  | TimeExpired
  | ResourceVanished
  | Interrupted

instance Eq IOErrorType where
  x == y = errorTypeNumber x == errorTypeNumber y

errorTypeNumber :: IOErrorType -> Int
errorTypeNumber = \case
  AlreadyExists -> 0
  NoSuchThing -> 1
  ResourceBusy -> 2
  ResourceExhausted -> 3
  EOF -> 4
  IllegalOperation -> 5
  PermissionDenied -> 6
  UserError -> 7
  UnsatisfiedConstraints -> 8
  SystemError -> 9
  ProtocolError -> 10
  OtherError -> 11
  InvalidArgument -> 12
  InappropriateType -> 13
  HardwareFault -> 14
  UnsupportedOperation -> 15
  TimeExpired -> 16
  ResourceVanished -> 17
  Interrupted -> 18

instance Show IOErrorType where
  showsPrec _ kind = showString $ case kind of
    AlreadyExists -> "already exists"
    NoSuchThing -> "does not exist"
    ResourceBusy -> "resource busy"
    ResourceExhausted -> "resource exhausted"
    EOF -> "end of file"
    IllegalOperation -> "illegal operation"
    PermissionDenied -> "permission denied"
    UserError -> "user error"
    HardwareFault -> "hardware fault"
    InappropriateType -> "inappropriate type"
    Interrupted -> "interrupted"
    InvalidArgument -> "invalid argument"
    OtherError -> "failed"
    ProtocolError -> "protocol error"
    ResourceVanished -> "resource vanished"
    SystemError -> "system error"
    TimeExpired -> "timeout"
    UnsatisfiedConstraints -> "unsatisfied constraints"
    UnsupportedOperation -> "unsupported operation"

-- |
-- The kind of a failure the runtime reports, by its number: see
-- @failure@ in the runtime.
failureType :: Int -> IOErrorType
failureType = \case
  1 -> AlreadyExists
  2 -> NoSuchThing
  3 -> ResourceBusy
  4 -> ResourceExhausted
  7 -> PermissionDenied
  8 -> InappropriateType
  9 -> InvalidArgument
  _ -> OtherError

-- |
-- Something of any type, as it was thrown.
data Any

-- |
-- What is thrown: the name of its type, what a program that does not catch
-- it exits with, how it is shown, and the value. The runtime looks at the
-- second and the third when nothing catches it. @Control.Exception@ throws
-- and catches these; an 'IOError' is one named @IOException@.
data Thrown = Thrown String Int Text Any

-- |
-- Throw an error of input or output from anywhere.
throwIOError :: IOError -> a
throwIOError problem =
  primThrow (Thrown "IOException" (-1) (pack (show problem)) (primUnsafeCoerce problem))

ioError :: IOError -> IO a
ioError problem = IO (primSTBind (primSTPure ()) (\() -> throwIOError problem))

userError :: String -> IOError
userError description = IOError Nothing UserError "" description Nothing

catch :: IO a -> (IOError -> IO a) -> IO a
catch (IO action) handler = IO (primCatch action handle)
  where
    handle thrown@(Thrown name _ _ value)
      | name == "IOException" = runIO (handler (primUnsafeCoerce value))
      | otherwise = primThrow thrown

-- |
-- What a handle is called where something goes wrong with it: the file it
-- is of, or what GHC calls the handles every program has.
handleName :: Handle -> IO String
handleName (Handle number) = case number of
  0 -> pure "<stdin>"
  1 -> pure "<stdout>"
  2 -> pure "<stderr>"
  _ -> IO (fmap unpack (primFileName number))

-- |
-- Fail as GHC does where a handle has nothing left to read.
endOfFile :: Handle -> String -> IO a
endOfFile handle location = do
  name <- handleName handle
  ioError (IOError (Just handle) EOF location "" (Just name))

-- |
-- Open a file to read, to write, to add to, or to read and write, as the
-- mode is 0, 1, 2 or 3, or make one that was not there to read and write,
-- as it is 4; or fail as GHC's @openFile@ does.
openDescriptor :: FilePath -> Int -> ST s Int
openDescriptor path mode =
  primSTBind (primOpen (pack path) mode) $ \number ->
    if number >= 0
      then primSTPure number
      else failed "openFile" path number

-- |
-- Fail with what the runtime said of something done to a file, whose kind
-- is the number it gave.
failed :: String -> FilePath -> Int -> ST s a
failed location path number =
  primSTBind primLastFailure $ \description ->
    throwIOError
      IOError
        { ioe_handle = Nothing,
          ioe_type = failureType (negate number - 1),
          ioe_location = location,
          ioe_description = unpack description,
          ioe_filename = Just path
        }

-- |
-- Take a file away, or fail as GHC's @removeFile@ does.
removeFile :: FilePath -> IO ()
removeFile path = IO $
  primSTBind (primRemoveFile (pack path)) $ \number ->
    if number >= 0 then primSTPure () else failed "removeLink" path number

putChar :: Char -> IO ()
putChar c = IO (primWrite 1 (pack [c]))

putStr :: String -> IO ()
putStr s = IO (primWrite 1 (pack s))

putStrLn :: String -> IO ()
putStrLn = putStrLnText . pack

print :: (Show a) => a -> IO ()
print x = putStrLn (show x)

-- |
-- The next character a handle has, or the failure GHC's @hGetChar@ has at
-- the end.
handleChar :: Handle -> IO Char
handleChar handle@(Handle number) = do
  code <- IO (primReadChar number)
  if code < 0
    then endOfFile handle "hGetChar"
    else pure (toEnum code)

-- |
-- The next line a handle has, without its end, or the failure GHC's
-- @hGetLine@ has at the end.
handleLine :: Handle -> IO String
handleLine handle@(Handle number) = do
  text <- IO (primReadLine number)
  case unpack text of
    '1' : line -> pure line
    _ -> endOfFile handle "hGetLine"

getChar :: IO Char
getChar = handleChar (Handle 0)

getLine :: IO String
getLine = handleLine (Handle 0)

getContents :: IO String
getContents = IO (fmap unpack (primReadAll 0))

-- The haskell's report definition has missing variables
interact :: (String -> String) -> IO ()
interact f = getContents >>= \s -> putStr (f s)

readFile :: FilePath -> IO String
readFile path = IO $ do
  file <- openDescriptor path 0
  text <- primReadAll file
  primClose file
  return (unpack text)

writeFile :: FilePath -> String -> IO ()
writeFile path s = IO $ do
  file <- openDescriptor path 1
  primWrite file (pack s)
  primClose file

appendFile :: FilePath -> String -> IO ()
appendFile path s = IO $ do
  file <- openDescriptor path 2
  primWrite file (pack s)
  primClose file

readIO :: (Read a) => String -> IO a
readIO s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> return x
  [] -> ioError (userError "Prelude.readIO: no parse")
  _ -> ioError (userError "Prelude.readIO: ambiguous parse")

readLn :: (Read a) => IO a
readLn = do
  l <- getLine
  r <- readIO l
  return r
