module System.IO
  ( IO,
    fixIO,
    FilePath,
    Handle,
    stdin,
    stdout,
    stderr,
    withFile,
    openFile,
    IOMode (..),
    hClose,
    readFile,
    readFile',
    writeFile,
    appendFile,
    hFileSize,
    hSetFileSize,
    hIsEOF,
    isEOF,
    BufferMode (..),
    hSetBuffering,
    hGetBuffering,
    hFlush,
    hGetPosn,
    hSetPosn,
    HandlePosn (..),
    hSeek,
    SeekMode (..),
    hTell,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hIsSeekable,
    hIsTerminalDevice,
    hSetEcho,
    hGetEcho,
    hShow,
    hWaitForInput,
    hReady,
    hGetChar,
    hGetLine,
    hLookAhead,
    hGetContents,
    hGetContents,
    hPutChar,
    hPutStr,
    hPutStrLn,
    hPrint,
    interact,
    putChar,
    putStr,
    putStrLn,
    print,
    getChar,
    getLine,
    getContents,
    readIO,
    readLn,
    withBinaryFile,
    openBinaryFile,
    hSetBinaryMode,
    hPutBuf,
    hGetBuf,
    hGetBufSome,
    hPutBufNonBlocking,
    hGetBufNonBlocking,
    openTempFile,
    openBinaryTempFile,
    openTempFileWithDefaultPermissions,
    openBinaryTempFileWithDefaultPermissions,
    hSetEncoding,
    hGetEncoding,
    TextEncoding (..),
    latin1,
    utf8,
    utf8_bom,
    utf16,
    utf16le,
    utf16be,
    utf32,
    utf32le,
    utf32be,
    localeEncoding,
    char8,
    mkTextEncoding,
    hSetNewlineMode,
    Newline (..),
    nativeNewline,
    NewlineMode (..),
    noNewlineTranslation,
    universalNewlineMode,
    nativeNewlineMode,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (pack)
import Data.Text (unpack)
import Foreign.Ptr (Ptr)
import Hazy (primClock, primFileSize, primSeek, primSetFileSize, primTell)
import Hazy.Prelude (IO (..))
import Hazy.PreludeIO (endOfFile)
import System.IO.Error (catchIOError, isAlreadyExistsError, ioError)
import System.IO.Internal (Handle (..))
import qualified System.IO.Internal as Internal
import System.IO.Unsafe (unsafeInterleaveIO)

-- The result is given to the action that makes it, to be looked at only
-- once it is made.
fixIO :: (a -> IO a) -> IO a
fixIO action = do
  place <- newIORef (error "System.IO.fixIO: the result was needed before it was made")
  result <- unsafeInterleaveIO (readIORef place)
  made <- action result
  writeIORef place made
  pure made


stdin :: Handle
stdin = Handle 0

stdout :: Handle
stdout = Handle 1

stderr :: Handle
stderr = Handle 2

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile path mode action = do
  handle <- openFile path mode
  result <- action handle
  hClose handle
  pure result

openFile :: FilePath -> IOMode -> IO Handle
openFile path mode = Internal.open path $ case mode of
  ReadMode -> 0
  WriteMode -> 1
  AppendMode -> 2
  ReadWriteMode -> 3

data IOMode
  = ReadMode
  | WriteMode
  | AppendMode
  | ReadWriteMode

hClose :: Handle -> IO ()
hClose = Internal.close

readFile' :: FilePath -> IO String
readFile' = readFile

hFileSize :: Handle -> IO Integer
hFileSize (Handle number) = IO (primFileSize number)

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize (Handle number) size = IO (primSetFileSize number size)

hIsEOF :: Handle -> IO Bool
hIsEOF = Internal.atEnd

isEOF :: IO Bool
isEOF = hIsEOF stdin

data BufferMode
  = NoBuffering
  | LineBuffering
  | BlockBuffering (Maybe Int)

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering _ _ = pure ()

-- As GHC starts them: a terminal by lines, and the rest in blocks.
hGetBuffering :: Handle -> IO BufferMode
hGetBuffering handle = do
  terminal <- hIsTerminalDevice handle
  pure (if terminal then LineBuffering else BlockBuffering Nothing)

hFlush :: Handle -> IO ()
hFlush = Internal.flush

hGetPosn :: Handle -> IO HandlePosn
hGetPosn handle = fmap (HandlePosn handle) (hTell handle)

hSetPosn :: HandlePosn -> IO ()
hSetPosn (HandlePosn handle place) = hSeek handle AbsoluteSeek place

data HandlePosn = HandlePosn Handle Integer

instance Eq HandlePosn where
  HandlePosn handle place == HandlePosn handle' place' = handle == handle' && place == place'

instance Show HandlePosn where
  showsPrec precedence (HandlePosn handle place) =
    showsPrec precedence handle . showString " at position " . shows place

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek (Handle number) mode offset = IO $ primSeek number from offset
  where
    from = case mode of
      AbsoluteSeek -> 0
      RelativeSeek -> 1
      SeekFromEnd -> 2

data SeekMode
  = AbsoluteSeek
  | RelativeSeek
  | SeekFromEnd

hTell :: Handle -> IO Integer
hTell (Handle number) = IO (primTell number)

hIsOpen :: Handle -> IO Bool
hIsOpen handle = fmap (/= 0) (Internal.state handle)

hIsClosed :: Handle -> IO Bool
hIsClosed handle = fmap (== 0) (Internal.state handle)

hIsReadable :: Handle -> IO Bool
hIsReadable handle = fmap odd (Internal.state handle)

hIsWritable :: Handle -> IO Bool
hIsWritable handle = fmap (>= 2) (Internal.state handle)

-- Only a file that was opened is, not the three every program has.
hIsSeekable :: Handle -> IO Bool
hIsSeekable handle@(Handle number)
  | number > 2 = hIsOpen handle
  | otherwise = pure False

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice = Internal.terminal

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho _ _ = pure ()

-- Nothing that is typed is kept from being shown.
hGetEcho :: Handle -> IO Bool
hGetEcho handle = hIsTerminalDevice handle

hShow :: Handle -> IO String
hShow handle = pure (show handle)

-- Input is there to be read as soon as it is asked for, until the end,
-- which is a failure as in GHC.
hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput handle _ = do
  end <- hIsEOF handle
  if end then endOfFile handle "hWaitForInput" else pure True

hReady :: Handle -> IO Bool
hReady handle = hWaitForInput handle 0

hGetChar :: Handle -> IO Char
hGetChar = Internal.nextChar

hGetLine :: Handle -> IO String
hGetLine = Internal.nextLine

hLookAhead :: Handle -> IO Char
hLookAhead = Internal.peekChar

hGetContents :: Handle -> IO String
hGetContents handle = fmap unpack (Internal.contents handle)

hGetContents' :: Handle -> IO String
hGetContents' = hGetContents

hPutChar :: Handle -> Char -> IO ()
hPutChar handle c = Internal.put handle (pack [c])

hPutStr :: Handle -> String -> IO ()
hPutStr handle characters = Internal.put handle (pack characters)

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn handle characters = Internal.put handle (pack (characters ++ "\n"))

hPrint :: (Show a) => Handle -> a -> IO ()
hPrint handle value = hPutStrLn handle (show value)

getContents' :: IO String
getContents' = getContents

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = withFile

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile = openFile

hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode _ _ = pure ()

-- | So many bytes from memory, written to the file as they are.
hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf (Handle descriptor) = c_putBuf descriptor

-- | So many bytes read from the file into memory, or fewer at its end; how
-- many were read.
hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf (Handle descriptor) address count = c_getBuf descriptor address count 0

-- | As many bytes as can be had at once, and no more than so many.
hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
hGetBufSome (Handle descriptor) address count = c_getBuf descriptor address count 1

-- | Writing does not wait here, so all is written.
hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hPutBufNonBlocking handle address count = count <$ hPutBuf handle address count

hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking = hGetBufSome

foreign import ccall unsafe "hazy_put_buf" c_putBuf :: Int -> Ptr a -> Int -> IO ()

foreign import ccall unsafe "hazy_get_buf" c_getBuf :: Int -> Ptr a -> Int -> Int -> IO Int

-- A file that was not there, made in a directory and named after a
-- template: what is before the template's extension, a number, and the
-- extension. It is open to read and write.
openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile directory template = do
  start <- IO primClock
  go (abs start `mod` 1000000)
  where
    (stem, extension) = case break (== '.') (reverse template) of
      (reversed, '.' : rest) -> (reverse rest, '.' : reverse reversed)
      _ -> (template, "")
    go number = do
      let path = directory ++ "/" ++ stem ++ show number ++ extension
      made <-
        catchIOError
          (fmap Just (Internal.open path 4))
          (\problem -> if isAlreadyExistsError problem then pure Nothing else ioError problem)
      case made of
        Just handle -> pure (path, handle)
        Nothing -> go (number + 1)

openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile = openTempFile

openTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openTempFileWithDefaultPermissions = openTempFile

openBinaryTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions = openTempFile

hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding _ _ = pure ()

hGetEncoding :: Handle -> IO (Maybe TextEncoding)
hGetEncoding _ = pure (Just utf8)

-- Text is read and written as UTF-8, whichever of these is asked for.
newtype TextEncoding = TextEncoding String

latin1 :: TextEncoding
latin1 = TextEncoding "ISO-8859-1"

utf8 :: TextEncoding
utf8 = TextEncoding "UTF-8"

utf8_bom :: TextEncoding
utf8_bom = TextEncoding "UTF-8BOM"

utf16 :: TextEncoding
utf16 = TextEncoding "UTF-16"

utf16le :: TextEncoding
utf16le = TextEncoding "UTF-16LE"

utf16be :: TextEncoding
utf16be = TextEncoding "UTF-16BE"

utf32 :: TextEncoding
utf32 = TextEncoding "UTF-32"

utf32le :: TextEncoding
utf32le = TextEncoding "UTF-32LE"

utf32be :: TextEncoding
utf32be = TextEncoding "UTF-32BE"

localeEncoding :: TextEncoding
localeEncoding = utf8

char8 :: TextEncoding
char8 = TextEncoding "binary"

mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding name = pure (TextEncoding name)

hSetNewlineMode :: Handle -> NewlineMode -> IO ()
hSetNewlineMode _ _ = pure ()

data Newline
  = LF
  | CRLF

nativeNewline :: Newline
nativeNewline = LF

data NewlineMode = NewlineMode
  { inputNL :: Newline,
    outputNL :: Newline
  }

noNewlineTranslation :: NewlineMode
noNewlineTranslation = NewlineMode {inputNL = LF, outputNL = LF}

universalNewlineMode :: NewlineMode
universalNewlineMode = NewlineMode {inputNL = CRLF, outputNL = LF}

nativeNewlineMode :: NewlineMode
nativeNewlineMode = NewlineMode {inputNL = LF, outputNL = LF}
