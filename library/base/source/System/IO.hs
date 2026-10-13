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

import Data.Text (pack)
import Data.Text (unpack)
import System.IO.Internal (Handle (..))
import qualified System.IO.Internal as Internal
import Foreign.Ptr (Ptr)
import Hazy.Prelude (placeholder)

fixIO :: (a -> IO a) -> IO a
fixIO = placeholder


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
  ReadWriteMode -> error "System.IO.openFile: a file is not opened to read and write at once"

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
hFileSize = placeholder

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize = placeholder

hIsEOF :: Handle -> IO Bool
hIsEOF = placeholder

isEOF :: IO Bool
isEOF = placeholder

data BufferMode
  = NoBuffering
  | LineBuffering
  | BlockBuffering (Maybe Int)

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering _ _ = pure ()

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering = placeholder

hFlush :: Handle -> IO ()
hFlush _ = pure ()

hGetPosn :: Handle -> IO HandlePosn
hGetPosn = placeholder

hSetPosn :: HandlePosn -> IO ()
hSetPosn = placeholder

data HandlePosn

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek = placeholder

data SeekMode
  = AbsoluteSeek
  | RelativeSeek
  | SeekFromEnd

hTell :: Handle -> IO Integer
hTell = placeholder

hIsOpen :: Handle -> IO Bool
hIsOpen = placeholder

hIsClosed :: Handle -> IO Bool
hIsClosed = placeholder

hIsReadable :: Handle -> IO Bool
hIsReadable = placeholder

hIsWritable :: Handle -> IO Bool
hIsWritable = placeholder

hIsSeekable :: Handle -> IO Bool
hIsSeekable = placeholder

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice = Internal.terminal

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho _ _ = pure ()

hGetEcho :: Handle -> IO Bool
hGetEcho = placeholder

hShow :: Handle -> IO String
hShow = placeholder

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput = placeholder

hReady :: Handle -> IO Bool
hReady = placeholder

hGetChar :: Handle -> IO Char
hGetChar = placeholder

hGetLine :: Handle -> IO String
hGetLine handle = do
  found <- Internal.line handle
  case found of
    Just text -> pure (unpack text)
    Nothing -> error "System.IO.hGetLine: end of file"

hLookAhead :: Handle -> IO Char
hLookAhead = placeholder

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

hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf = placeholder

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf = placeholder

hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
hGetBufSome = placeholder

hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hPutBufNonBlocking = placeholder

hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking = placeholder

openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile = placeholder

openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile = placeholder

openTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openTempFileWithDefaultPermissions = placeholder

openBinaryTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions = placeholder

hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding _ _ = pure ()

hGetEncoding :: Handle -> IO (Maybe TextEncoding)
hGetEncoding = placeholder

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
