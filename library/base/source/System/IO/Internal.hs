-- |
-- Files as the runtime has them, for the modules that read and write.
module System.IO.Internal
  ( module System.IO.Internal,
    Handle (..),
  )
where

import Hazy
  ( primClose,
    primFileState,
    primFlush,
    primIsEOF,
    primIsTerminal,
    primPeekChar,
    primReadAll,
    primReadLine,
    primWrite,
  )
import Hazy.Prelude (IO (..), Text, pack, unpack)
import Hazy.PreludeIO (Handle (..), endOfFile, handleChar, handleLine, openDescriptor)

-- A file opened to read, to write, to add to, or to read and write, as the
-- mode is 0, 1, 2 or 3. One that cannot be is an IOError, as in GHC.
open :: FilePath -> Int -> IO Handle
open path mode = IO (fmap Handle (openDescriptor path mode))

close :: Handle -> IO ()
close (Handle number) = IO (primClose number)

put :: Handle -> Text -> IO ()
put (Handle number) text = IO (primWrite number text)

flush :: Handle -> IO ()
flush (Handle number) = IO (primFlush number)

-- Everything left to read. Nothing is read lazily, so a file is closed once
-- it has been read.
contents :: Handle -> IO Text
contents (Handle number) = IO $ do
  text <- primReadAll number
  if number > 2 then primClose number else pure ()
  pure text

-- A line without its end, or nothing if there is no more to read.
line :: Handle -> IO (Maybe Text)
line (Handle number) = IO $ do
  text <- primReadLine number
  pure $ case unpack text of
    '1' : rest -> Just (pack rest)
    _ -> Nothing

-- The next line, or an IOError at the end.
nextLine :: Handle -> IO String
nextLine = handleLine

-- The next character, or an IOError at the end.
nextChar :: Handle -> IO Char
nextChar = handleChar

-- The next character, left to be read, or an IOError at the end.
peekChar :: Handle -> IO Char
peekChar handle@(Handle number) = do
  code <- IO (primPeekChar number)
  if code < 0
    then endOfFile handle "hLookAhead"
    else pure (toEnum code)

atEnd :: Handle -> IO Bool
atEnd (Handle number) = IO (primIsEOF number)

terminal :: Handle -> IO Bool
terminal (Handle number) = IO (fmap (/= 0) (primIsTerminal number))

-- Whether a handle is open, and to what: 1 to read, 2 to write and 3 to do
-- both, or 0 if it is not.
state :: Handle -> IO Int
state (Handle number) = IO (primFileState number)
