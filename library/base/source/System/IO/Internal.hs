-- |
-- Files as the runtime has them, for the modules that read and write.
module System.IO.Internal where

import Hazy (primClose, primIsTerminal, primOpen, primReadAll, primReadLine, primWrite)
import Hazy.Prelude (IO (..), Text, pack, unpack)

-- A handle is the number the system knows an open file by.
newtype Handle = Handle Int
  deriving (Eq)

instance Show Handle where
  showsPrec _ (Handle number) = showString "<handle " . shows number . showString ">"

-- A file opened to read, to write or to add to, as the mode is 0, 1 or 2.
open :: FilePath -> Int -> IO Handle
open path mode = IO (fmap Handle (primOpen (pack path) mode))

close :: Handle -> IO ()
close (Handle number) = IO (primClose number)

put :: Handle -> Text -> IO ()
put (Handle number) text = IO (primWrite number text)

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

terminal :: Handle -> IO Bool
terminal (Handle number) = IO (fmap (/= 0) (primIsTerminal number))
