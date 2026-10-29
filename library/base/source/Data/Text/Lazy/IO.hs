module Data.Text.Lazy.IO where

import qualified Data.Text as Strict (Text, concat, length, pack)
import qualified Data.Text.IO as Strict (getContents, getLine, hGetContents, hGetLine, readFile)
import Data.Text.Lazy (fromStrict, toChunks)
import qualified System.IO.Internal as Internal
import Prelude hiding (appendFile, getContents, getLine, interact, putStr, putStrLn, readFile, writeFile)
import qualified Prelude
import Data.Text.Lazy (Text)
import System.IO (Handle)

readFile :: FilePath -> IO Text
readFile path = fmap fromStrict (Strict.readFile path)

writeFile :: FilePath -> Text -> IO ()
writeFile path text = do
  handle <- Internal.open path 1
  hPutStr handle text
  Internal.close handle

appendFile :: FilePath -> Text -> IO ()
appendFile path text = do
  handle <- Internal.open path 2
  hPutStr handle text
  Internal.close handle

hGetContents :: Handle -> IO Text
hGetContents handle = fmap fromStrict (Strict.hGetContents handle)

hGetLine :: Handle -> IO Text
hGetLine handle = fmap fromStrict (Strict.hGetLine handle)

hPutStr :: Handle -> Text -> IO ()
hPutStr handle text = Prelude.mapM_ (Internal.put handle) (batches (toChunks text))

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn handle text = hPutStr handle text >> Internal.put handle (Strict.pack "\n")

interact :: (Text -> Text) -> IO ()
interact f = getContents >>= \text -> putStr (f text)

getContents :: IO Text
getContents = fmap fromStrict Strict.getContents

getLine :: IO Text
getLine = fmap fromStrict Strict.getLine

putStr :: Text -> IO ()
putStr = hPutStr (Internal.Handle 1)

putStrLn :: Text -> IO ()
putStrLn = hPutStrLn (Internal.Handle 1)

-- Pieces of text joined until there are a few thousand characters to
-- write at a time: a text of any length is written without all of it being
-- made first, and one of many small pieces is not written a piece at a time.
batches :: [Strict.Text] -> [Strict.Text]
batches [] = []
batches chunks = case gather 0 chunks of
  (batch, rest) -> Strict.concat batch : batches rest
  where
    gather :: Int -> [Strict.Text] -> ([Strict.Text], [Strict.Text])
    gather size (chunk : others)
      | size < 8192 = case gather (size + Strict.length chunk) others of
          (batch, rest) -> (chunk : batch, rest)
    gather _ rest = ([], rest)
