module Data.Text.Lazy.IO where

import qualified Data.Text as Strict (pack)
import qualified Data.Text.IO as Strict (getContents, getLine, hGetContents, hGetLine, readFile)
import Data.Text.Lazy (fromStrict, unpack)
import qualified System.IO.Internal as Internal
import Prelude hiding (appendFile, getContents, getLine, interact, putStr, putStrLn, readFile, writeFile)
import qualified Prelude
import Data.Text.Lazy (Text)
import Hazy.Prelude (placeholder)
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
hPutStr handle text = Prelude.mapM_ (\piece -> Internal.put handle (Strict.pack piece)) (pieces (unpack text))

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

-- Characters a few thousand at a time, so that text of any length is
-- written without all of it being made first.
pieces :: [Char] -> [[Char]]
pieces [] = []
pieces characters = case Prelude.splitAt 4096 characters of
  (before, after) -> before : pieces after
