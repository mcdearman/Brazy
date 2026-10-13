module Data.Text.IO
  ( readFile,
    writeFile,
    appendFile,
    hGetContents,
    hGetLine,
    hPutStr,
    hPutStrLn,
    interact,
    getContents,
    getLine,
    putStr,
    putStrLn,
  )
where

import Data.Text (pack)
import qualified System.IO.Internal as Internal
import Prelude hiding (appendFile, getContents, getLine, interact, putStr, putStrLn, readFile, writeFile)
import qualified Prelude
import Data.Text (Text)
import Hazy.Prelude (placeholder, putStrLnText)
import System.IO (Handle)

readFile :: FilePath -> IO Text
readFile path = Internal.open path 0 >>= Internal.contents

writeFile :: FilePath -> Text -> IO ()
writeFile path text = do
  handle <- Internal.open path 1
  Internal.put handle text
  Internal.close handle

appendFile :: FilePath -> Text -> IO ()
appendFile path text = do
  handle <- Internal.open path 2
  Internal.put handle text
  Internal.close handle

hGetContents :: Handle -> IO Text
hGetContents = Internal.contents

hGetLine :: Handle -> IO Text
hGetLine handle = do
  found <- Internal.line handle
  case found of
    Just text -> pure text
    Nothing -> error "Data.Text.IO.hGetLine: end of file"

hPutStr :: Handle -> Text -> IO ()
hPutStr = Internal.put

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn handle text = Internal.put handle text >> Internal.put handle (pack "\n")

interact :: (Text -> Text) -> IO ()
interact f = getContents >>= \text -> putStr (f text)

getContents :: IO Text
getContents = Internal.contents (Internal.Handle 0)

getLine :: IO Text
getLine = hGetLine (Internal.Handle 0)

putStr :: Text -> IO ()
putStr = Internal.put (Internal.Handle 1)

putStrLn :: Text -> IO ()
putStrLn = putStrLnText
