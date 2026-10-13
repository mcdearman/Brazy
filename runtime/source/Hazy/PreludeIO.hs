module Hazy.PreludeIO where

import Hazy
import Hazy.Prelude

type FilePath = String

data IOError

instance Show IOError where
  showsPrec = placeholder

instance Eq IOError where
  (==) = placeholder

ioError :: IOError -> IO a
ioError = placeholder

userError :: String -> IOError
userError = placeholder

catch :: IO a -> (IOError -> IO a) -> IO a
catch = placeholder

putChar :: Char -> IO ()
putChar c = IO (primWrite 1 (pack [c]))

putStr :: String -> IO ()
putStr s = IO (primWrite 1 (pack s))

putStrLn :: String -> IO ()
putStrLn = putStrLnText . pack

print :: (Show a) => a -> IO ()
print x = putStrLn (show x)

getChar :: IO Char
getChar = placeholder

getLine :: IO String
getLine = do
  text <- IO (primReadLine 0)
  case unpack text of
    '1' : line -> return line
    _ -> error "Prelude.getLine: end of file"

getContents :: IO String
getContents = IO (fmap unpack (primReadAll 0))

-- The haskell's report definition has missing variables
interact :: (String -> String) -> IO ()
interact f = getContents >>= \s -> putStr (f s)

readFile :: FilePath -> IO String
readFile path = IO $ do
  file <- primOpen (pack path) 0
  text <- primReadAll file
  primClose file
  return (unpack text)

writeFile :: FilePath -> String -> IO ()
writeFile path s = IO $ do
  file <- primOpen (pack path) 1
  primWrite file (pack s)
  primClose file

appendFile :: FilePath -> String -> IO ()
appendFile path s = IO $ do
  file <- primOpen (pack path) 2
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
