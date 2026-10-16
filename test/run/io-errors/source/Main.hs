module Main where

import Control.Exception (IOException, SomeException)
import qualified Control.Exception as Exception
import System.Directory (removeFile)
import System.IO
import System.IO.Error

-- What goes wrong with input and output, and handles, as GHC has them.

scratch :: FilePath
scratch = "io-errors.scratch"

main :: IO ()
main = do
  -- A file that is not there.
  missing <- tryIOError (readFile "io-errors.missing")
  case missing of
    Left problem -> do
      print problem
      print (isDoesNotExistError problem, isAlreadyExistsError problem, ioeGetFileName problem)
      print (ioeGetLocation problem, ioeGetErrorString problem, isNothing (ioeGetHandle problem))
    Right _ -> putStrLn "read a file that is not there"
  -- The user's own.
  thrown <- tryIOError (ioError (userError "boom") :: IO ())
  print thrown
  catchIOError (ioError (userError "caught")) $ \problem ->
    putStrLn ("caught: " ++ ioeGetErrorString problem ++ " " ++ show (isUserError problem))
  print (userError "a" == userError "a", userError "a" == userError "b")
  print (mkIOError eofErrorType "here" Nothing (Just "there"))
  print (annotateIOError (userError "x") "where" Nothing (Just "file"))
  changed <- tryIOError (modifyIOError (`ioeSetErrorString` "changed") (ioError (userError "first")))
  print (changed :: Either IOError ())
  -- Through Control.Exception, as an exception like any other.
  viaTry <- Exception.try (readFile "io-errors.missing") :: IO (Either IOException String)
  putStrLn (either (const "an IOException") id viaTry)
  Exception.catch (ioError (userError "any")) $ \problem ->
    putStrLn ("some exception: " ++ show (problem :: SomeException))
  -- Handles.
  writeFile scratch "h\233llo\nworld\n"
  handle <- openFile scratch ReadMode
  size <- hFileSize handle
  first <- hGetChar handle
  ahead <- hLookAhead handle
  second <- hGetChar handle
  rest <- hGetLine handle
  place <- hTell handle
  line <- hGetLine handle
  end <- hIsEOF handle
  print (size, first, ahead, second, rest, place, line, end)
  past <- tryIOError (hGetLine handle)
  print past
  pastChar <- tryIOError (hGetChar handle)
  print pastChar
  hSeek handle AbsoluteSeek 0
  again <- hGetLine handle
  readable <- hIsReadable handle
  writable <- hIsWritable handle
  seekable <- hIsSeekable handle
  print (again, readable, writable, seekable)
  hClose handle
  closed <- hIsClosed handle
  print closed
  -- To read and write.
  both <- openFile scratch ReadWriteMode
  hSeek both SeekFromEnd 0
  hPutStr both "more\n"
  hSeek both AbsoluteSeek 0
  firstLine <- hGetLine both
  hSeek both RelativeSeek 1
  w <- hGetChar both
  position <- hGetPosn both
  hClose both
  print (firstLine, w, show position == "")
  contents <- readFile scratch
  putStr contents
  appendFile scratch "end\n"
  readFile scratch >>= putStr
  removeFile scratch
  gone <- tryIOError (removeFile scratch)
  print (either isDoesNotExistError (const False) gone)
  -- A directory is not a file.
  directory <- tryIOError (openFile "." ReadMode)
  print (either (\problem -> show (ioeGetErrorType problem)) (const "opened") directory)
  where
    isNothing Nothing = True
    isNothing (Just _) = False
