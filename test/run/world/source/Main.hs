module Main where

import Control.Exception (Exception, catch, evaluate, throw, try)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.List (sort)
import Data.IORef (IORef, modifyIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.STRef (STRef, modifySTRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy.IO
import System.Exit (ExitCode (..), exitWith)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.IO (IOMode (AppendMode, ReadMode, WriteMode), hClose, hGetContents, hPutStr, hPutStrLn, openFile, stdout)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

data Failure = Failure String Int
  deriving (Show)

instance Exception Failure

data Setting = Setting {level :: Maybe Int, inside :: [Setting], sign :: Int}
  deriving (Show)

-- A sum kept in a place that is changed as the numbers go by.
total :: [Int] -> Int
total numbers = runST $ do
  place <- newSTRef 0
  forM_ numbers $ \number -> modifySTRef' place (+ number)
  readSTRef place

-- Places are told apart, whatever they hold.
places :: ST s (Bool, Bool, [Int])
places = do
  one <- newSTRef [1 :: Int]
  two <- newSTRef [1]
  modifySTRef one (0 :)
  writeSTRef two [9, 9]
  first <- readSTRef one
  second <- readSTRef two
  pure (one == one, one == two, first ++ second)

checked :: Int -> Int
checked number
  | number > 2 = throw (Failure "too big" number)
  | otherwise = number * 2

main :: IO ()
main = do
  print (total [1 .. 100], runST places)
  counter <- newIORef (0 :: Int)
  forM_ [1 .. 10] $ \number -> modifyIORef counter (+ number)
  modifyIORef' counter (* 2)
  readIORef counter >>= print
  -- What a place holds is not evaluated by being put there.
  lazy <- newIORef (undefined :: Int)
  writeIORef lazy 5
  readIORef lazy >>= print
  -- Thrown from inside what is evaluated, and caught by who began last.
  result <- try (evaluate (checked 1 + checked 2))
  print (result :: Either Failure Int)
  result <- try (evaluate (checked 1 + checked 3))
  print (result :: Either Failure Int)
  (print (checked 7) >> putStrLn "not here") `catch` \(Failure reason number) -> putStrLn (reason ++ ": " ++ show number)
  nested <-
    (((evaluate (checked 4) >> pure "none") `catch` \(Failure _ number) -> when (number > 3) (throw (Failure "again" (number + 1))) >> pure "inner")
      `catch` \(Failure reason number) -> pure (reason ++ " " ++ show number))
  putStrLn nested
  -- An exit is an exception of its own type: a handler of another type lets
  -- it by, and one of its own catches it.
  stopped <- try (exitWith (ExitFailure 3) `catch` \(Failure _ _) -> putStrLn "not this one")
  print (stopped :: Either ExitCode ())
  missed <- try (try (evaluate (checked 9)) :: IO (Either ExitCode Int))
  print (missed :: Either Failure (Either ExitCode Int))
  -- What a field is equal to is shown as it is, and a record among arguments in brackets.
  print (Setting {level = Just 1, inside = [Setting Nothing [] (-2)], sign = -1})
  print (Just (Setting (Just (-3)) [] 0))
  after <- try (pure 3)
  print (after :: Either Failure Int)
  -- Files.
  let directory = ".test/world/a/b"
      file = directory ++ "/text"
  createDirectoryIfMissing True directory
  writeFile file "one\n"
  appendFile file "two\n"
  handle <- openFile file AppendMode
  hPutStr handle "three"
  hPutStrLn handle "!"
  hClose handle
  readFile file >>= putStr
  handle <- openFile file ReadMode
  text <- hGetContents handle
  print (lines text)
  Text.IO.writeFile (directory ++ "/other") (Text.pack "λ and text\n")
  Text.IO.readFile (directory ++ "/other") >>= Text.IO.putStr
  Lazy.IO.writeFile (directory ++ "/lazy") (Lazy.pack (concatMap show [1 .. 3000 :: Int]))
  long <- Text.IO.readFile (directory ++ "/lazy")
  print (Text.length long, Text.take 12 long)
  names <- listDirectory directory
  exists <- traverse doesFileExist [file, directory, directory ++ "/none"]
  folders <- traverse doesDirectoryExist [file, directory, directory ++ "/none"]
  print (sort names, exists, folders)
  -- An action that is put off is run when what it gives is wanted.
  written <- unsafeInterleaveIO (putStrLn "now" >> pure 1)
  putStrLn "before"
  print (written + unsafePerformIO (pure (2 :: Int)))
  putStr "no line end"
  putStr "\n"
  Text.IO.hPutStrLn stdout (Text.pack "done")
