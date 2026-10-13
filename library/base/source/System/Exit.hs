module System.Exit where

import System.IO (hPutStrLn, stderr)
import Control.Exception (Exception (..), Tag (..), throwIO)

data ExitCode
  = ExitSuccess
  | ExitFailure Int
  deriving (Eq, Ord, Show)

-- To exit is to throw this. A program that catches it goes on, and one
-- that does not stops with what it says.
instance Exception ExitCode where
  exceptionTag = Tag "ExitCode"
  exceptionStatus = \case
    ExitSuccess -> 0
    ExitFailure status -> status

exitWith :: ExitCode -> IO a
exitWith = throwIO

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

die :: String -> IO a
die message = hPutStrLn stderr message >> exitFailure
