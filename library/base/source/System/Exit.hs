module System.Exit where

import Hazy (primExit)
import Hazy.Prelude (IO (..))
import System.IO (hPutStrLn, stderr)
import Control.Exception (Exception)

data ExitCode
  = ExitSuccess
  | ExitFailure Int
  deriving (Eq, Ord, Show)

instance Exception ExitCode

exitWith :: ExitCode -> IO a
exitWith code = IO $ primExit $ case code of
  ExitSuccess -> 0
  ExitFailure status -> status

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

die :: String -> IO a
die message = hPutStrLn stderr message >> exitFailure
