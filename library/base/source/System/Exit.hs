module System.Exit where

import Control.Exception (Exception)
import Hazy.Prelude (placeholder)

data ExitCode
  = ExitSuccess
  | ExitFailure Int
  deriving (Eq, Ord, Show)

instance Exception ExitCode

exitWith :: ExitCode -> IO a
exitWith = placeholder

exitFailure :: IO a
exitFailure = placeholder

exitSuccess :: IO a
exitSuccess = placeholder

die :: String -> IO a
die = placeholder
