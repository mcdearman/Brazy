module System.Process where

import Data.List (intercalate)
import Hazy (primCallProcess)
import Hazy.Prelude (IO (..), pack)

callProcess :: FilePath -> [String] -> IO ()
callProcess program arguments = do
  status <- IO (primCallProcess (pack program) (pack (intercalate "\n" arguments)))
  if status == 0
    then pure ()
    else error ("callProcess: " ++ program ++ " (exit " ++ show status ++ ")")
