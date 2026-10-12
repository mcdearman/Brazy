module System.Environment
  ( getArgs,
    getProgName,
    executablePath,
    getExecutablePath,
    getEnv,
    lookupEnv,
    setEnv,
    unsetEnv,
    withArgs,
    withProgName,
    getEnvironment,
  )
where

import Hazy (primArg, primArgCount, primExecutablePath, primGetEnv)
import Hazy.Prelude (IO (..), pack, unpack)
import Hazy.Prelude (placeholder)

getArgs :: IO [String]
getArgs = IO $ do
  count <- primArgCount
  mapM (\index -> fmap unpack (primArg index)) [0 .. count - 1]

getProgName :: IO String
getProgName = placeholder

executablePath :: Maybe (IO (Maybe FilePath))
executablePath = placeholder

getExecutablePath :: IO FilePath
getExecutablePath = IO (fmap unpack primExecutablePath)

getEnv :: String -> IO String
getEnv name = do
  found <- lookupEnv name
  case found of
    Just value -> pure value
    Nothing -> error ("System.Environment.getEnv: " ++ name ++ " is not set")

lookupEnv :: String -> IO (Maybe String)
lookupEnv name = IO $ do
  found <- primGetEnv (pack name)
  pure $ case unpack found of
    '1' : value -> Just value
    _ -> Nothing

setEnv :: String -> String -> IO ()
setEnv = placeholder

unsetEnv :: String -> IO ()
unsetEnv = placeholder

withArgs :: [String] -> IO a -> IO a
withArgs = placeholder

withProgName :: String -> IO a -> IO a
withProgName = placeholder

getEnvironment :: IO [(String, String)]
getEnvironment = placeholder
