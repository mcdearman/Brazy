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

import Hazy
  ( primArg,
    primArgCount,
    primCatch,
    primSTBind,
    primThrow,
    primEnvironment,
    primExecutablePath,
    primGetEnv,
    primProgName,
    primSetArgs,
    primSetEnv,
    primSetProgName,
    primUnsetEnv,
  )
import Hazy.Prelude (IO (..), pack, unpack)
import Hazy.PreludeIO (IOErrorType (NoSuchThing), IOException (..), ioError)

getArgs :: IO [String]
getArgs = IO $ do
  count <- primArgCount
  mapM (\index -> fmap unpack (primArg index)) [0 .. count - 1]

getProgName :: IO String
getProgName = IO (fmap unpack primProgName)

executablePath :: Maybe (IO (Maybe FilePath))
executablePath = Just (fmap Just getExecutablePath)

getExecutablePath :: IO FilePath
getExecutablePath = IO (fmap unpack primExecutablePath)

getEnv :: String -> IO String
getEnv name = do
  found <- lookupEnv name
  case found of
    Just value -> pure value
    Nothing ->
      ioError
        IOError
          { ioe_handle = Nothing,
            ioe_type = NoSuchThing,
            ioe_location = "getEnv",
            ioe_description = "no environment variable",
            ioe_filename = Just name
          }

lookupEnv :: String -> IO (Maybe String)
lookupEnv name = IO $ do
  found <- primGetEnv (pack name)
  pure $ case unpack found of
    '1' : value -> Just value
    _ -> Nothing

setEnv :: String -> String -> IO ()
setEnv name value = IO (primSetEnv (pack name) (pack value))

unsetEnv :: String -> IO ()
unsetEnv name = IO (primUnsetEnv (pack name))

-- Run an action with other arguments, and give the program its own back
-- after, however the action ends.
withArgs :: [String] -> IO a -> IO a
withArgs given action = do
  old <- getArgs
  setArgs given
  restoring (setArgs old) action
  where
    setArgs arguments = IO (primSetArgs (pack (concatMap (++ "\0") arguments)))

withProgName :: String -> IO a -> IO a
withProgName name action = do
  old <- getProgName
  setName name
  restoring (setName old) action
  where
    setName given = IO (primSetProgName (pack given))

-- Run an action, and then another however the first ends: if something is
-- thrown, it is thrown on as it was.
restoring :: IO () -> IO a -> IO a
restoring restore (IO action) = do
  result <- IO (primCatch action (\thrown -> primSTBind (runIO restore) (\() -> primThrow thrown)))
  restore
  pure result

getEnvironment :: IO [(String, String)]
getEnvironment = IO (fmap (pairs . split . unpack) primEnvironment)
  where
    split text = case break (== '\0') text of
      ("", "") -> []
      (part, _ : rest) -> part : split rest
      (part, "") -> [part]
    pairs (name : value : rest) = (name, value) : pairs rest
    pairs _ = []
