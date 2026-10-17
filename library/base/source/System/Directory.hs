module System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    removeDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
    renameDirectory,
    listDirectory,
    getDirectoryContents,
    getCurrentDirectory,
    setCurrentDirectory,
    withCurrentDirectory,
    getHomeDirectory,
    XdgDirectory (..),
    getXdgDirectory,
    XdgDirectoryList (..),
    getXdgDirectoryList,
    getAppUserDataDirectory,
    getUserDocumentsDirectory,
    getTemporaryDirectory,
    removeFile,
    renameFile,
    renamePath,
    copyFile,
    copyFileWithMetadata,
    getFileSize,
    canonicalizePath,
    makeAbsolute,
    makeRelativeToCurrentDirectory,
    doesPathExist,
    doesFileExist,
    doesDirectoryExist,
    findExecutable,
    findExecutables,
    findExecutablesInDirectories,
    findFile,
    findFiles,
    findFileWith,
    findFilesWith,
    exeExtension,
    createFileLink,
    createDirectoryLink,
    removeDirectoryLink,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    Permissions,
    emptyPermissions,
    readable,
    writable,
    executable,
    searchable,
    setOwnerReadable,
    setOwnerWritable,
    setOwnerExecutable,
    setOwnerSearchable,
    getPermissions,
    setPermissions,
    copyPermissions,
    isSymbolicLink,
  )
where

import Hazy (primCreateDirectory, primListDirectory, primPathKind)
import Hazy.Prelude (IO (..), pack, unpack)
import Control.Exception (bracket)
import Data.Bits (testBit)
import Data.Int (Int64)
import Foreign.C.Error (throwErrnoIfNull, throwErrnoPathIfMinus1, throwErrnoPathIfMinus1_, throwErrnoPathIfNull)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (free)
import System.Environment (lookupEnv)
import System.FilePath (getSearchPath, isAbsolute, makeRelative, normalise, splitSearchPath, (<.>), (</>))
import System.Info (os)
import qualified Hazy.PreludeIO

createDirectory :: FilePath -> IO ()
createDirectory path = IO (primCreateDirectory (pack path))

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing _ path = IO (primCreateDirectory (pack path))

removeDirectory :: FilePath -> IO ()
removeDirectory = pathAction "removeDirectory" c_removeDirectory

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = pathAction "removeDirectoryRecursive" c_removeDirectoryRecursive

removePathForcibly :: FilePath -> IO ()
-- | What is at a path, and all under it, removed; nothing there is not
-- wrong.
removePathForcibly path = do
  there <- doesPathExist path
  link <- if there then pathIsSymbolicLink path else pure False
  directory <- doesDirectoryExist path
  if link
    then removeDirectoryLink path
    else
      if directory
        then removeDirectoryRecursive path
        else if there then removeFile path else pure ()

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory = rename "renameDirectory"

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = IO (fmap (\names -> lines (unpack names)) (primListDirectory (pack path)))

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = (\names -> "." : ".." : names) <$> listDirectory path

getCurrentDirectory :: IO FilePath
getCurrentDirectory = madeString "getCurrentDirectory" c_currentDirectory

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = pathAction "setCurrentDirectory" c_setCurrentDirectory

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory directory action =
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory directory
    action

getHomeDirectory :: IO FilePath
getHomeDirectory = madeString "getHomeDirectory" c_homeDirectory

data XdgDirectory
  = XdgData
  | XdgConfig
  | XdgCache
  | XdgState

getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
-- | Where the XDG variable for the kind says, if it is set to a path that
-- is absolute, and otherwise where the specification says by default.
getXdgDirectory kind path = do
  set <- lookupEnv variable
  base <- case set of
    Just directory | isAbsolute directory -> pure directory
    _ -> (</> fallback) <$> getHomeDirectory
  pure (base </> path)
  where
    (variable, fallback) = case kind of
      XdgData -> ("XDG_DATA_HOME", ".local/share")
      XdgConfig -> ("XDG_CONFIG_HOME", ".config")
      XdgCache -> ("XDG_CACHE_HOME", ".cache")
      XdgState -> ("XDG_STATE_HOME", ".local/state")

data XdgDirectoryList
  = XdgDataDirs
  | XdgConfigDirs

getXdgDirectoryList :: XdgDirectoryList -> IO [FilePath]
getXdgDirectoryList kind = do
  set <- lookupEnv variable
  pure $ case set of
    Just directories | not (null directories) -> splitSearchPath directories
    _ -> fallback
  where
    (variable, fallback) = case kind of
      XdgDataDirs -> ("XDG_DATA_DIRS", ["/usr/local/share/", "/usr/share/"])
      XdgConfigDirs -> ("XDG_CONFIG_DIRS", ["/etc/xdg"])

getAppUserDataDirectory :: FilePath -> IO FilePath
getAppUserDataDirectory application
  | windows = do
      data' <- lookupEnv "APPDATA"
      case data' of
        Just directory -> pure (directory </> application)
        Nothing -> (</> ("AppData" </> "Roaming" </> application)) <$> getHomeDirectory
  | otherwise = (</> ('.' : application)) <$> getHomeDirectory

getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory
  | windows = (</> "Documents") <$> getHomeDirectory
  | otherwise = getHomeDirectory

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = madeString "getTemporaryDirectory" c_temporaryDirectory

removeFile :: FilePath -> IO ()
removeFile = Hazy.PreludeIO.removeFile

renameFile :: FilePath -> FilePath -> IO ()
renameFile = rename "renameFile"

renamePath :: FilePath -> FilePath -> IO ()
renamePath = rename "renamePath"

copyFile :: FilePath -> FilePath -> IO ()
copyFile from to =
  withCString from $ \from' ->
    withCString to $ \to' ->
      throwErrnoPathIfMinus1_ "copyFile" from (c_copyFile from' to')

copyFileWithMetadata :: FilePath -> FilePath -> IO ()
copyFileWithMetadata from to = do
  copyFile from to
  copyPermissions from to

getFileSize :: FilePath -> IO Integer
getFileSize path =
  withCString path $ \path' ->
    toInteger <$> throwErrnoPathIfMinus1 "getFileSize" path (c_fileSize path')

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path =
  withCString path $ \path' -> do
    made <- throwErrnoPathIfNull "canonicalizePath" path (c_canonicalize path')
    taken made

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute path
  | isAbsolute path = pure (normalise path)
  | otherwise = (\directory -> normalise (directory </> path)) <$> getCurrentDirectory

makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory path = (`makeRelative` path) <$> getCurrentDirectory

doesPathExist :: FilePath -> IO Bool
doesPathExist path = IO (fmap (/= 0) (primPathKind (pack path)))

doesFileExist :: FilePath -> IO Bool
doesFileExist path = IO (fmap (== 1) (primPathKind (pack path)))

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist path = IO (fmap (== 2) (primPathKind (pack path)))

findExecutable :: String -> IO (Maybe FilePath)
findExecutable name = do
  found <- findExecutables name
  pure $ case found of
    first : _ -> Just first
    [] -> Nothing

findExecutables :: String -> IO [FilePath]
findExecutables name = getSearchPath >>= (`findExecutablesInDirectories` name)

findExecutablesInDirectories :: [FilePath] -> String -> IO [FilePath]
findExecutablesInDirectories directories name =
  findFilesWith runs directories (name <.> exeExtension)
  where
    runs path = executable <$> getPermissions path

findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile = findFileWith (\_ -> pure True)

findFiles :: [FilePath] -> String -> IO [FilePath]
findFiles = findFilesWith (\_ -> pure True)

findFileWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO (Maybe FilePath)
findFileWith wanted directories name = do
  found <- findFilesWith wanted directories name
  pure $ case found of
    first : _ -> Just first
    [] -> Nothing

findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]
findFilesWith wanted directories name = concat <$> mapM look directories
  where
    look directory = do
      let path = directory </> name
      there <- doesFileExist path
      keep <- if there then wanted path else pure False
      pure [path | keep]

exeExtension :: String
exeExtension = if windows then "exe" else ""

createFileLink :: FilePath -> FilePath -> IO ()
createFileLink target link = makeLink "createFileLink" 0 target link

createDirectoryLink :: FilePath -> FilePath -> IO ()
createDirectoryLink target link = makeLink "createDirectoryLink" 1 target link

removeDirectoryLink :: FilePath -> IO ()
removeDirectoryLink = pathAction "removeDirectoryLink" c_removeLink

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink path =
  withCString path $ \path' ->
    (== 1) <$> throwErrnoPathIfMinus1 "pathIsSymbolicLink" path (c_isLink path')

getSymbolicLinkTarget :: FilePath -> IO FilePath
getSymbolicLinkTarget path =
  withCString path $ \path' -> do
    made <- throwErrnoPathIfNull "getSymbolicLinkTarget" path (c_readLink path')
    taken made

-- | What may be done with what is at a path: read it, write it, run it if
-- it is a file, and search it if it is a directory.
data Permissions = Permissions Bool Bool Bool Bool
  deriving (Eq, Ord)

instance Show Permissions where
  showsPrec d (Permissions r w x s) =
    showParen (d > 10) $
      showString "Permissions {readable = "
        . shows r
        . showString ", writable = "
        . shows w
        . showString ", executable = "
        . shows x
        . showString ", searchable = "
        . shows s
        . showString "}"

emptyPermissions :: Permissions
emptyPermissions = Permissions False False False False

readable :: Permissions -> Bool
readable (Permissions may _ _ _) = may

writable :: Permissions -> Bool
writable (Permissions _ may _ _) = may

executable :: Permissions -> Bool
executable (Permissions _ _ may _) = may

searchable :: Permissions -> Bool
searchable (Permissions _ _ _ may) = may

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable may (Permissions _ w x s) = Permissions may w x s

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable may (Permissions r _ x s) = Permissions r may x s

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable may (Permissions r w _ s) = Permissions r w may s

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable may (Permissions r w x _) = Permissions r w x may

getPermissions :: FilePath -> IO Permissions
getPermissions path =
  withCString path $ \path' -> do
    bits <- throwErrnoPathIfMinus1 "getPermissions" path (c_permissions path')
    pure (Permissions (testBit bits 0) (testBit bits 1) (testBit bits 2) (testBit bits 3))

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions path (Permissions r w x s) =
  withCString path $ \path' ->
    throwErrnoPathIfMinus1_ "setPermissions" path (c_setPermissions path' bits)
  where
    bits = sum [bit | (bit, True) <- zip [1, 2, 4, 8] [r, w, x, s]]

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions from to = getPermissions from >>= setPermissions to

isSymbolicLink :: FilePath -> IO Bool
isSymbolicLink = pathIsSymbolicLink

-- Whether this is Windows, where things are called otherwise.
windows :: Bool
windows = os == "mingw32"

-- What the runtime does to what is at a path, which fails as C does.
pathAction :: String -> (CString -> IO CInt) -> FilePath -> IO ()
pathAction name action path = withCString path (throwErrnoPathIfMinus1_ name path . action)

rename :: String -> FilePath -> FilePath -> IO ()
rename name from to =
  withCString from $ \from' ->
    withCString to $ \to' ->
      throwErrnoPathIfMinus1_ name from (c_rename from' to')

makeLink :: String -> CInt -> FilePath -> FilePath -> IO ()
makeLink name directory target link =
  withCString target $ \target' ->
    withCString link $ \link' ->
      throwErrnoPathIfMinus1_ name link (c_createLink target' link' directory)

-- A string the runtime made with malloc, read and freed.
taken :: CString -> IO String
taken made = do
  text <- peekCString made
  free made
  pure text

madeString :: String -> IO CString -> IO FilePath
madeString name make = throwErrnoIfNull name make >>= taken

foreign import ccall unsafe "hazy_remove_directory" c_removeDirectory :: CString -> IO CInt

foreign import ccall unsafe "hazy_remove_directory_recursive" c_removeDirectoryRecursive :: CString -> IO CInt

foreign import ccall unsafe "hazy_rename" c_rename :: CString -> CString -> IO CInt

foreign import ccall unsafe "hazy_current_directory" c_currentDirectory :: IO CString

foreign import ccall unsafe "hazy_set_current_directory" c_setCurrentDirectory :: CString -> IO CInt

foreign import ccall unsafe "hazy_home_directory" c_homeDirectory :: IO CString

foreign import ccall unsafe "hazy_temporary_directory" c_temporaryDirectory :: IO CString

foreign import ccall unsafe "hazy_copy_file" c_copyFile :: CString -> CString -> IO CInt

foreign import ccall unsafe "hazy_file_size" c_fileSize :: CString -> IO Int64

foreign import ccall unsafe "hazy_canonicalize" c_canonicalize :: CString -> IO CString

foreign import ccall unsafe "hazy_create_link" c_createLink :: CString -> CString -> CInt -> IO CInt

foreign import ccall unsafe "hazy_remove_link" c_removeLink :: CString -> IO CInt

foreign import ccall unsafe "hazy_read_link" c_readLink :: CString -> IO CString

foreign import ccall unsafe "hazy_is_link" c_isLink :: CString -> IO CInt

foreign import ccall unsafe "hazy_permissions" c_permissions :: CString -> IO CInt

foreign import ccall unsafe "hazy_set_permissions" c_setPermissions :: CString -> CInt -> IO CInt
