module System.FilePath
  ( FilePath,
    pathSeparator,
    pathSeparators,
    isPathSeparator,
    searchPathSeparator,
    isSearchPathSeparator,
    extSeparator,
    isExtSeparator,
    splitSearchPath,
    getSearchPath,
    splitExtension,
    takeExtension,
    replaceExtension,
    (-<.>),
    dropExtension,
    addExtension,
    hasExtension,
    (<.>),
    splitExtensions,
    dropExtensions,
    takeExtensions,
    replaceExtensions,
    isExtensionOf,
    stripExtension,
    splitFileName,
    takeFileName,
    replaceFileName,
    dropFileName,
    takeBaseName,
    replaceBaseName,
    takeDirectory,
    replaceDirectory,
    combine,
    (</>),
    splitPath,
    joinPath,
    splitDirectories,
    splitDrive,
    joinDrive,
    takeDrive,
    hasDrive,
    dropDrive,
    isDrive,
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,
    normalise,
    equalFilePath,
    makeRelative,
    isRelative,
    isAbsolute,
    isValid,
    makeValid,
  )
where

import Data.Char (isAlpha)
import Data.List (dropWhileEnd)
import System.Info (os)
import Hazy.Prelude (placeholder)

-- Whether paths are those of Windows, where either stroke parts a path.
windows :: Bool
windows = os == "mingw32"

-- A path cut after its last separator: what comes before with the
-- separator, and the name after.
cut :: FilePath -> (FilePath, FilePath)
cut path = (reverse directory, reverse name)
  where
    (name, directory) = break isPathSeparator (reverse path)

pathSeparator :: Char
pathSeparator = if windows then '\\' else '/'

pathSeparators :: [Char]
pathSeparators = if windows then "\\/" else "/"

isPathSeparator :: Char -> Bool
isPathSeparator c = c `elem` pathSeparators

searchPathSeparator :: Char
searchPathSeparator = placeholder

isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator = placeholder

extSeparator :: Char
extSeparator = '.'

isExtSeparator :: Char -> Bool
isExtSeparator c = c == '.'

splitSearchPath :: String -> [FilePath]
splitSearchPath = placeholder

getSearchPath :: IO [FilePath]
getSearchPath = placeholder

splitExtension :: FilePath -> (String, String)
splitExtension path = case break isExtSeparator (reverse name) of
  (_, "") -> (path, "")
  (extension, _ : rest) -> (directory ++ reverse rest, '.' : reverse extension)
  where
    (directory, name) = cut path

takeExtension :: FilePath -> String
takeExtension = snd . splitExtension

replaceExtension :: FilePath -> String -> FilePath
replaceExtension path extension = addExtension (dropExtension path) extension

infixr 7 -<.>

(-<.>) :: FilePath -> String -> FilePath
(-<.>) = placeholder

dropExtension :: FilePath -> FilePath
dropExtension = fst . splitExtension

addExtension :: FilePath -> String -> FilePath
addExtension path "" = path
addExtension path extension@(first : _)
  | isExtSeparator first = path ++ extension
  | otherwise = path ++ "." ++ extension

hasExtension :: FilePath -> Bool
hasExtension = not . null . takeExtension

infixr 7 <.>

(<.>) :: FilePath -> String -> FilePath
(<.>) = addExtension

splitExtensions :: FilePath -> (FilePath, String)
splitExtensions = placeholder

dropExtensions :: FilePath -> FilePath
dropExtensions = placeholder

takeExtensions :: FilePath -> String
takeExtensions = placeholder

replaceExtensions :: FilePath -> String -> FilePath
replaceExtensions = placeholder

isExtensionOf :: String -> FilePath -> Bool
isExtensionOf = placeholder

stripExtension :: String -> FilePath -> Maybe FilePath
stripExtension = placeholder

splitFileName :: FilePath -> (String, String)
splitFileName path = case cut path of
  ("", name) -> ("./", name)
  other -> other

takeFileName :: FilePath -> FilePath
takeFileName = snd . cut

replaceFileName :: FilePath -> String -> FilePath
replaceFileName = placeholder

dropFileName :: FilePath -> FilePath
dropFileName = fst . splitFileName

takeBaseName :: FilePath -> String
takeBaseName = dropExtension . takeFileName

replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName = placeholder

takeDirectory :: FilePath -> FilePath
takeDirectory = dropTrailingPathSeparator . dropFileName

replaceDirectory :: FilePath -> String -> FilePath
replaceDirectory = placeholder

combine :: FilePath -> FilePath -> FilePath
combine a b
  | hasDrive b || not (null b) && isPathSeparator (head b) = b
  | null a = b
  | isPathSeparator (last a) = a ++ b
  | otherwise = a ++ [pathSeparator] ++ b

infixr 5 </>

(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine

splitPath :: FilePath -> [FilePath]
splitPath = placeholder

joinPath :: [FilePath] -> FilePath
joinPath = placeholder

splitDirectories :: FilePath -> [FilePath]
splitDirectories = placeholder

splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive = placeholder

joinDrive :: FilePath -> FilePath -> FilePath
joinDrive = placeholder

takeDrive :: FilePath -> FilePath
takeDrive = placeholder

hasDrive :: FilePath -> Bool
hasDrive path = case path of
  letter : ':' : _ | windows -> isAlpha letter
  first : _ -> isPathSeparator first
  [] -> False

dropDrive :: FilePath -> FilePath
dropDrive = placeholder

isDrive :: FilePath -> Bool
isDrive = placeholder

hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator path = not (null path) && isPathSeparator (last path)

addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator path = if hasTrailingPathSeparator path then path else path ++ [pathSeparator]

dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator path
  | hasTrailingPathSeparator path = case dropWhileEnd isPathSeparator path of
      "" -> [last path]
      shorter -> shorter
  | otherwise = path

normalise :: FilePath -> FilePath
normalise = placeholder

equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath = placeholder

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative = placeholder

isRelative :: FilePath -> Bool
isRelative = not . hasDrive

isAbsolute :: FilePath -> Bool
isAbsolute = hasDrive

isValid :: FilePath -> Bool
isValid = placeholder

makeValid :: FilePath -> FilePath
makeValid = placeholder
