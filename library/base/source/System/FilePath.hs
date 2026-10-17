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

import Data.Char (isAlpha, toLower, toUpper)
import Data.List (dropWhileEnd, isSuffixOf, stripPrefix)
import System.Environment (lookupEnv)
import System.Info (os)

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
searchPathSeparator = if windows then ';' else ':'

isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator c = c == searchPathSeparator

extSeparator :: Char
extSeparator = '.'

isExtSeparator :: Char -> Bool
isExtSeparator c = c == '.'

splitSearchPath :: String -> [FilePath]
-- | The directories of a search path. An empty one is the present
-- directory, except on Windows, where it is left out and quotes are taken
-- off.
splitSearchPath = go
  where
    go text = case break isSearchPathSeparator text of
      (first, []) -> entry first
      (first, _ : rest) -> entry first ++ go rest
    entry "" = if windows then [] else ["."]
    entry ('"' : quoted) | windows, not (null quoted), last quoted == '"' = [init quoted]
    entry directory = [directory]

getSearchPath :: IO [FilePath]
getSearchPath = maybe [] splitSearchPath <$> lookupEnv "PATH"

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
(-<.>) = replaceExtension

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
splitExtensions path = (directory ++ base, extensions)
  where
    (directory, name) = cut path
    (base, extensions) = break isExtSeparator name

dropExtensions :: FilePath -> FilePath
dropExtensions = fst . splitExtensions

takeExtensions :: FilePath -> String
takeExtensions = snd . splitExtensions

replaceExtensions :: FilePath -> String -> FilePath
replaceExtensions path extension = dropExtensions path <.> extension

isExtensionOf :: String -> FilePath -> Bool
isExtensionOf extension@('.' : _) = isSuffixOf extension . takeExtensions
isExtensionOf extension = isSuffixOf ('.' : extension) . takeExtensions

stripExtension :: String -> FilePath -> Maybe FilePath
stripExtension [] path = Just path
stripExtension extension@(first : _) path =
  reverse <$> stripPrefix (reverse dotted) (reverse path)
  where
    dotted = if isExtSeparator first then extension else '.' : extension

splitFileName :: FilePath -> (String, String)
splitFileName path = case cut path of
  ("", name) -> ("./", name)
  other -> other

takeFileName :: FilePath -> FilePath
takeFileName = snd . cut

replaceFileName :: FilePath -> String -> FilePath
replaceFileName path name = fst (cut path) </> name

dropFileName :: FilePath -> FilePath
dropFileName = fst . splitFileName

takeBaseName :: FilePath -> String
takeBaseName = dropExtension . takeFileName

replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName path base = joined directory (base <.> takeExtension name)
  where
    (directory, name) = cut path

takeDirectory :: FilePath -> FilePath
takeDirectory = dropTrailingPathSeparator . dropFileName

replaceDirectory :: FilePath -> String -> FilePath
replaceDirectory path directory = joined directory (takeFileName path)

combine :: FilePath -> FilePath -> FilePath
combine a b
  | hasDrive b || not (null b) && isPathSeparator (head b) = b
  | null a = b
  | null b = a
  | isPathSeparator (last a) = a ++ b
  | otherwise = a ++ [pathSeparator] ++ b

infixr 5 </>

(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine

splitPath :: FilePath -> [FilePath]
splitPath path = [drive | not (null drive)] ++ go rest
  where
    (drive, rest) = splitDrive path
    go "" = []
    go text = (name ++ separators) : go after
      where
        (name, more) = break isPathSeparator text
        (separators, after) = span isPathSeparator more

joinPath :: [FilePath] -> FilePath
joinPath = foldr combine ""

splitDirectories :: FilePath -> [FilePath]
splitDirectories = map dropTrailingPathSeparator . splitPath

splitDrive :: FilePath -> (FilePath, FilePath)
-- | The drive a path begins with: on Windows a letter and a colon, with the
-- separators after it, and otherwise the separators it begins with.
splitDrive path = case path of
  letter : ':' : rest
    | windows,
      isAlpha letter,
      (separators, after) <- span isPathSeparator rest ->
        (letter : ':' : separators, after)
  _ -> span isPathSeparator path

joinDrive :: FilePath -> FilePath -> FilePath
joinDrive = joined

takeDrive :: FilePath -> FilePath
takeDrive = fst . splitDrive

hasDrive :: FilePath -> Bool
hasDrive path = case path of
  letter : ':' : _ | windows -> isAlpha letter
  first : _ -> isPathSeparator first
  [] -> False

dropDrive :: FilePath -> FilePath
dropDrive = snd . splitDrive

isDrive :: FilePath -> Bool
isDrive path = not (null path) && null (dropDrive path)

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
-- | The path with its separators all the one, no more than one between two
-- names, and no "." in it, as filepath has it.
normalise path = result ++ [pathSeparator | directoryPath rest && not (hasTrailingPathSeparator result)]
  where
    (drive, rest) = splitDrive path
    result = case (normalDrive drive, names rest) of
      ("", "") -> "."
      (drive', names') -> joined drive' names'
    normalDrive "" = ""
    normalDrive drive'
      | windows, letter : ':' : more <- drive' = toUpper letter : ':' : [pathSeparator | not (null more)]
      | otherwise = [pathSeparator]
    names = joinPath . filter (/= ".") . splitDirectories
    directoryPath xs =
      hasTrailingPathSeparator xs || not (null xs) && last xs == '.' && hasTrailingPathSeparator (init xs)

equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath a b = simple a == simple b
  where
    simple = (if windows then map toLower else id) . dropTrailingPathSeparator . normalise

makeRelative :: FilePath -> FilePath -> FilePath
-- | The second path from the first, if it is under it, as filepath has it.
makeRelative from path
  | equalFilePath from path = "."
  | rooted from /= rooted path = path
  | otherwise = go (unrooted from) (unrooted path)
  where
    go "" rest = dropWhile isPathSeparator rest
    go a b =
      let (a1, a2) = first a
          (b1, b2) = first b
       in if equalFilePath a1 b1 then go a2 b2 else path
    first text = (dropWhile isPathSeparator name, dropWhile isPathSeparator rest)
      where
        (name, rest) = break isPathSeparator (dropWhile isPathSeparator text)
    rooted text = map (\c -> if isPathSeparator c then pathSeparator else toLower c) (takeDrive text)
    unrooted = dropDrive

isRelative :: FilePath -> Bool
isRelative = not . hasDrive

isAbsolute :: FilePath -> Bool
isAbsolute = hasDrive

isValid :: FilePath -> Bool
-- | Whether a path can be one: not empty, with no 0 in it, and on Windows
-- none of the characters Windows refuses in a name.
isValid path =
  not (null path)
    && notElem '\0' path
    && (not windows || not (any (`elem` "<>\"|?*") (dropDrive path)))

makeValid :: FilePath -> FilePath
makeValid "" = "_"
makeValid path = map valid path
  where
    valid c
      | c == '\0' = '_'
      | windows && c `elem` "<>\"|?*" && c `notElem` takeDrive path = '_'
      | otherwise = c

-- Two paths joined with a separator between them, whatever the second is.
joined :: FilePath -> FilePath -> FilePath
joined a b
  | null a = b
  | null b = a
  | hasTrailingPathSeparator a = a ++ b
  | otherwise = a ++ [pathSeparator] ++ b
