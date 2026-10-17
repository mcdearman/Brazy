module Main where

import Control.Exception (try)
import Data.List (sort)
import System.Directory
import System.FilePath (isAbsolute, takeFileName, (</>))

-- System.Directory, in a directory of its own that is made and taken away
-- again. Nothing shown depends on where it is run.

main :: IO ()
main = do
  let place = "hazy-directory-test"
  removePathForcibly place
  createDirectoryIfMissing True place
  withCurrentDirectory place $ do
    writeFile "a.txt" "hello"
    copyFile "a.txt" "b.txt"
    print =<< getFileSize "b.txt"
    renameFile "b.txt" "c.txt"
    createDirectory "sub"
    renameDirectory "sub" "sub2"
    writeFile ("sub2" </> "x") "x"
    print . sort =<< listDirectory "."
    print . sort =<< getDirectoryContents "."
    here <- getCurrentDirectory
    print (takeFileName here)
    absolute <- makeAbsolute "c.txt"
    print (takeFileName absolute, isAbsolute absolute)
    print =<< makeRelativeToCurrentDirectory absolute
    print . takeFileName =<< canonicalizePath ("sub2" </> ".." </> "c.txt")
    permissions <- getPermissions "c.txt"
    print (readable permissions, writable permissions, searchable permissions)
    setPermissions "c.txt" (setOwnerWritable False permissions)
    print . writable =<< getPermissions "c.txt"
    setPermissions "c.txt" permissions
    print . searchable =<< getPermissions "sub2"
    print . fmap takeFileName =<< findFile ["missing", "sub2", "."] "x"
    print . map takeFileName =<< findFiles [".", "sub2"] "c.txt"
    full <- try (removeDirectory "sub2")
    putStrLn (either (\problem -> const "not empty" (problem :: IOError)) (const "removed") full)
    missing <- try (getFileSize "nothing-here")
    putStrLn (either (\problem -> const "no such file" (problem :: IOError)) show missing)
    print =<< pathIsSymbolicLink "c.txt"
    removeDirectoryRecursive "sub2"
    removePathForcibly "missing"
    print =<< doesDirectoryExist "sub2"
  removeDirectoryRecursive place
  print =<< doesDirectoryExist place
  home <- getHomeDirectory
  temporary <- getTemporaryDirectory
  print (not (null home), not (null temporary), exeExtension `elem` ["", "exe"])
  print (readable emptyPermissions, emptyPermissions == emptyPermissions)
