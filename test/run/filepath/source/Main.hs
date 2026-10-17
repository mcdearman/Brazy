module Main where

import Data.List (intercalate)
import System.FilePath

-- System.FilePath, shown with every separator as a stroke, so that what is
-- shown is the same wherever it is run.

slashed :: FilePath -> FilePath
slashed = map (\c -> if isPathSeparator c then '/' else c)

main :: IO ()
main = do
  print (splitSearchPath (intercalate [searchPathSeparator] ["bin", "usr/bin", "x"]), isSearchPathSeparator searchPathSeparator)
  print ("file.tar.gz" -<.> "zip", splitExtensions "dir/file.tar.gz", dropExtensions "a.b.c", takeExtensions "x.tar.gz", replaceExtensions "x.tar.gz" "zip")
  print (isExtensionOf "gz" "x.tar.gz", isExtensionOf ".tar.gz" "x.tar.gz", isExtensionOf "tar" "x.tar.gz", stripExtension "gz" "x.tar.gz", stripExtension ".zip" "x.tar")
  print (slashed (replaceFileName "dir/old.txt" "new.md"), slashed (replaceBaseName "dir/old.txt" "new"), slashed (replaceDirectory "dir/a.txt" "other"))
  print (map slashed (splitPath "/usr/local/bin/"), map slashed (splitDirectories "/usr/local/bin/"), slashed (joinPath ["a", "b", "c.txt"]))
  print (slashed (takeDrive "/a/b"), slashed (dropDrive "/a/b"), isDrive "/", isDrive "/a", slashed (fst (splitDrive "relative/path")))
  print (map (slashed . normalise) ["/a/./b//c/", "./x/", ".", "a/../b", "//share/x"])
  print (equalFilePath "a/b/" "a/./b", equalFilePath "a/b" "a/c")
  print (slashed (makeRelative "/home/user" "/home/user/docs/a.txt"), slashed (makeRelative "/home/user" "/etc/passwd"), makeRelative "/a" "/a")
  print (isValid "", isValid "ok.txt", makeValid "", slashed (makeValid "fine/name"))
