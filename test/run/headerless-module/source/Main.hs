-- A module that is its body alone, as the report allows: it is the module
-- Main, and gives main.

import Data.Char (toUpper)

greeting :: String
greeting = "hello"

main :: IO ()
main = do
  putStrLn (map toUpper greeting)
  print (helper 20)
  where
    helper n = n + 1 :: Int
