module Main where

-- An Integer shown in tens: nothing, ones that are less than nothing, which
-- are in brackets where they are given to something, and ones far bigger
-- than a word.

main :: IO ()
main = do
  print (0 :: Integer)
  print (-5 :: Integer, 5 :: Integer)
  putStrLn (showsPrec 7 (-5 :: Integer) "")
  print (Just (-12345678901234567890 :: Integer))
  print (2 ^ 200 :: Integer)
  print (length (show (3 ^ 20000 :: Integer)), take 10 (show (3 ^ 20000 :: Integer)))
  print (negate (10 ^ 30) :: Integer)
