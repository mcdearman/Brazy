module Main where

main :: IO ()
main = do
  print (read "0x1F" :: Int, read "0o17" :: Int, read "0b101" :: Integer, read "0XfF" :: Integer)
  print (read "-12" :: Int, read " ( -0x10 ) " :: Int, read "007" :: Int, read "0" :: Integer)
  print (reads "12 rest" :: [(Int, String)], reads "0x" :: [(Int, String)], reads "x" :: [(Int, String)])
  print (read "[1,0x2, -3]" :: [Int], read "(4,-5)" :: (Int, Integer))
  print (read "123456789012345678901234567890" :: Integer)
