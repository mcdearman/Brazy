module Main where

import Debug.Trace (trace)

-- A number is read with the space around it passed over, as any token is.

main :: IO ()
main = do
  print (read " 1 " :: Int)
  print (read " ( -42 ) " :: Integer)
  print (read "  [1, 2 ,3]  " :: [Int])
  print (read "( 7 , -8 )" :: (Int, Int))
  print (trace "traced" (read "5" :: Int))
