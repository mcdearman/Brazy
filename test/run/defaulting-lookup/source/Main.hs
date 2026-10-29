module Main where

-- A number whose type nothing settles is the first type of the default
-- list that is an instance of every class asked of it. The instances are
-- looked up as any other is, so a class of the library, as Ix is, can be
-- asked of it, and so can a class of the program's own.

import Data.Ix (inRange, range)

class Describe a where
  describe :: a -> String

instance Describe Integer where
  describe n = "the integer " ++ show n

instance Describe Double where
  describe x = "the double " ++ show x

main :: IO ()
main = do
  print (range (1, 4))
  print (inRange (1, 10) 5)
  putStrLn (describe 3)
  putStrLn (describe 2.5)
  print (describe (2 + 3), 7 `div` 2)
