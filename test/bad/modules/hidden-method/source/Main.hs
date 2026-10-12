module Main where

import Hidden (Hidden, shown)

data Usage = Usage

-- The method cannot be seen from here, so it cannot be defined here.
instance Hidden Usage where
  private _ = 2

main :: IO ()
main = print (shown Usage)
