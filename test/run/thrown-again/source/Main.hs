module Main where

import Control.Exception

-- A value that throws while it is worked out, asked for again once what it
-- threw was caught, throws again, as GHC's does: it is not left being
-- worked out, which looked like a loop.

-- At the top of the program.
divided :: Int
divided = sum [1 .. 10] `div` 0

-- Inside another, which is at the top too: both throw again.
around :: Int
around = divided + 1

asked :: String -> Int -> IO ()
asked name value = do
  result <- try (evaluate value)
  putStrLn $ name ++ ": " ++ either (\e -> show (e :: ArithException)) show result

-- Made while the program runs, and shared by everything that asks for it.
local :: Int -> IO ()
local by = do
  let shared = sum [1 .. 40] `div` by
  asked "local" shared
  asked "local again" shared

main :: IO ()
main = do
  asked "divided" divided
  asked "divided again" divided
  asked "around" around
  asked "around again" around
  local 0
  local 5
  -- Asked for again by what catches it.
  nested <-
    try $
      evaluate around `catch` \e -> do
        putStrLn ("first: " ++ show (e :: ArithException))
        evaluate around
  putStrLn $ "nested: " ++ either (\e -> show (e :: ArithException)) show nested
