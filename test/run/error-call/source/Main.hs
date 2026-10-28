module Main where

import Control.Exception (ErrorCall (..), SomeException, catch, evaluate, handle, throw, try)

-- An error, caught as what it is and as any exception.
safeDiv :: Int -> Int -> Int
safeDiv _ 0 = error "divided by nothing"
safeDiv a b = a `div` b

-- Deep inside a value that is only evaluated when it is wanted.
deep :: [Int]
deep = [1, 2, error "deep down", 4]

main :: IO ()
main = do
  result <- try (evaluate (safeDiv 1 0))
  case result of
    Left (ErrorCall message) -> putStrLn ("caught: " ++ message)
    Right value -> print value
  fine <- try (evaluate (safeDiv 10 2))
  case fine of
    Left (ErrorCall message) -> putStrLn ("caught: " ++ message)
    Right value -> print value
  anything <- try (evaluate (length (show deep)))
  putStrLn (either (\e -> "any exception: " ++ show (e :: SomeException)) show anything)
  print (take 2 deep, length deep)
  caught <- evaluate (head ([] :: [Int])) `catch` \e -> do
    putStrLn ("from head: " ++ takeWhile (/= ':') (show (e :: ErrorCall)))
    pure 0
  print caught
  thrown <- handle (\(ErrorCall message) -> pure (length message)) (evaluate (throw (ErrorCall "thrown by hand")))
  print thrown
  print (show (ErrorCall "shown"))
