module Main where

import Control.Exception
import Data.Ratio ((%))

-- What GHC throws where arithmetic goes wrong and where no pattern matches,
-- caught.

data Shape = Circle | Square | Triangle
  deriving (Show)

corners :: Shape -> Int
corners Square = 4
corners Triangle = 3

firstOf :: [a] -> a
firstOf (x : _) = x

arith :: Int -> IO ()
arith n = do
  quotient <- try (evaluate (n `div` 0))
  print (quotient :: Either ArithException Int)
  remainder <- try (evaluate (n `mod` (0 :: Int)))
  print (remainder :: Either ArithException Int)
  big <- try (evaluate ((2 ^ (70 :: Int) + toInteger n) `quot` 0))
  print (big :: Either ArithException Integer)
  bigger <- try (evaluate ((2 ^ (70 :: Int) + toInteger n) `rem` 0))
  print (bigger :: Either ArithException Integer)

matched :: IO Int -> IO ()
matched action = do
  result <- try action
  putStrLn $ case result of
    Left (PatternMatchFail _) -> "no pattern matched"
    Right value -> "matched " ++ show value

main :: IO ()
main = do
  arith 7
  arith (-3)
  print (17 `div` 5 :: Int, 17 `mod` (-5) :: Int, (-17) `quot` 5 :: Integer, (-17) `rem` 5 :: Integer)
  ratio <- try (evaluate (1 % (0 :: Integer)))
  putStrLn (either (\e -> show (e :: ArithException)) show ratio)
  print (DivideByZero == DivideByZero, DivideByZero < RatioZeroDenominator, [Overflow, Underflow, LossOfPrecision, Denormal])
  handle (\e -> putStrLn ("caught " ++ show (e :: ArithException))) (print (5 `quot` (0 :: Int)))
  -- Something else thrown is not caught as arithmetic.
  other <- try (try (evaluate (error "not arithmetic" :: Int)))
  case other of
    Left (ErrorCall message) -> putStrLn ("an error: " ++ message)
    Right inner -> print (inner :: Either ArithException Int)
  -- Where no pattern matches.
  matched (evaluate (corners Square))
  matched (evaluate (corners Circle))
  matched (evaluate (firstOf ([] :: [Int])))
  matched (evaluate (case Just (3 :: Int) of Nothing -> 0))
  matched (evaluate (firstOf [5, 6]))
  -- And as SomeException.
  some <- try (evaluate (corners Circle))
  putStrLn (either (\(SomeException _) -> "something was thrown") show some)
