module Main where

import Control.Exception (Exception, evaluate, throw, try)
import Data.Void (Void, absurd)

data Nothing'

data Failure = Failure String
  deriving (Show)

instance Exception Failure

never :: Nothing' -> a
never = \case {}

neither :: Nothing' -> Int
neither nothing = case nothing of {}

main :: IO ()
main = do
  -- A case with no alternatives evaluates what it is a case of.
  first <- try (evaluate (never (throw (Failure "lambda case")) :: Int))
  print (first :: Either Failure Int)
  second <- try (evaluate (neither (throw (Failure "case"))))
  print (second :: Either Failure Int)
  third <- try (evaluate (absurd (throw (Failure "absurd") :: Void) :: Int))
  print (third :: Either Failure Int)
