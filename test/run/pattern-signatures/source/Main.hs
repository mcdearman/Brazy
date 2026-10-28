{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- A pattern that says its type, as ScopedTypeVariables has it.

-- A type that is known, which picks what a number is.
next (n :: Int) = n + 1

-- A type variable that a forall brought into scope.
pairUp :: forall a. Show a => a -> [String]
pairUp x = map (\(y :: a) -> show y) [x, x]

-- A type variable of its own: any type.
firstOf :: [b] -> b
firstOf ((x :: c) : _) = x

-- In a tuple, in a case, and named with @.
sizes :: (String, Int) -> Int
sizes (s :: String, n) = length s + n

describe :: Maybe Int -> String
describe value = case value of
  Just (n :: Int) -> "just " ++ show n
  whole@(Nothing :: Maybe Int) -> "nothing: " ++ show whole

main :: IO ()
main = do
  print (next 41, pairUp 'c', firstOf "xyz")
  print (sizes ("four", 2), describe (Just 3), describe Nothing)
  print ((\(s :: String) -> length s) "hello")
