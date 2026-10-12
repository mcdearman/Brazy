module Main where

-- Two patterns side by side that each tell something of the same type: the
-- second is read by what the first said, so that what they hide is known
-- to be the same.

data Layer f a where
  Flat :: f a -> Layer f a
  Deep :: Layer f a -> Layer f [a]

depth :: (Layer f a, Layer f a) -> Int
depth (Deep left, Deep right) = 1 + depth (left, right)
depth _ = 0

-- A type the constructor's index settles is not hidden at all: what is
-- made of it may be seen from outside the match.
inner :: Layer f [a] -> Maybe (Layer f a)
inner layer = case layer of
  Deep under -> Just under
  Flat _ -> Nothing

unwrap :: [Layer f [a]] -> [Layer f a]
unwrap layers = [under | Deep under <- layers]

-- What is required of a type variable is had of the type a match says the
-- variable is.
data Is a where
  IsInt :: Is Int
  IsBool :: Is Bool

class Named a where
  name :: a -> String

instance Named Bool where
  name b = if b then "yes" else "no"

describe :: (Show a) => Is a -> a -> String
describe IsInt n = show (n + 1)
describe IsBool b = name (not b)

main :: IO ()
main = do
  print (depth (Deep (Deep (Flat "x")), Deep (Deep (Flat "y"))), depth (Flat (Just "x"), Deep (Flat (Just 'y'))))
  print (maybe 0 (const 1) (inner (Deep (Flat (Just 'x')))) + length (unwrap [Deep (Flat "a"), Flat ["b"], Deep (Flat "c")]))
  putStrLn (describe IsInt 41 ++ " " ++ describe IsBool False)
