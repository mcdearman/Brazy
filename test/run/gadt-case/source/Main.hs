module Main where

-- Matching on a constructor that says what type it makes, somewhere other
-- than at the parameters of a function: on a variable that was bound
-- further out, whose type is known from the function's signature.

data Value a where
  Number :: Int -> Value Int
  Truth :: Bool -> Value Bool
  Both :: Value a -> Value b -> Value (a, b)

-- In a case expression.
get :: Value a -> a
get value = case value of
  Number n -> n
  Truth b -> b
  Both x y -> (get x, get y)

-- Inside a lambda, and again further in.
describe :: Value a -> a -> String
describe value = \given -> case value of
  Number _ -> "number " ++ show (given + 1)
  Truth _ -> "truth " ++ show (not given)
  Both x _ -> case x of
    Number _ -> "pair of a number " ++ show (fst given * 2)
    _ -> "pair"

-- After another parameter, and where what the type is given holds types
-- that are the constructor's own.
second :: Int -> Value a -> a
second n (Number m) = n + m
second _ (Truth b) = b
second n (Both x y) = (second n x, second n y)

-- With an explicit forall, and a type that is hidden and used where it is.
data Shown where
  Shown :: forall a. a -> (a -> String) -> Shown

render :: Shown -> String
render shown = case shown of
  Shown a display -> display a

main :: IO ()
main = do
  print (get (Number 4), get (Truth True), get (Both (Number 1) (Truth False)))
  putStrLn (describe (Number 0) 41)
  putStrLn (describe (Truth True) False)
  putStrLn (describe (Both (Number 0) (Truth True)) (21, True))
  print (second 1 (Both (Number 1) (Both (Truth True) (Number 5))))
  putStrLn (render (Shown 'x' show) ++ render (Shown 7 show))
