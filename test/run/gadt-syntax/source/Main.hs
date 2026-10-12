module Main where

-- An ordinary type, written with where.
data Shape where
  Circle :: Int -> Shape
  Box :: !Int -> Int -> Shape

area :: Shape -> Int
area (Circle r) = 3 * r * r
area (Box w h) = w * h

-- A type with a parameter, whose constructors make it at the parameter.
data Option a where
  None :: Option a
  Some :: a -> Option a

orElse :: Option a -> a -> a
orElse None d = d
orElse (Some x) _ = x

main :: IO ()
main = do
  print (area (Circle 2), area (Box 3 4))
  print (orElse (Some 5) 0, orElse None 'x')
