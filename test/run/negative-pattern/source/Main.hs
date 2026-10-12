module Main where

f :: Int -> String
f (-1) = "minus one"
f 0 = "zero"
f _ = "other"

g :: Int -> String
g index = go (index - 1)
  where
    go (-1)
      | text <- "end" = text
    go n = "more " ++ show n

h :: Integer -> String
h = \case
  -2 -> "minus two"
  _ -> "other"

main :: IO ()
main = do
  print (map f [-1, 0, 1])
  print (map g [0, 1])
  print (map h [-2, 2])
  print (length ([] :: [Int]) - 1 == -1, f (length ([] :: [Int]) - 1))
