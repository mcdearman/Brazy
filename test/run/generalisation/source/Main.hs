module Main where

-- Functions with no signature whose types are constrained are generalised,
-- as the report has it: at the top, in a where and in a let, and when they
-- call themselves through another. A pattern binding is not.

square x = x * x

describe x = "<" ++ show x ++ ">"

total xs = foldr (+) 0 xs

pingCount 0 = 0
pingCount n = 1 + pongCount (n - 1)

pongCount 0 = 0
pongCount n = 1 + pingCount (n - 1)

plus = (+)

main :: IO ()
main = do
  print (square (3 :: Int), square (1.5 :: Double))
  putStrLn (describe True ++ describe 'c')
  print (total [1, 2, 3 :: Int], total [0.5, 0.25 :: Double])
  let double x = x + x
      shown x = show x ++ "!"
  print (double (2 :: Int), double (0.5 :: Double), shown 'a', shown [True])
  print (pingCount (5 :: Int), pingCount (4 :: Integer))
  print (plus 1 2, sumWith 10)
  where
    sumWith k = (go (k :: Int), go (fromIntegral k :: Double))
      where
        go m = m * 2
