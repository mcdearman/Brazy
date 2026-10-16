module Main where

import Data.Foldable (toList)

-- A pair is a Functor, Foldable and Traversable of what it holds second;
-- lists and NonEmpty are Foldable and Traversable, through classes the
-- compiler has built in.

main :: IO ()
main = do
  print (fmap (+ 1) ('a', 1 :: Int), toList ("x", True), sum ('s', 5 :: Int), length ((), 'q'))
  print (traverse (\x -> Just (x * 2)) ("label", 21 :: Int), sequenceA ('k', [1, 2 :: Int]))
  print (foldr (:) [] (1 :: Int, 'z'), null (1 :: Int, ()), elem 3 ('e', 3 :: Int), maximum (0 :: Int, 'm'))
  print (sum [1 .. 10 :: Int], product [1 .. 5 :: Integer], maximum "hello", minimum [3, 1, 2 :: Int])
  print (foldl (-) 100 [1, 2, 3 :: Int], foldr1 (-) [10, 3, 2 :: Int], foldl1 (-) [10, 3, 2 :: Int], null [], length "four")
  print (traverse (\x -> if x > 0 then Right x else Left x) [1, 2, 3 :: Int], sequence [Just 'a', Just 'b'])
  mapM_ print (Just 'm')
  r <- mapM (\x -> pure (x + 1)) [1, 2, 3 :: Int]
  print (r, concatMap show [1, 2 :: Int], and [True, False], any odd [2, 4 :: Int], elem 'l' "hello")
