module Main where

import Control.Applicative (Const (..))
import Data.Bifoldable
import Data.Bifunctor (bimap, first, second)
import Data.Bitraversable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

-- Data.List.NonEmpty, Data.Bifunctor, Data.Bifoldable and Data.Bitraversable.

main :: IO ()
main = do
  let xs = 3 :| [1, 4, 1, 5, 9, 2, 6] :: NonEmpty Int
  print (NonEmpty.intersperse 0 xs, NonEmpty.scanl (+) 0 [1, 2, 3 :: Int], NonEmpty.scanr (+) 0 [1, 2, 3 :: Int])
  print (NonEmpty.scanl1 (+) xs, NonEmpty.scanr1 max xs)
  print (NonEmpty.transpose ((1 :| [2, 3]) :| [4 :| [5, 6 :: Int]]))
  print (NonEmpty.sortWith negate xs, NonEmpty.unfoldr (\n -> (n, if n < 3 then Just (n + 1) else Nothing)) (0 :: Int))
  print (NonEmpty.inits "abc", NonEmpty.tails "abc", NonEmpty.prependList [7, 8] xs)
  print (NonEmpty.take 4 (NonEmpty.iterate (* 2) (1 :: Int)), NonEmpty.take 3 (NonEmpty.repeat 'x'), NonEmpty.take 5 (NonEmpty.cycle ('a' :| "b")))
  print (NonEmpty.unfold (\n -> (n, if n < 2 then Just (n + 1) else Nothing)) (0 :: Int), NonEmpty.insert 4 [1, 3, 5, 7 :: Int])
  print (NonEmpty.some1 (Nothing :: Maybe Char), NonEmpty.some1 [] :: [NonEmpty Int])
  print (NonEmpty.take 3 xs, NonEmpty.drop 5 xs, NonEmpty.splitAt 2 xs)
  print (NonEmpty.takeWhile odd xs, NonEmpty.dropWhile odd xs, NonEmpty.span odd xs, NonEmpty.break even xs)
  print (NonEmpty.filter even xs, NonEmpty.partition (> 3) xs)
  print (NonEmpty.group "aabccc", NonEmpty.groupBy (\a b -> even a == even b) [2, 4, 1, 3, 6 :: Int])
  print (NonEmpty.groupWith (`div` 3) [1, 2, 3, 4, 7 :: Int], NonEmpty.groupAllWith (`mod` 3) [1, 2, 3, 4, 7 :: Int])
  print (NonEmpty.group1 ('a' :| "abb"), NonEmpty.groupBy1 (<=) (1 :| [2, 0, 3 :: Int]))
  print (NonEmpty.groupWith1 even (2 :| [4, 1 :: Int]), NonEmpty.groupAllWith1 even (2 :| [1, 4, 3 :: Int]))
  print (NonEmpty.isPrefixOf [3, 1] xs, NonEmpty.isPrefixOf [3, 2] xs, NonEmpty.isPrefixOf [] xs)
  print (NonEmpty.nub xs, NonEmpty.nubBy (\a b -> a `mod` 3 == b `mod` 3) xs, xs NonEmpty.!! 0, xs NonEmpty.!! 5)
  print (NonEmpty.unzip ((1, 'a') :| [(2, 'b') :: (Int, Char)]), NonEmpty.xor (True :| [True, True]), NonEmpty.xor (False :| [True, False]))
  -- Bifunctor.
  print (bimap (+ 1) show (1 :: Int, True), first negate (Left 3 :: Either Int Char), second succ (Right 'a' :: Either Int Char))
  print (bimap (* 2) not ('x', 3 :: Int, True), first length (Const "abc" :: Const String Int))
  print (second show (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int), bimap id id (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int, 5 :: Int))
  -- Bifoldable.
  print (bifold ("ab", "cd"), bifoldMap show show (1 :: Int, 'c'), bifoldr (:) (:) [] (1 :: Int, 2 :: Int))
  print (bifoldl (flip (:)) (flip (:)) [] (1 :: Int, 2 :: Int), bifoldr' (+) (+) 0 (Left 4 :: Either Int Int))
  print (bifoldr1 (-) (10 :: Int, 3 :: Int), bifoldl1 (-) (10 :: Int, 3 :: Int), bifoldl' (+) (+) 1 ('x', 2 :: Int, 3 :: Int))
  bifoldrM (\a c -> print a >> return (a + c)) (\b c -> print b >> return (b + c)) 0 (1 :: Int, 2 :: Int) >>= print
  bifoldlM (\c a -> print a >> return (a + c)) (\c b -> print b >> return (b + c)) 0 (1 :: Int, 2 :: Int) >>= print
  bitraverse_ print print (1 :: Int, "two")
  bifor_ (Right 'z' :: Either Int Char) print print
  bisequenceA_ (putStrLn "left", putStrLn "right")
  print (biasum (Nothing, Just 'j'), bimsum ([1, 2], [3 :: Int]), biList (5 :: Int, 6), binull (Left 1 :: Either Int Int))
  print (bilength (1 :: Int, 'c'), bilength (Const 1 :: Const Int Char), bielem 2 (1, 2 :: Int), binotElem 2 (1, 2 :: Int))
  print (bimaximum (3, 7 :: Int), biminimum (3, 7 :: Int), bisum (3, 7 :: Int), biproduct (3, 7 :: Int))
  print (biconcat ([1], [2, 3 :: Int]), biconcatMap show (replicate 2) (1 :: Int, 'c'), biand (True, False), bior (True, False))
  print (biany even odd (1 :: Int, 2 :: Int), biall odd even (1 :: Int, 2 :: Int), bimaximumBy compare (4, 4 :: Int), biminimumBy compare (2, 1 :: Int))
  print (bifind even (3, 4 :: Int), bifind even (Left 3 :: Either Int Int))
  -- Bitraversable.
  print (bitraverse Just (\x -> if x > 0 then Just x else Nothing) (1 :: Int, 2 :: Int), bisequenceA (Just 'a', Nothing :: Maybe Int))
  r <- bimapM (\a -> print a >> return (a * 2)) (\b -> print b >> return (show b)) (Left 21 :: Either Int Int)
  print r
  print (bisequence ([1, 2], "ab" :: String) :: [(Int, Char)], bifor ('c', 3 :: Int, 4 :: Int) (\x -> [x, x + 1]) (\x -> [x]))
  print (bimapAccumL (\s a -> (s + a, a * 10)) (\s b -> (s * b, show s)) 1 (2 :: Int, 3 :: Int))
  print (bimapAccumR (\s a -> (s + a, a * 10)) (\s b -> (s * b, show s)) 1 (2 :: Int, 3 :: Int))
  print (bimapDefault (+ 1) not (1 :: Int, False), bifoldMapDefault show (: []) (1 :: Int, 'c'))
  print (bitraverse (\a -> [a, a + 1]) pure (Const 5 :: Const Int Bool))
