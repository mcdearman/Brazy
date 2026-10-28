module Main where

import Control.Applicative (Alternative (..))
import Control.Monad (mplus, mzero)
import qualified Data.Functor.Identity as I
import Data.Monoid (Sum (..))

-- Instances of the classes everyone uses, for the types everyone uses, that
-- GHC has and Hazy lacked.

main :: IO ()
main = do
  -- Maybe as a monoid of what it holds.
  print (Just [1] <> Nothing <> Just [2 :: Int], mempty :: Maybe [Int], mconcat [Just [1 :: Int], Nothing, Just [3]])
  print (foldMap (\x -> if x > 2 then Just (Sum x) else Nothing) [1 .. 5 :: Int])
  -- Either as a semigroup: the first Right.
  print (Left 1 <> Right 'a' <> (Right 'b' :: Either Int Char), Left 1 <> (Left 2 :: Either Int Char))
  -- A pair as a writer.
  print (("a", (+ 1)) <*> ("b", 2 :: Int), ("x", 1 :: Int) >>= \n -> ("y", n + 1), pure 5 :: (String, Int))
  print (traverse (\x -> ([x], x * 2)) [1, 2, 3 :: Int])
  -- A triple as a functor of what it holds last.
  print (fmap (+ 1) ('a', True, 3 :: Int), (* 2) <$> ("x", 'y', 4 :: Int))
  -- Identity shown as GHC shows it.
  print (I.Identity 3 :: I.Identity Int, fmap show (I.Identity True), [I.Identity 'a'], Just (I.Identity (-2 :: Int)))
  -- IO as an Alternative: an error of input or output gives way.
  first <- (fail "no" :: IO Int) <|> pure 3
  second <- (ioError (userError "no") :: IO Int) `mplus` pure 4
  third <- pure 5 <|> (mzero :: IO Int)
  print (first, second, third)
