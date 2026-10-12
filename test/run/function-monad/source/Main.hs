module Main where

import Control.Monad.Fix (mfix)
import Data.Functor.Identity (Identity (..))

-- Functions from one type are a functor and a monad: each is given the
-- same thing.

both :: Int -> (Int, Int)
both = do
  a <- (+ 1)
  b <- (* 2)
  pure (a, b)

-- A monad tied in a knot: what the action gives is what it is given.
ones :: Identity [Int]
ones = mfix (\rest -> Identity (1 : rest))

main :: IO ()
main = do
  print (both 10)
  print (fmap (+ 1) (* 3) (4 :: Int))
  print (((,) <$> succ <*> pred) 'm')
  print (take 3 (runIdentity ones))
  print (fmap (take 2) (mfix (\rest -> Just ('a' : rest))))
