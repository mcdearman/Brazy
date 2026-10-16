module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Bool (bool)
import Data.Either
import Data.Foldable
import Data.Functor
import Data.List
import Data.Ratio
import Data.STRef
import Data.Semigroup
import Data.Traversable
import Data.Tuple (swap)
import System.Environment
import System.IO
import System.IO.Unsafe (unsafeFixIO)
import Text.Show (showListWith)

-- What the standard modules give, used through all of them at once.

main :: IO ()
main = do
  -- Data.List.
  print (subsequences [1, 2, 3 :: Int])
  print (permutations [1, 2, 3 :: Int])
  print (scanl (+) 0 [1, 2, 3 :: Int], scanl1 max [3, 1, 4 :: Int], scanr (+) 0 [1, 2, 3 :: Int], scanr1 (+) [1, 2, 3 :: Int])
  print (take 4 (scanl' (+) 0 [1 ..]) :: [Int], take 3 (iterate' (* 2) 1) :: [Int])
  print (isSubsequenceOf "ace" "abcde", isSubsequenceOf "aec" "abcde")
  print (elemIndices 'a' "banana", findIndices even [1, 2, 3, 4 :: Int])
  print (zip5 [1 :: Int, 2] "ab" [True, False] [(), ()] "xy")
  print (zipWith6 (\a b c d e f -> a + b + c + d + e + f) [1] [2] [3] [4] [5] [6 :: Int])
  print (unzip4 [(1 :: Int, 'a', True, "x"), (2, 'b', False, "y")])
  print (deleteFirstsBy (==) [1, 2, 3, 2, 1 :: Int] [2, 1])
  print (genericTake (2 :: Integer) "abc", genericDrop (1 :: Integer) "abc", genericSplitAt (1 :: Integer) "abc")
  print (genericIndex "abc" (2 :: Integer), genericReplicate (3 :: Integer) 'x', genericLength "four" :: Integer)
  print (mapAccumL (\s x -> (s + x, s * x)) 0 [1, 2, 3 :: Int], mapAccumR (\s x -> (s + x, s * x)) 0 [1, 2, 3 :: Int])
  print (notElem 3 [1, 2 :: Int], sort [3, 1, 2 :: Int], nub [1, 1, 2 :: Int])
  -- Control.Monad and Data.Foldable.
  print (mfilter even (Just (4 :: Int)), mfilter even (Just (3 :: Int)), msum [Nothing, Just 'a', Just 'b'])
  print (liftM4 (\a b c d -> a + b + c + d) (Just 1) (Just 2) (Just 3) (Just (4 :: Int)))
  print (liftM5 (\a b c d e -> [a, b, c, d, e]) "a" "b" "c" "d" "e")
  forM_ [1, 2 :: Int] print
  xs <- forM [1, 2 :: Int] (\x -> pure (x * 10))
  print xs
  sequence_ [putStr "a", putStr "b", putStrLn "c"]
  void (pure 'x')
  -- Data.Functor, Data.Either, Data.Tuple, Data.Bool.
  print (Just 1 $> 'x', Just (2 :: Int) <&> (+ 1), void [1, 2 :: Int])
  let eithers = [Left 'a', Right (1 :: Int), Left 'b', Right 2]
  print (lefts eithers, rights eithers, partitionEithers eithers)
  print (isLeft (Left 'a' :: Either Char ()), isRight (Left 'a' :: Either Char ()), fromLeft 'z' (Right () :: Either Char ()), fromRight 0 (Right (5 :: Int) :: Either () Int))
  print (swap (1 :: Int, 'a'), bool "no" "yes" True, bool "no" "yes" False)
  -- Data.Ratio, Text.Show, Data.Semigroup.
  print (approxRational (pi :: Double) 0.001, approxRational (0.333 :: Double) 0.01, approxRational (-1.5 :: Double) 0.1)
  putStrLn (showListWith shows [1, 2, 3 :: Int] "")
  print (stimesMonoid (3 :: Int) "ab", stimesIdempotent (5 :: Int) (Max (3 :: Int)), mtimesDefault (2 :: Int) [True])
  print (appEndo (diff [1 :: Int]) [2], take 7 (cycle1 "abc"))
  -- Control.Arrow.
  print (((+ 1) ^>> (* 2)) (3 :: Int), ((* 2) >>^ (+ 1)) (3 :: Int), returnA 'r', ((+ 1) <<^ (* 2)) (3 :: Int), ((* 2) ^<< (+ 1)) (3 :: Int))
  print (leftApp (+ (1 :: Int)) (Left 1 :: Either Int Char), leftApp (+ (1 :: Int)) (Right 'c' :: Either Int Char))
  -- Fixed points of actions.
  ones <- fixIO (\xs -> pure (1 : take 3 xs))
  print (ones :: [Int])
  twos <- unsafeFixIO (\xs -> pure (2 : take 2 xs))
  print (twos :: [Int])
  print (runST (fixST (\xs -> pure (3 : take 2 xs))) :: [Int])
  print (runST (do ref <- newSTRef (0 :: Int); modifySTRef ref (+ 5); readSTRef ref))
  -- The environment.
  setEnv "BRAZY_LIBRARY_TEST" "set"
  lookupEnv "BRAZY_LIBRARY_TEST" >>= print
  environment <- getEnvironment
  print (lookup "BRAZY_LIBRARY_TEST" environment)
  unsetEnv "BRAZY_LIBRARY_TEST"
  lookupEnv "BRAZY_LIBRARY_TEST" >>= print
  withArgs ["one", "two"] getArgs >>= print
  withProgName "named" getProgName >>= putStrLn
  hFlush stdout
