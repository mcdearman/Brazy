module Main where

import Data.Functor.Identity (Identity (..))
import qualified Data.Vector as V

-- What was left of Data.Vector.

main :: IO ()
main = do
  let v = V.fromList [3, 1, 4, 1, 5, 9, 2, 6 :: Int]
  print (runIdentity (V.iterateNM 4 (pure . (* 2)) 1), V.unfoldrExactN 3 (\s -> (s, s + 10)) 1)
  print (runIdentity (V.unfoldrM (\s -> pure (if s > 3 then Nothing else Just (s, s + 1))) 0), runIdentity (V.unfoldrNM 2 (\s -> pure (Just (s, s * 3))) 1))
  print (runIdentity (V.unfoldrExactNM 3 (\s -> pure (s, s - 1)) 9))
  print (V.constructN 5 (\u -> if V.null u then 1 else V.sum u), V.constructrN 4 (\u -> V.length u * 10))
  print (V.update_ v (V.fromList [0, 2]) (V.fromList [30, 40]), V.accumulate_ (+) v (V.fromList [1, 1]) (V.fromList [100, 1000]))
  print (V.izipWith4 (\i a b c d -> i + a + b + c + d) v v v v, V.zip5 v v v v v V.! 2)
  print (V.unzip4 (V.fromList [(1, 'a', True, "x"), (2, 'b', False, "y" :: String)]))
  print (runIdentity (V.mapMaybeM (\x -> pure (if odd x then Just (x * 10) else Nothing)) v), runIdentity (V.imapMaybeM (\i x -> pure (if even i then Just (i, x) else Nothing)) v))
  print (V.partitionWith (\x -> if even x then Left x else Right (show x)) v)
  print (V.groupBy (\a b -> even a == even b) v, V.group (V.fromList "aabccc"))
  print (V.maximumOn negate v, V.minimumOn negate v, V.maxIndex v, V.minIndex v, V.maxIndexBy (\a b -> compare (a `mod` 3) (b `mod` 3)) v, V.minIndexBy compare v)
  print (V.maximumOn (`mod` 3) v, V.minimumOn (`mod` 3) v)
  print (runIdentity (V.fold1M (\a b -> pure (a * 10 + b)) v))
  print (V.prescanl (+) 0 v, V.postscanl (+) 0 v, V.iscanl (\i a b -> a + i * b) 0 v)
  print (V.prescanr (+) 0 v, V.postscanr (+) 0 v, V.iscanr (\i a b -> b + i * a) 0 v)
  print (V.prescanl' (+) 0 v == V.prescanl (+) 0 v, V.postscanr' (+) 0 v == V.postscanr (+) 0 v)
