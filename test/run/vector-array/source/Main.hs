module Main where

import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict

-- A vector is an array of the runtime's: one longer than a header can
-- count fields of is as good as a short one.
long :: Vector.Vector Int
long = Vector.generate 20000 (* 3)

main :: IO ()
main = do
  print (Vector.length long, long Vector.! 0, long Vector.! 19999, Vector.sum long)
  -- What is not asked for is not evaluated.
  let lazy = Vector.fromList [1, undefined, 3 :: Int]
  print (lazy Vector.! 0 + lazy Vector.! 2)
  print (Vector.length (Vector.map (`div` 0) lazy))
  -- A strict vector evaluates what it holds, and only once.
  let strict = Strict.generate 5 (\index -> index * index)
  print (Strict.toList strict, Strict.sum strict)
  -- Changes make a new vector, and leave the old one as it was.
  let changed = long Vector.// [(0, -1), (19999, -2)]
  print (Vector.head changed, Vector.last changed, Vector.head long, Vector.last long)
  print (Vector.toList (Vector.accum (+) (Vector.replicate 3 10) [(0, 1), (2, 5), (0, 1)]))
  print (Vector.toList (Vector.imap (+) (Vector.fromList [10, 20, 30 :: Int])))
  print (Vector.foldr (:) [] (Vector.fromList "array"))
  print (Vector.toList (Vector.empty :: Vector.Vector Int), Vector.toList (Vector.singleton 'x'))
