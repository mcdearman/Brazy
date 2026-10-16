module Main where

import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8)

-- Functions of C given and giving numbers of a fixed width, and a Bool,
-- each called with the type C has for it: a function of C called with
-- another type is wrong in C as well, and on Windows a small number is
-- not widened for it.

foreign import ccall "abs" abs32 :: Int32 -> Int32

foreign import ccall "llabs" abs64 :: Int64 -> Int64

foreign import ccall "isdigit" digit :: Int32 -> Bool

main :: IO ()
main = do
  print (abs32 (-5), abs64 (-9000000000000))
  print (map (digit . fromIntegral . fromEnum) "a1 9")
  print (abs32 (minBound + 1), (maxBound :: Int16) + 1, (200 :: Word8) * 2)
