module Main where

import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8)

-- Functions of C given and giving numbers of a fixed width, and a Bool.

foreign import ccall "abs" abs32 :: Int32 -> Int32

foreign import ccall "abs" abs16 :: Int16 -> Int16

foreign import ccall "llabs" abs64 :: Int64 -> Int64

foreign import ccall "toupper" upper8 :: Word8 -> Word8

foreign import ccall "isdigit" digit :: Int32 -> Bool

main :: IO ()
main = do
  print (abs32 (-5), abs16 (-300), abs64 (-9000000000000))
  print (map upper8 [97, 98, 122])
  print (map (digit . fromIntegral . fromEnum) "a1 9")
  print (abs32 (minBound + 1), (maxBound :: Int16) + 1, (200 :: Word8) * 2)
