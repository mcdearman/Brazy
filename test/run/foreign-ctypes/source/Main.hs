module Main where

import Data.Bits (complement, popCount, shiftL, (.&.))
import Foreign.C.Types (CChar (..), CDouble (..), CFloat (..), CInt (..), CLong (..), CSize (..), CUInt (..))
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek, sizeOf)

-- C's types as newtypes: shown, read and counted as what they hold, with
-- their constructors to match.

foreign import ccall unsafe "abs" c_abs :: CInt -> CInt

foreign import ccall unsafe "floor" c_floor :: CDouble -> CDouble

unwrap :: CInt -> Int
unwrap (CInt n) = fromIntegral n

main :: IO ()
main = do
  print (CInt 5, show (CDouble 2.5), read "-7" :: CInt, [CChar 1 .. CChar 4])
  print ((maxBound :: CInt) + 1, (0 :: CUInt) - 1, complement (0 :: CUInt), popCount (255 :: CSize), (1 :: CLong) `shiftL` 40)
  print (c_abs (-12), unwrap (c_abs (-3)), c_floor 2.75, (7 :: CInt) `div` 2, toInteger (CSize 9), (0xF0 :: CUInt) .&. 0x3C)
  print (sizeOf (undefined :: CInt), sizeOf (undefined :: CChar), sizeOf (undefined :: CDouble), sizeOf (undefined :: CLong))
  print =<< with (CInt 42) peek
  print (realToFrac (CFloat 1.5) :: Double, truncate (CDouble 3.9) :: Int, isNaN (CDouble (0 / 0)), sqrt (CDouble 16))
