module Main where

import Foreign.C.Types (CDouble (..), CLLong (..))
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castFunPtrToPtr, nullFunPtr, nullPtr)

-- The addresses of functions of C, and functions of C called by their
-- addresses.

foreign import ccall "&sqrt" sqrtAddress :: FunPtr (CDouble -> CDouble)

foreign import ccall "& llabs" absAddress :: FunPtr (CLLong -> CLLong)

foreign import ccall "dynamic" callDouble :: FunPtr (CDouble -> CDouble) -> CDouble -> CDouble

foreign import ccall "dynamic" callLong :: FunPtr (CLLong -> IO CLLong) -> CLLong -> IO CLLong

foreign import ccall "dynamic" release :: FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()

main :: IO ()
main = do
  print (callDouble sqrtAddress 81)
  print =<< callLong (castFunPtr absAddress) (-5)
  print (sqrtAddress == sqrtAddress, sqrtAddress /= nullFunPtr, castFunPtrToPtr finalizerFree /= nullPtr)
  memory <- mallocBytes 32 :: IO (Ptr ())
  release finalizerFree memory
  putStrLn "freed"
