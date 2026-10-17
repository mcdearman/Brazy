module Main where

import Control.Exception (IOException, catch)
import Data.Int (Int8)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar (..), CDouble (..), CInt (..))
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (peekElemOff)

-- Functions of Haskell that C calls by a name, called here through their
-- addresses, as C would call them.

triple :: CInt -> CInt
triple n = 3 * n

scale :: CDouble -> CInt -> CDouble
scale x n = x * fromIntegral n

greet :: CInt -> IO CInt
greet n = do
  putStrLn ("greet " ++ show n)
  pure (n + 1)

sum8 :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt
sum8 a b c d e f g h = a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g + 8 * h

negate8 :: Int8 -> Int8
negate8 = negate

positive :: CInt -> Bool
positive = (> 0)

second :: Ptr CChar -> IO CChar
second text = peekElemOff text 1

-- Calls back into Haskell from inside a call from C.
nested :: CInt -> IO CInt
nested n = do
  r <- callGreet greetAddress n
  pure (callTriple tripleAddress r)

-- What is thrown inside a call from C is caught there.
guarded :: CInt -> IO CInt
guarded n =
  ioError (userError ("bad " ++ show n))
    `catch` \e -> do
      putStrLn ("caught: " ++ show (e :: IOException))
      pure (n * 10)

done :: IO ()
done = putStrLn "done"

foreign export ccall triple :: CInt -> CInt
foreign export ccall "hazy_test_scale" scale :: CDouble -> CInt -> CDouble
foreign export ccall greet :: CInt -> IO CInt
foreign export ccall sum8 :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt
foreign export ccall negate8 :: Int8 -> Int8
foreign export ccall positive :: CInt -> Bool
foreign export ccall second :: Ptr CChar -> IO CChar
foreign export ccall nested :: CInt -> IO CInt
foreign export ccall guarded :: CInt -> IO CInt
foreign export ccall done :: IO ()

foreign import ccall "&triple" tripleAddress :: FunPtr (CInt -> CInt)
foreign import ccall "dynamic" callTriple :: FunPtr (CInt -> CInt) -> CInt -> CInt

foreign import ccall "&hazy_test_scale" scaleAddress :: FunPtr (CDouble -> CInt -> CDouble)
foreign import ccall "dynamic" callScale :: FunPtr (CDouble -> CInt -> CDouble) -> CDouble -> CInt -> CDouble

foreign import ccall "&greet" greetAddress :: FunPtr (CInt -> IO CInt)
foreign import ccall "dynamic" callGreet :: FunPtr (CInt -> IO CInt) -> CInt -> IO CInt

foreign import ccall "&sum8" sum8Address :: FunPtr (CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt)
foreign import ccall "dynamic" callSum8 :: FunPtr (CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt) -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt

foreign import ccall "&negate8" negate8Address :: FunPtr (Int8 -> Int8)
foreign import ccall "dynamic" callNegate8 :: FunPtr (Int8 -> Int8) -> Int8 -> Int8

foreign import ccall "&positive" positiveAddress :: FunPtr (CInt -> Bool)
foreign import ccall "dynamic" callPositive :: FunPtr (CInt -> Bool) -> CInt -> Bool

foreign import ccall "&second" secondAddress :: FunPtr (Ptr CChar -> IO CChar)
foreign import ccall "dynamic" callSecond :: FunPtr (Ptr CChar -> IO CChar) -> Ptr CChar -> IO CChar

foreign import ccall "&nested" nestedAddress :: FunPtr (CInt -> IO CInt)
foreign import ccall "&guarded" guardedAddress :: FunPtr (CInt -> IO CInt)

foreign import ccall "&done" doneAddress :: FunPtr (IO ())
foreign import ccall "dynamic" callDone :: FunPtr (IO ()) -> IO ()

main :: IO ()
main = do
  print (callTriple tripleAddress 14)
  print (callScale scaleAddress 1.5 4)
  callGreet greetAddress 41 >>= print
  print (callSum8 sum8Address 1 2 3 4 5 6 7 8)
  print (callNegate8 negate8Address 100, callNegate8 negate8Address (-128))
  print (callPositive positiveAddress 3, callPositive positiveAddress (-3))
  withCString "hazy" (callSecond secondAddress) >>= print
  callGreet nestedAddress 5 >>= print
  callGreet guardedAddress 3 >>= print
  callDone doneAddress
