module Main where

import Control.Monad (forM, replicateM_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Array (peekArray, withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)
import Foreign.Storable (peek)

-- Functions of C made of functions of Haskell as the program runs, called
-- through their addresses, and by C's sort.

foreign import ccall "wrapper" wrapPure :: (CInt -> CInt) -> IO (FunPtr (CInt -> CInt))
foreign import ccall "dynamic" callPure :: FunPtr (CInt -> CInt) -> CInt -> CInt

foreign import ccall "wrapper" wrapAction :: (CInt -> IO CInt) -> IO (FunPtr (CInt -> IO CInt))
foreign import ccall "dynamic" callAction :: FunPtr (CInt -> IO CInt) -> CInt -> IO CInt

foreign import ccall "wrapper" wrapDone :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "dynamic" callDone :: FunPtr (IO ()) -> IO ()

foreign import ccall "wrapper"
  wrapCompare :: (Ptr CInt -> Ptr CInt -> IO CInt) -> IO (FunPtr (Ptr CInt -> Ptr CInt -> IO CInt))
foreign import ccall "qsort"
  qsort :: Ptr CInt -> CSize -> CSize -> FunPtr (Ptr CInt -> Ptr CInt -> IO CInt) -> IO ()

main :: IO ()
main = do
  -- Each holds a number of its own.
  scales <- forM [1 .. 5] $ \k -> wrapPure (* k)
  print [callPure f 10 | f <- scales]
  mapM_ freeHaskellFunPtr scales
  -- One that keeps a count.
  total <- newIORef 0
  add <- wrapAction $ \n -> do
    modifyIORef total (+ n)
    readIORef total
  mapM (callAction add) [1, 2, 3, 4] >>= print
  freeHaskellFunPtr add
  -- Made and freed many more times than there can be at once.
  replicateM_ 300 $ do
    f <- wrapPure (+ 1)
    callPure f 1 `seq` freeHaskellFunPtr f
  done <- wrapDone (putStrLn "done")
  callDone done
  freeHaskellFunPtr done
  -- C's sort, which calls back to compare.
  calls <- newIORef (0 :: Int)
  comparing <- wrapCompare $ \a b -> do
    modifyIORef calls (+ 1)
    x <- peek a
    y <- peek b
    pure (if x < y then -1 else if x > y then 1 else 0)
  sorted <- withArrayLen [5, 3, 9, 1, 7, 2, 8, 6, 4, 0] $ \n p -> do
    qsort p (fromIntegral n) 4 comparing
    peekArray n p
  print sorted
  readIORef calls >>= print . (> 0)
  freeHaskellFunPtr comparing
