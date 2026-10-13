module Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef,
    modifyIORef',
  )
where

import Data.STRef (STRef, modifySTRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Hazy.Prelude (IO (..), RealWorld)

newtype IORef a = IORef (STRef RealWorld a)
  deriving (Eq)

newIORef :: a -> IO (IORef a)
newIORef value = IO (fmap IORef (newSTRef value))

readIORef :: IORef a -> IO a
readIORef (IORef reference) = IO (readSTRef reference)

writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef reference) value = IO (writeSTRef reference value)

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef (IORef reference) f = IO (modifySTRef reference f)

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' (IORef reference) f = IO (modifySTRef' reference f)
