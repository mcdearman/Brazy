-- |
-- Small things for values in memory.
module Foreign.Marshal.Utils
  ( with,
    new,
    fromBool,
    toBool,
    maybeNew,
    maybeWith,
    maybePeek,
    withMany,
    copyBytes,
    moveBytes,
    fillBytes,
  )
where

import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (alloca, malloc)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..))

foreign import ccall unsafe "memcpy" c_memcpy :: Ptr a -> Ptr a -> Int -> IO ()

foreign import ccall unsafe "memmove" c_memmove :: Ptr a -> Ptr a -> Int -> IO ()

foreign import ccall unsafe "memset" c_memset :: Ptr a -> Int32 -> Int -> IO ()

-- | A value in memory for as long as the action runs.
with :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
with value action = alloca $ \address -> do
  poke address value
  action address

-- | A value in memory, until it is freed.
new :: (Storable a) => a -> IO (Ptr a)
new value = do
  address <- malloc
  poke address value
  pure address

fromBool :: (Num a) => Bool -> a
fromBool False = 0
fromBool True = 1

toBool :: (Eq a, Num a) => a -> Bool
toBool = (/= 0)

maybeNew :: (a -> IO (Ptr b)) -> Maybe a -> IO (Ptr b)
maybeNew = maybe (pure nullPtr)

maybeWith :: (a -> (Ptr b -> IO c) -> IO c) -> Maybe a -> (Ptr b -> IO c) -> IO c
maybeWith = maybe ($ nullPtr)

maybePeek :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybePeek peeking address
  | address == nullPtr = pure Nothing
  | otherwise = Just <$> peeking address

withMany :: (a -> (b -> res) -> res) -> [a] -> ([b] -> res) -> res
withMany _ [] action = action []
withMany withOne (x : xs) action =
  withOne x $ \x' -> withMany withOne xs (\xs' -> action (x' : xs'))

-- | So many bytes from the second to the first, which do not overlap.
copyBytes :: Ptr a -> Ptr a -> Int -> IO ()
copyBytes = c_memcpy

-- | So many bytes from the second to the first, which may overlap.
moveBytes :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes = c_memmove

fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes address byte = c_memset address (fromIntegral byte)
