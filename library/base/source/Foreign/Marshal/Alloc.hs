-- |
-- Memory, as C's library gives it out and takes it back.
--
-- What is had for a while, by 'alloca' and its kin, is given back when the
-- action is done, whether it gives or throws.
module Foreign.Marshal.Alloc
  ( alloca,
    allocaBytes,
    allocaBytesAligned,
    malloc,
    mallocBytes,
    calloc,
    callocBytes,
    realloc,
    reallocBytes,
    free,
    finalizerFree,
  )
where

import Control.Exception (bracket)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))

foreign import ccall unsafe "malloc" c_malloc :: Int -> IO (Ptr a)

foreign import ccall unsafe "calloc" c_calloc :: Int -> Int -> IO (Ptr a)

foreign import ccall unsafe "realloc" c_realloc :: Ptr a -> Int -> IO (Ptr b)

foreign import ccall unsafe "free" c_free :: Ptr a -> IO ()

-- | The address of C's free, for what frees memory to be called with.
foreign import ccall unsafe "&free" finalizerFree :: FunPtr (Ptr a -> IO ())

-- The type of what an address is of, for its size; it is never looked at.
pointee :: IO (Ptr a) -> a
pointee _ = undefined

-- | Memory for a value of the type, as long as the action runs.
alloca :: (Storable a) => (Ptr a -> IO b) -> IO b
alloca action = result
  where
    result = allocaBytesAligned (sizeOf value) (alignment value) action
    value = parameter action result
    parameter :: (Ptr a -> IO b) -> IO b -> a
    parameter _ _ = undefined

allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes size = bracket (mallocBytes size) free

-- | malloc gives out memory aligned for any value, which is all an
-- alignment can ask.
allocaBytesAligned :: Int -> Int -> (Ptr a -> IO b) -> IO b
allocaBytesAligned size _ = allocaBytes size

malloc :: (Storable a) => IO (Ptr a)
malloc = result
  where
    result = mallocBytes (sizeOf (pointee result))

mallocBytes :: Int -> IO (Ptr a)
mallocBytes size = c_malloc size >>= failed "mallocBytes"

calloc :: (Storable a) => IO (Ptr a)
calloc = result
  where
    result = callocBytes (sizeOf (pointee result))

callocBytes :: Int -> IO (Ptr a)
callocBytes size = c_calloc 1 size >>= failed "callocBytes"

realloc :: (Storable b) => Ptr a -> IO (Ptr b)
realloc address = result
  where
    result = castPtr <$> reallocBytes address (sizeOf (pointee result))

reallocBytes :: Ptr a -> Int -> IO (Ptr a)
reallocBytes address 0 = nullPtr <$ free address
reallocBytes address size = c_realloc address size >>= failed "reallocBytes"

free :: Ptr a -> IO ()
free = c_free

-- What C's library gives when it has no memory to give.
failed :: String -> Ptr a -> IO (Ptr a)
failed name address
  | address == nullPtr = ioError (userError ("Foreign.Marshal.Alloc." ++ name ++ ": out of memory"))
  | otherwise = pure address
