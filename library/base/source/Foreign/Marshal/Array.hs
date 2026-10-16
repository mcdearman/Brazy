-- |
-- Values one after another in memory.
module Foreign.Marshal.Array
  ( mallocArray,
    mallocArray0,
    allocaArray,
    allocaArray0,
    reallocArray,
    reallocArray0,
    callocArray,
    callocArray0,
    peekArray,
    peekArray0,
    pokeArray,
    pokeArray0,
    newArray,
    newArray0,
    withArray,
    withArray0,
    withArrayLen,
    withArrayLen0,
    copyArray,
    moveArray,
    lengthArray0,
    advancePtr,
  )
where

import Foreign.Marshal.Alloc (allocaBytesAligned, callocBytes, mallocBytes, reallocBytes)
import Foreign.Marshal.Utils (copyBytes, moveBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))

-- The type of what an address is of, for its size; it is never looked at.
pointee :: IO (Ptr a) -> a
pointee _ = undefined

element :: Ptr a -> a
element _ = undefined

mallocArray :: (Storable a) => Int -> IO (Ptr a)
mallocArray count = result
  where
    result = mallocBytes (count * sizeOf (pointee result))

mallocArray0 :: (Storable a) => Int -> IO (Ptr a)
mallocArray0 count = mallocArray (count + 1)

callocArray :: (Storable a) => Int -> IO (Ptr a)
callocArray count = result
  where
    result = callocBytes (count * sizeOf (pointee result))

callocArray0 :: (Storable a) => Int -> IO (Ptr a)
callocArray0 count = callocArray (count + 1)

allocaArray :: (Storable a) => Int -> (Ptr a -> IO b) -> IO b
allocaArray count action = allocaBytesAligned (count * sizeOf value) (alignment value) action
  where
    value = parameter action
    parameter :: (Ptr a -> IO b) -> a
    parameter _ = undefined

allocaArray0 :: (Storable a) => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 count = allocaArray (count + 1)

reallocArray :: (Storable a) => Ptr a -> Int -> IO (Ptr a)
reallocArray address count = castPtr <$> reallocBytes address (count * sizeOf (element address))

reallocArray0 :: (Storable a) => Ptr a -> Int -> IO (Ptr a)
reallocArray0 address count = reallocArray address (count + 1)

peekArray :: (Storable a) => Int -> Ptr a -> IO [a]
peekArray count address
  | count <= 0 = pure []
  | otherwise = go (count - 1) []
  where
    go 0 values = (: values) <$> peekElemOff address 0
    go index values = do
      value <- peekElemOff address index
      go (index - 1) (value : values)

-- | The values up to the first that is the one given.
peekArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO [a]
peekArray0 end address = do
  count <- lengthArray0 end address
  peekArray count address

pokeArray :: (Storable a) => Ptr a -> [a] -> IO ()
pokeArray address values = sequence_ [pokeElemOff address index value | (index, value) <- zip [0 ..] values]

pokeArray0 :: (Storable a) => a -> Ptr a -> [a] -> IO ()
pokeArray0 end address values = do
  pokeArray address values
  pokeElemOff address (length values) end

newArray :: (Storable a) => [a] -> IO (Ptr a)
newArray values = do
  address <- mallocArray (length values)
  pokeArray address values
  pure address

newArray0 :: (Storable a) => a -> [a] -> IO (Ptr a)
newArray0 end values = do
  address <- mallocArray0 (length values)
  pokeArray0 end address values
  pure address

withArray :: (Storable a) => [a] -> (Ptr a -> IO b) -> IO b
withArray values action = withArrayLen values (const action)

withArray0 :: (Storable a) => a -> [a] -> (Ptr a -> IO b) -> IO b
withArray0 end values action = withArrayLen0 end values (const action)

withArrayLen :: (Storable a) => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen values action =
  allocaArray count $ \address -> do
    pokeArray address values
    action count address
  where
    count = length values

withArrayLen0 :: (Storable a) => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen0 end values action =
  allocaArray0 count $ \address -> do
    pokeArray0 end address values
    action count address
  where
    count = length values

-- | So many values from the second to the first, which do not overlap.
copyArray :: (Storable a) => Ptr a -> Ptr a -> Int -> IO ()
copyArray target source count = copyBytes target source (count * sizeOf (element target))

-- | So many values from the second to the first, which may overlap.
moveArray :: (Storable a) => Ptr a -> Ptr a -> Int -> IO ()
moveArray target source count = moveBytes target source (count * sizeOf (element target))

-- | How many values there are before the one given.
lengthArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO Int
lengthArray0 end address = go 0
  where
    go index = do
      value <- peekElemOff address index
      if value == end then pure index else go (index + 1)

advancePtr :: (Storable a) => Ptr a -> Int -> Ptr a
advancePtr address count = address `plusPtr` (count * sizeOf (element address))
