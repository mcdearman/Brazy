module Main where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free, malloc, mallocBytes)
import Foreign.Marshal.Array (advancePtr, allocaArray, copyArray, mallocArray, newArray, newArray0, peekArray, peekArray0, pokeArray, withArrayLen)
import Foreign.Marshal.Utils (fillBytes, fromBool, new, toBool, with)
import Foreign.Ptr (Ptr, castPtr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))

-- Values in memory, through Foreign.Storable and Foreign.Marshal, and an
-- address given to C and given back.

foreign import ccall unsafe "memchr" findByte :: Ptr Word8 -> Int32 -> Int -> IO (Ptr Word8)

roundTrip :: (Storable a) => a -> IO a
roundTrip value = with value peek

main :: IO ()
main = do
  print =<< (sequence [fromIntegral <$> roundTrip (-5 :: Int8), fromIntegral <$> roundTrip (-30000 :: Int16), fromIntegral <$> roundTrip (-2000000000 :: Int32), fromIntegral <$> roundTrip (minBound :: Int64)] :: IO [Integer])
  print =<< (sequence [fromIntegral <$> roundTrip (250 :: Word8), fromIntegral <$> roundTrip (65000 :: Word16), fromIntegral <$> roundTrip (4000000000 :: Word32), fromIntegral <$> roundTrip (maxBound :: Word64)] :: IO [Integer])
  print =<< ((,,,) <$> roundTrip (123456789 :: Int) <*> roundTrip 'λ' <*> roundTrip True <*> roundTrip (2.5 :: Double))
  print =<< roundTrip (1.25 :: Float)
  print (sizeOf (0 :: Int), sizeOf 'c', sizeOf (0 :: Word16), alignment (0 :: Double), sizeOf nullPtr)
  -- Arrays.
  numbers <- newArray [1 .. 10 :: Int32]
  print =<< peekArray 10 numbers
  pokeArray (numbers `advancePtr` 2) [30, 40]
  print =<< peekElemOff numbers 3
  copies <- mallocArray 10 :: IO (Ptr Int32)
  copyArray copies numbers 10
  print =<< peekArray 5 copies
  print (copies `advancePtr` 3 `minusPtr` copies)
  free numbers
  free copies
  text <- newArray0 0 (map (fromIntegral . fromEnum) "hello" :: [Word8])
  print =<< (map (toEnum . fromIntegral) <$> peekArray0 0 text :: IO String)
  -- C finds a byte, and gives back an address.
  found <- findByte text (fromIntegral (fromEnum 'l')) 5
  print (found `minusPtr` text)
  missing <- findByte text (fromIntegral (fromEnum 'z')) 5
  print (missing == nullPtr)
  free text
  -- Bytes.
  allocaBytes 8 $ \bytes -> do
    fillBytes bytes 0xab 8
    print =<< (peek (castPtr bytes) :: IO Word64)
  allocaArray 3 $ \values -> do
    pokeArray values [1.5, 2.5, 3.5 :: Double]
    print . sum =<< peekArray 3 values
  withArrayLen "abc" $ \count values -> print . (,) count =<< peekArray count values
  boxed <- new (42 :: Int)
  modified <- peek boxed
  poke boxed (modified + 1)
  print =<< peek boxed
  free boxed
  cell <- malloc :: IO (Ptr Double)
  poke cell 0.5
  print =<< (peekByteOff (castPtr cell `plusPtr` 0 :: Ptr ()) 0 :: IO Double)
  free cell
  print (fromBool True :: Int, toBool (0 :: Int), toBool (7 :: Int32))
  alloca $ \place -> do
    poke place (nullPtr `plusPtr` 64 :: Ptr Int)
    print . (`minusPtr` nullPtr) =<< peek place
  raw <- mallocBytes 16
  pokeByteOff raw 8 (7 :: Int16)
  print =<< (peekByteOff raw 8 :: IO Int16)
  free (raw :: Ptr ())
