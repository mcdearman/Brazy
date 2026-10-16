-- |
-- Values that can be kept in memory, each in so many bytes. They are read
-- and written by the runtime, through functions of C it has for each
-- width.
module Foreign.Storable
  ( Storable (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (FunPtr, Ptr, castPtr, plusPtr)

class Storable a where
  sizeOf :: a -> Int
  alignment :: a -> Int

  peekElemOff :: Ptr a -> Int -> IO a
  peekElemOff address index = result
    where
      result = peekByteOff address (index * sizeOf (inside result))

  pokeElemOff :: Ptr a -> Int -> a -> IO ()
  pokeElemOff address index value = pokeByteOff address (index * sizeOf value) value

  peekByteOff :: Ptr b -> Int -> IO a
  peekByteOff address offset = peek (address `plusPtr` offset)

  pokeByteOff :: Ptr b -> Int -> a -> IO ()
  pokeByteOff address offset = poke (address `plusPtr` offset)

  peek :: Ptr a -> IO a
  peek address = peekElemOff address 0

  poke :: Ptr a -> a -> IO ()
  poke address = pokeElemOff address 0

-- The type of what an action gives, for its size; it is never looked at.
inside :: IO a -> a
inside _ = undefined

foreign import ccall unsafe "hazy_peek_i8" peekInt8 :: Ptr Int8 -> IO Int8

foreign import ccall unsafe "hazy_poke_i8" pokeInt8 :: Ptr Int8 -> Int8 -> IO ()

foreign import ccall unsafe "hazy_peek_u8" peekWord8 :: Ptr Word8 -> IO Word8

foreign import ccall unsafe "hazy_poke_u8" pokeWord8 :: Ptr Word8 -> Word8 -> IO ()

foreign import ccall unsafe "hazy_peek_i16" peekInt16 :: Ptr Int16 -> IO Int16

foreign import ccall unsafe "hazy_poke_i16" pokeInt16 :: Ptr Int16 -> Int16 -> IO ()

foreign import ccall unsafe "hazy_peek_u16" peekWord16 :: Ptr Word16 -> IO Word16

foreign import ccall unsafe "hazy_poke_u16" pokeWord16 :: Ptr Word16 -> Word16 -> IO ()

foreign import ccall unsafe "hazy_peek_i32" peekInt32 :: Ptr Int32 -> IO Int32

foreign import ccall unsafe "hazy_poke_i32" pokeInt32 :: Ptr Int32 -> Int32 -> IO ()

foreign import ccall unsafe "hazy_peek_u32" peekWord32 :: Ptr Word32 -> IO Word32

foreign import ccall unsafe "hazy_poke_u32" pokeWord32 :: Ptr Word32 -> Word32 -> IO ()

foreign import ccall unsafe "hazy_peek_i64" peekInt64 :: Ptr Int64 -> IO Int64

foreign import ccall unsafe "hazy_poke_i64" pokeInt64 :: Ptr Int64 -> Int64 -> IO ()

foreign import ccall unsafe "hazy_peek_u64" peekWord64 :: Ptr Word64 -> IO Word64

foreign import ccall unsafe "hazy_poke_u64" pokeWord64 :: Ptr Word64 -> Word64 -> IO ()

foreign import ccall unsafe "hazy_peek_f32" peekFloat :: Ptr Float -> IO Float

foreign import ccall unsafe "hazy_poke_f32" pokeFloat :: Ptr Float -> Float -> IO ()

foreign import ccall unsafe "hazy_peek_f64" peekDouble :: Ptr Double -> IO Double

foreign import ccall unsafe "hazy_poke_f64" pokeDouble :: Ptr Double -> Double -> IO ()

foreign import ccall unsafe "hazy_peek_ptr" peekPtr :: Ptr (Ptr a) -> IO (Ptr a)

foreign import ccall unsafe "hazy_poke_ptr" pokePtr :: Ptr (Ptr a) -> Ptr a -> IO ()

foreign import ccall unsafe "hazy_peek_ptr" peekFunPtr :: Ptr (FunPtr a) -> IO (FunPtr a)

foreign import ccall unsafe "hazy_poke_ptr" pokeFunPtr :: Ptr (FunPtr a) -> FunPtr a -> IO ()

instance Storable Int8 where
  sizeOf _ = 1
  alignment _ = 1
  peek = peekInt8
  poke = pokeInt8

instance Storable Word8 where
  sizeOf _ = 1
  alignment _ = 1
  peek = peekWord8
  poke = pokeWord8

instance Storable Int16 where
  sizeOf _ = 2
  alignment _ = 2
  peek = peekInt16
  poke = pokeInt16

instance Storable Word16 where
  sizeOf _ = 2
  alignment _ = 2
  peek = peekWord16
  poke = pokeWord16

instance Storable Int32 where
  sizeOf _ = 4
  alignment _ = 4
  peek = peekInt32
  poke = pokeInt32

instance Storable Word32 where
  sizeOf _ = 4
  alignment _ = 4
  peek = peekWord32
  poke = pokeWord32

instance Storable Int64 where
  sizeOf _ = 8
  alignment _ = 8
  peek = peekInt64
  poke = pokeInt64

instance Storable Word64 where
  sizeOf _ = 8
  alignment _ = 8
  peek = peekWord64
  poke = pokeWord64

-- An Int is kept in memory in 64 bits, as C's int64_t is.
instance Storable Int where
  sizeOf _ = 8
  alignment _ = 8
  peek address = fromIntegral <$> peekInt64 (castPtr address)
  poke address value = pokeInt64 (castPtr address) (fromIntegral value)

instance Storable Word where
  sizeOf _ = 8
  alignment _ = 8
  peek address = fromIntegral <$> peekWord64 (castPtr address)
  poke address value = pokeWord64 (castPtr address) (fromIntegral value)

-- A Char is kept as its code, in 32 bits.
instance Storable Char where
  sizeOf _ = 4
  alignment _ = 4
  peek address = toEnum . fromIntegral <$> peekInt32 (castPtr address)
  poke address value = pokeInt32 (castPtr address) (fromIntegral (fromEnum value))

-- A Bool is kept as C's int: 1 for True and 0 for False.
instance Storable Bool where
  sizeOf _ = 4
  alignment _ = 4
  peek address = (/= 0) <$> peekInt32 (castPtr address)
  poke address value = pokeInt32 (castPtr address) (if value then 1 else 0)

instance Storable Float where
  sizeOf _ = 4
  alignment _ = 4
  peek = peekFloat
  poke = pokeFloat

instance Storable Double where
  sizeOf _ = 8
  alignment _ = 8
  peek = peekDouble
  poke = pokeDouble

instance Storable (Ptr a) where
  sizeOf _ = 8
  alignment _ = 8
  peek = peekPtr
  poke = pokePtr

instance Storable (FunPtr a) where
  sizeOf _ = 8
  alignment _ = 8
  peek = peekFunPtr
  poke = pokeFunPtr
