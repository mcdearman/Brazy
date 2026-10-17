-- |
-- Addresses of memory and of functions of C. 'Ptr' and 'FunPtr' are
-- builtins, kept as an 'Int', which the compiler knows as a foreign
-- function is given them.
module Foreign.Ptr
  ( Ptr,
    nullPtr,
    castPtr,
    plusPtr,
    alignPtr,
    minusPtr,
    FunPtr,
    nullFunPtr,
    castFunPtr,
    castFunPtrToPtr,
    castPtrToFunPtr,
    freeHaskellFunPtr,
    IntPtr (..),
    ptrToIntPtr,
    intPtrToPtr,
    WordPtr (..),
    ptrToWordPtr,
    wordPtrToPtr,
  )
where

import Hazy.Prelude (FunPtr (..), Ptr (..))

nullPtr :: Ptr a
nullPtr = Ptr 0

castPtr :: Ptr a -> Ptr b
castPtr (Ptr address) = Ptr address

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr address) offset = Ptr (address + offset)

-- | The first address from this one that is a multiple of the alignment.
alignPtr :: Ptr a -> Int -> Ptr a
alignPtr pointer@(Ptr address) alignment = case address `rem` alignment of
  0 -> pointer
  over -> Ptr (address + (alignment - over))

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr (Ptr address) (Ptr address') = address - address'

nullFunPtr :: FunPtr a
nullFunPtr = FunPtr 0

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr address) = FunPtr address

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr address) = Ptr address

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr address) = FunPtr address

-- | Free a function of C that a wrapper made, which is not called again.
foreign import ccall "hazy_wrapper_free" freeHaskellFunPtr :: FunPtr a -> IO ()

newtype IntPtr = IntPtr Int
  deriving (Eq, Ord, Show)

ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr (Ptr address) = IntPtr address

intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr (IntPtr address) = Ptr address

newtype WordPtr = WordPtr Word
  deriving (Eq, Ord, Show)

ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr address) = WordPtr (fromIntegral address)

wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr (WordPtr address) = Ptr (fromIntegral address)
