module ForeignDynamic where

import Data.Int (Int64)
import Foreign.Ptr (FunPtr)

-- The address a function is called by is of a function of the type of
-- the rest, which here is an action and there is not.
foreign import ccall "dynamic" call :: FunPtr (Int64 -> Int64) -> Int64 -> IO Int64
