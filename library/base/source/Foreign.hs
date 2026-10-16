-- |
-- What a program that calls C has: the numbers of a fixed width, addresses,
-- values in memory, and memory.
module Foreign
  ( module Data.Bits,
    module Data.Int,
    module Data.Word,
    module Foreign.Ptr,
    module Foreign.Storable,
    module Foreign.Marshal,
  )
where

import qualified Data.Bits
import qualified Data.Int
import qualified Data.Word
import qualified Foreign.Marshal
import qualified Foreign.Ptr
import qualified Foreign.Storable
