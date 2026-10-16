module Builtin.Word8 where

import Builtin (builtinData)
import Core.Tree.Data (Data)
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

-- A number of 8 bits with no sign, kept as an Integer. The
-- compiler knows it, as a foreign function is given one as C has it.
bindings :: Bindings () scope
definition :: Data scope
(bindings, definition) =
  Type2.Word8
    `builtinData` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      newtype Word8 = Word8 Integer
      """
