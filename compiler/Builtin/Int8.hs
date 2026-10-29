-- |
-- The builtin type @Int8@, written as Haskell source: a newtype of an
-- @Int@. 'builtinData' compiles it into the names and Core definition the
-- compiler keeps of it.
module Builtin.Int8 where

import Builtin (builtinData)
import Core.Tree.Data (Data)
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

-- A number of 8 bits with a sign, kept as an Int. The
-- compiler knows it, as a foreign function is given one as C has it.
bindings :: Bindings () scope
definition :: Data scope
(bindings, definition) =
  Type2.Int8
    `builtinData` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      newtype Int8 = Int8 Int
      """
