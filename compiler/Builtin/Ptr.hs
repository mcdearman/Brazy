-- |
-- The builtin type @Ptr@, written as Haskell source: a newtype of an
-- @Int@. 'builtinData' compiles it into the names and Core definition the
-- compiler keeps of it.
module Builtin.Ptr where

import Builtin (builtinData)
import Core.Tree.Data (Data)
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

-- An address of memory that holds values of a type, kept as an Int. The compiler
-- knows it, as a foreign function is given one as C has it.
bindings :: Bindings () scope
definition :: Data scope
(bindings, definition) =
  Type2.Ptr
    `builtinData` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      newtype Ptr a = Ptr Int
      """
