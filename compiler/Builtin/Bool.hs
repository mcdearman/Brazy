-- |
-- The builtin type @Bool@, written as Haskell source: its constructors
-- @False@ and @True@. 'builtinData' compiles it into the names and Core
-- definition the compiler keeps of it.
module Builtin.Bool where

import Builtin (builtinData)
import Core.Tree.Data (Data)
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

bindings :: Bindings () scope
definition :: Data scope
(bindings, definition) =
  Type2.Bool
    `builtinData` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      data Bool = False | True
      """
