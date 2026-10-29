-- |
-- The builtin type @IO@, written as Haskell source: a newtype of
-- @ST RealWorld@. 'builtinData' compiles it into the names and Core
-- definition the compiler keeps of it.
module Builtin.IO where

import Builtin (builtinData)
import Core.Tree.Data (Data)
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

-- An action of the outside world. The compiler knows it, as a foreign
-- function that is an action gives one.
bindings :: Bindings () scope
definition :: Data scope
(bindings, definition) =
  Type2.IO
    `builtinData` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      newtype IO a = IO (ST RealWorld a)
      """
