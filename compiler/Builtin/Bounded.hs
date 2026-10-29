-- |
-- The builtin class @Bounded@, written as Haskell source: its methods
-- @minBound@ and @maxBound@, which have no defaults. 'builtinClass' compiles
-- it into the names, Core definition and extra the compiler keeps of it.
module Builtin.Bounded where

import Builtin (builtinClass)
import Core.Tree.Class (Class)
import Core.Tree.ClassExtra (ClassExtra (..))
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

bindings :: Bindings () scope
definition :: Class scope
extra :: ClassExtra scope
(bindings, definition, extra) =
  Type2.Bounded
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Bounded a where
        minBound :: a
        maxBound :: a
      """
