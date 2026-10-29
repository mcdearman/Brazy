-- |
-- The builtin class @Ix@, written as Haskell source: its superclass @Ord@,
-- its methods, and a default for @rangeSize@. 'builtinClass' compiles it
-- into the names, Core definition and extra the compiler keeps of it.
module Builtin.Ix where

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
  Type2.Ix
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Ord a => Ix a where
        range :: (a, a) -> [a]
        index :: (a, a) -> a -> Int
        inRange :: (a, a) -> a -> Bool
        rangeSize :: (a, a) -> Int

        rangeSize bounds = case bounds of
          (_, high) -> if inRange bounds high then index bounds high + 1 else 0
      """
