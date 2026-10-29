-- |
-- The builtin class @Monoid@, written as Haskell source: its superclass
-- @Semigroup@ and its methods @mempty@, @mappend@ and @mconcat@, each with a
-- default. 'builtinClass' compiles it into the names, Core definition and
-- extra the compiler keeps of it.
module Builtin.Monoid where

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
  Type2.Monoid
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class (Semigroup a) => Monoid a where
        mempty :: a
        mempty = mconcat []

        mappend :: a -> a -> a
        mappend = (<>)

        mconcat :: [a] -> a
        mconcat = \\case
          [] -> mempty
          (x : xs) -> x <> mconcat xs
      """
