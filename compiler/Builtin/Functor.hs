-- |
-- The builtin class @Functor@, written as Haskell source: its methods
-- @fmap@ and @<$@, a default for @<$@, and its fixity. 'builtinClass'
-- compiles it into the names, Core definition and extra the compiler keeps
-- of it.
module Builtin.Functor where

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
  Type2.Functor
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Functor f where
        fmap :: (a -> b) -> f a -> f b
        (<$) :: a -> f b -> f a

        (<$) a = fmap (\\_ -> a)
      infixl 4 <$
      """
