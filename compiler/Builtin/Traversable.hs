module Builtin.Traversable where

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
  Type2.Traversable
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class (Functor t, Foldable t) => Traversable t where
        traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
        sequenceA :: Applicative f => t (f a) -> f (t a)
        mapM :: Monad m => (a -> m b) -> t a -> m (t b)
        sequence :: Monad m => t (m a) -> m (t a)

        traverse f t = sequenceA (fmap f t)
        sequenceA t = traverse (\\x -> x) t
        mapM f t = traverse f t
        sequence t = sequenceA t
      """
