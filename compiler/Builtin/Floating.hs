-- |
-- The builtin class @Floating@, written as Haskell source: its superclass
-- @Fractional@, its methods, defaults for @**@, @logBase@, @sqrt@, @tan@ and
-- @tanh@, and the fixity of @**@. 'builtinClass' compiles it into the
-- names, Core definition and extra the compiler keeps of it.
module Builtin.Floating where

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
  Type2.Floating
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Fractional a => Floating a where
        pi :: a
        exp, log, sqrt :: a -> a
        (**), logBase :: a -> a -> a
        sin, cos, tan :: a -> a
        asin, acos, atan :: a -> a
        sinh, cosh, tanh :: a -> a
        asinh, acosh, atanh :: a -> a

        x ** y = exp (log x * y)
        logBase x y = log y / log x
        sqrt x = x ** 0.5
        tan x = sin x / cos x
        tanh x = sinh x / cosh x
      infixr 8 **
      """
