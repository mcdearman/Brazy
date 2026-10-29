-- |
-- The builtin class @RealFrac@, written as Haskell source: its
-- superclasses @Real@ and @Fractional@, its methods, and defaults for all
-- but @properFraction@. 'builtinClass' compiles it into the names, Core
-- definition and extra the compiler keeps of it.
module Builtin.RealFrac where

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
  Type2.RealFrac
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class (Real a, Fractional a) => RealFrac a where
        properFraction :: Integral b => a -> (b, a)
        truncate, round :: Integral b => a -> b
        ceiling, floor :: Integral b => a -> b

        truncate x = m where (m, _) = properFraction x

        round x =
          let (n, r) = properFraction x
              m = if r < 0 then n - 1 else n + 1
           in case compare (abs r) 0.5 of
                LT -> n
                EQ -> if n `rem` 2 == 0 then n else m
                GT -> m

        ceiling x = if r > 0 then n + 1 else n
          where
            (n, r) = properFraction x

        floor x = if r < 0 then n - 1 else n
          where
            (n, r) = properFraction x
      """
