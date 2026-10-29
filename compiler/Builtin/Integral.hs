-- |
-- The builtin class @Integral@, written as Haskell source: its superclasses
-- @Real@ and @Enum@, its methods, defaults for all but @quotRem@ and
-- @toInteger@, and the fixities of @quot@, @rem@, @div@ and @mod@.
-- 'builtinClass' compiles it into the names, Core definition and extra the
-- compiler keeps of it.
module Builtin.Integral where

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
  Type2.Integral
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class (Real a, Enum a) => Integral a where
        quot :: a -> a -> a
        rem :: a -> a -> a
        div :: a -> a -> a
        mod :: a -> a -> a
        quotRem :: a -> a -> (a, a)
        divMod :: a -> a -> (a, a)
        toInteger :: a -> Integer

        n `quot` d = q where (q,r) = quotRem n d
        n `rem` d = r where (q,r) = quotRem n d
        n `div` d = q where (q,r) = divMod n d
        n `mod` d = r where (q,r) = divMod n d
        divMod n d = if signum r == -signum d then (q-1,r+d) else qr where
          qr@(q, r) = quotRem n d
      infixl 7 `quot`, `rem`, `div`, `mod`
      """
