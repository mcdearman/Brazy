module Builtin.RealFloat where

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
  Type2.RealFloat
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class (RealFrac a, Floating a) => RealFloat a where
        floatRadix :: a -> Integer
        floatDigits :: a -> Int
        floatRange :: a -> (Int, Int)
        decodeFloat :: a -> (Integer, Int)
        encodeFloat :: Integer -> Int -> a
        exponent :: a -> Int
        significand :: a -> a
        scaleFloat :: Int -> a -> a
        isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE :: a -> Bool
        atan2 :: a -> a -> a

        exponent x = if m == 0 then 0 else n + floatDigits x
          where
            (m, n) = decodeFloat x

        significand x = encodeFloat m (negate (floatDigits x))
          where
            (m, _) = decodeFloat x

        scaleFloat k x = encodeFloat m (n + k)
          where
            (m, n) = decodeFloat x

        atan2 y x
          | x > 0 = atan (y / x)
          | both (x == 0) (y > 0) = pi / 2
          | both (x < 0) (y > 0) = pi + atan (y / x)
          | either (both (x <= 0) (y < 0))
              (either (both (x < 0) (isNegativeZero y))
                (both (isNegativeZero x) (isNegativeZero y))) =
              negate (atan2 (negate y) x)
          | both (y == 0) (either (x < 0) (isNegativeZero x)) = pi
          | both (x == 0) (y == 0) = y
          | True = x + y
          where
            both a b = if a then b else False
            either a b = if a then True else b
      """
