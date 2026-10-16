module Semantic.Index.Type2 where

import {-# SOURCE #-} qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Type as Type1
import Semantic.Scope (Environment ((:+)), Local)
import Prelude hiding (map, traverse)

data Index scope
  = Index !(Type1.Index scope)
  | Lifted !(Constructor.Index scope)
  | Bool
  | Char
  | ST
  | IO
  | RealWorld
  | Arrow
  | List
  | NonEmpty
  | Tuple !Int
  | Integer
  | Int
  | Ordering
  | Ratio
  | Float
  | Double
  | Int8
  | Int16
  | Int32
  | Int64
  | Word
  | Word8
  | Word16
  | Word32
  | Word64
  | Num
  | Enum
  | Bounded
  | Eq
  | Ord
  | Real
  | Integral
  | Ix
  | Fractional
  | Floating
  | RealFrac
  | RealFloat
  | Functor
  | Foldable
  | Traversable
  | Applicative
  | Monad
  | MonadFail
  | Semigroup
  | Monoid
  | Show
  | Read
  | Lazy
  | Strict
  | Every
  deriving (Show, Eq, Ord)

data Split scope
  = Normal !(Type1.Index scope)
  | Constructor !(Constructor.Index scope)
  | Builtin !(forall scope. Index scope)

split :: Index scope -> Split scope
split = \case
  Index index -> Normal index
  Lifted constructor -> Constructor constructor
  Bool -> Builtin Bool
  Char -> Builtin Char
  ST -> Builtin ST
  IO -> Builtin IO
  RealWorld -> Builtin RealWorld
  Arrow -> Builtin Arrow
  List -> Builtin List
  NonEmpty -> Builtin NonEmpty
  Tuple count -> Builtin (Tuple count)
  Integer -> Builtin Integer
  Int -> Builtin Int
  Ordering -> Builtin Ordering
  Ratio -> Builtin Ratio
  Float -> Builtin Float
  Double -> Builtin Double
  Int8 -> Builtin Int8
  Int16 -> Builtin Int16
  Int32 -> Builtin Int32
  Int64 -> Builtin Int64
  Word -> Builtin Word
  Word8 -> Builtin Word8
  Word16 -> Builtin Word16
  Word32 -> Builtin Word32
  Word64 -> Builtin Word64
  Num -> Builtin Num
  Enum -> Builtin Enum
  Bounded -> Builtin Bounded
  Eq -> Builtin Eq
  Ord -> Builtin Ord
  Real -> Builtin Real
  Integral -> Builtin Integral
  Ix -> Builtin Ix
  Fractional -> Builtin Fractional
  Floating -> Builtin Floating
  RealFrac -> Builtin RealFrac
  RealFloat -> Builtin RealFloat
  Functor -> Builtin Functor
  Foldable -> Builtin Foldable
  Traversable -> Builtin Traversable
  Applicative -> Builtin Applicative
  Monad -> Builtin Monad
  MonadFail -> Builtin MonadFail
  Semigroup -> Builtin Semigroup
  Monoid -> Builtin Monoid
  Show -> Builtin Show
  Read -> Builtin Read
  Lazy -> Builtin Lazy
  Strict -> Builtin Strict
  Every -> Builtin Every

unlocal :: Index (Local ':+ scope) -> Index scope
unlocal index = case split index of
  Normal normal -> Index $ Type1.unlocal normal
  Constructor lifted -> Lifted $ Constructor.unlocal lifted
  Builtin builtin -> builtin
