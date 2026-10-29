{-# LANGUAGE_HAZY UnorderedRecords #-}
module Semantic.Index.Method where

import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (Bounded, Enum, Eq, Ord, Show)
import qualified Prelude

data Index scope = Index
  { typeIndex :: !(Type2.Index scope),
    methodIndex :: !Int
  }
  deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)

instance Shift0.Functor Index where
  map = Shift.mapDefault

instance Shift.Functor Index where
  map category Index {typeIndex, methodIndex} = Index {typeIndex = Shift.map category typeIndex, methodIndex}

data Num
  = Plus
  | Minus
  | Multiply
  | Negate
  | Abs
  | Signum
  | FromInteger
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

plus = Index {typeIndex = Type2.Num, methodIndex = Prelude.fromEnum Plus}

minus = Index {typeIndex = Type2.Num, methodIndex = Prelude.fromEnum Minus}

multiply = Index {typeIndex = Type2.Num, methodIndex = Prelude.fromEnum Multiply}

negate = Index {typeIndex = Type2.Num, methodIndex = Prelude.fromEnum Negate}

abs = Index {typeIndex = Type2.Num, methodIndex = Prelude.fromEnum Abs}

signum = Index {typeIndex = Type2.Num, methodIndex = Prelude.fromEnum Signum}

fromInteger = Index {typeIndex = Type2.Num, methodIndex = Prelude.fromEnum FromInteger}

data Enum
  = Succ
  | Pred
  | ToEnum
  | FromEnum
  | EnumFrom
  | EnumFromThen
  | EnumFromTo
  | EnumFromThenTo
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

succ = Index {typeIndex = Type2.Enum, methodIndex = Prelude.fromEnum Succ}

pred = Index {typeIndex = Type2.Enum, methodIndex = Prelude.fromEnum Pred}

toEnum = Index {typeIndex = Type2.Enum, methodIndex = Prelude.fromEnum ToEnum}

fromEnum = Index {typeIndex = Type2.Enum, methodIndex = Prelude.fromEnum FromEnum}

enumFrom = Index {typeIndex = Type2.Enum, methodIndex = Prelude.fromEnum EnumFrom}

enumFromThen = Index {typeIndex = Type2.Enum, methodIndex = Prelude.fromEnum EnumFromThen}

enumFromTo = Index {typeIndex = Type2.Enum, methodIndex = Prelude.fromEnum EnumFromTo}

enumFromThenTo = Index {typeIndex = Type2.Enum, methodIndex = Prelude.fromEnum EnumFromThenTo}

data Bounded
  = MinBound
  | MaxBound
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

minBound = Index {typeIndex = Type2.Bounded, methodIndex = Prelude.fromEnum MinBound}

maxBound = Index {typeIndex = Type2.Bounded, methodIndex = Prelude.fromEnum MaxBound}

data Eq
  = Equal
  | NotEqual
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

equal = Index {typeIndex = Type2.Eq, methodIndex = Prelude.fromEnum Equal}

notEqual = Index {typeIndex = Type2.Eq, methodIndex = Prelude.fromEnum NotEqual}

data Ord
  = Compare
  | LessThen
  | LessThenEqual
  | GreaterThen
  | GreaterThenEqual
  | Max
  | Min
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

compare = Index {typeIndex = Type2.Ord, methodIndex = Prelude.fromEnum Compare}

lessThen = Index {typeIndex = Type2.Ord, methodIndex = Prelude.fromEnum LessThen}

lessThenEqual = Index {typeIndex = Type2.Ord, methodIndex = Prelude.fromEnum LessThenEqual}

greaterThen = Index {typeIndex = Type2.Ord, methodIndex = Prelude.fromEnum GreaterThen}

greaterThenEqual = Index {typeIndex = Type2.Ord, methodIndex = Prelude.fromEnum GreaterThenEqual}

max = Index {typeIndex = Type2.Ord, methodIndex = Prelude.fromEnum Max}

min = Index {typeIndex = Type2.Ord, methodIndex = Prelude.fromEnum Min}

data Real = ToRational
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

toRational = Index {typeIndex = Type2.Real, methodIndex = Prelude.fromEnum ToRational}

data Integral = Quot | Rem | Div | Mod | QuotRem | DivMod | ToInteger
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

quot = Index {typeIndex = Type2.Integral, methodIndex = Prelude.fromEnum Quot}

rem = Index {typeIndex = Type2.Integral, methodIndex = Prelude.fromEnum Rem}

div = Index {typeIndex = Type2.Integral, methodIndex = Prelude.fromEnum Div}

mod = Index {typeIndex = Type2.Integral, methodIndex = Prelude.fromEnum Mod}

quotRem = Index {typeIndex = Type2.Integral, methodIndex = Prelude.fromEnum QuotRem}

divMod = Index {typeIndex = Type2.Integral, methodIndex = Prelude.fromEnum DivMod}

toInteger = Index {typeIndex = Type2.Integral, methodIndex = Prelude.fromEnum ToInteger}

data Fractional = Divide | Recip | FromRational
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

divide = Index {typeIndex = Type2.Fractional, methodIndex = Prelude.fromEnum Divide}

recip = Index {typeIndex = Type2.Fractional, methodIndex = Prelude.fromEnum Recip}

fromRational = Index {typeIndex = Type2.Fractional, methodIndex = Prelude.fromEnum FromRational}

data Functor
  = Fmap
  | Fconst
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

fmap = Index {typeIndex = Type2.Functor, methodIndex = Prelude.fromEnum Fmap}

fconst = Index {typeIndex = Type2.Functor, methodIndex = Prelude.fromEnum Fconst}

data Foldable
  = Fold
  | FoldMap
  | FoldMap'
  | Foldr
  | Foldl
  | Foldl'
  | Foldr1
  | Foldl1
  | ToList
  | Null
  | Length
  | Elem
  | Maximum
  | Minimum
  | Sum
  | Product
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

foldr = Index {typeIndex = Type2.Foldable, methodIndex = Prelude.fromEnum Foldr}

data Traversable
  = Traverse
  | SequenceA
  | MapM
  | Sequence
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

traverse = Index {typeIndex = Type2.Traversable, methodIndex = Prelude.fromEnum Traverse}

data Ix
  = IxRange
  | IxIndex
  | IxInRange
  | IxRangeSize
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

ixRange = Index {typeIndex = Type2.Ix, methodIndex = Prelude.fromEnum IxRange}

ixIndex = Index {typeIndex = Type2.Ix, methodIndex = Prelude.fromEnum IxIndex}

ixInRange = Index {typeIndex = Type2.Ix, methodIndex = Prelude.fromEnum IxInRange}

data Applicative
  = Pure
  | Ap
  | LiftA2
  | DiscardLeft
  | DiscardRight
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

pure = Index {typeIndex = Type2.Applicative, methodIndex = Prelude.fromEnum Pure}

ap = Index {typeIndex = Type2.Applicative, methodIndex = Prelude.fromEnum Ap}

liftA2 = Index {typeIndex = Type2.Applicative, methodIndex = Prelude.fromEnum LiftA2}

discardLeft = Index {typeIndex = Type2.Applicative, methodIndex = Prelude.fromEnum DiscardLeft}

discardRight = Index {typeIndex = Type2.Applicative, methodIndex = Prelude.fromEnum DiscardRight}

data Monad
  = Bind
  | Then
  | Return
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

bind = Index {typeIndex = Type2.Monad, methodIndex = Prelude.fromEnum Bind}

thenx = Index {typeIndex = Type2.Monad, methodIndex = Prelude.fromEnum Then}

return = Index {typeIndex = Type2.Monad, methodIndex = Prelude.fromEnum Return}

data MonadFail
  = Fail
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

fail = Index {typeIndex = Type2.MonadFail, methodIndex = Prelude.fromEnum Fail}

data Semigroup
  = Combine
  | Sconcat
  | Stimes
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

combine = Index {typeIndex = Type2.Semigroup, methodIndex = Prelude.fromEnum Combine}

sconcat = Index {typeIndex = Type2.Semigroup, methodIndex = Prelude.fromEnum Sconcat}

stimes = Index {typeIndex = Type2.Semigroup, methodIndex = Prelude.fromEnum Stimes}

data Monoid
  = Mempty
  | Mappend
  | Mconcat
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

mempty = Index {typeIndex = Type2.Monoid, methodIndex = Prelude.fromEnum Mempty}

mappend = Index {typeIndex = Type2.Monoid, methodIndex = Prelude.fromEnum Mappend}

mconcat = Index {typeIndex = Type2.Monoid, methodIndex = Prelude.fromEnum Mconcat}

data Show = ShowsPrec | Show | ShowList
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

showsPrec = Index {typeIndex = Type2.Show, methodIndex = Prelude.fromEnum ShowsPrec}

show = Index {typeIndex = Type2.Show, methodIndex = Prelude.fromEnum Show}

showList = Index {typeIndex = Type2.Show, methodIndex = Prelude.fromEnum ShowList}

data Read = ReadsPrec | ReadList
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

readsPrec = Index {typeIndex = Type2.Read, methodIndex = Prelude.fromEnum ReadsPrec}

readList = Index {typeIndex = Type2.Read, methodIndex = Prelude.fromEnum ReadList}
