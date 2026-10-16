module Semantic.Index.Constructor where

import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (Bool (..), IO, Ordering (..), map, traverse)

data Index scope = Index
  { typeIndex :: !(Type2.Index scope),
    constructorIndex :: !Int
  }
  deriving (Show, Eq, Ord)

data Bool = False | True
  deriving (Enum, Bounded)

false =
  Index
    { typeIndex = Type2.Bool,
      constructorIndex = fromEnum False
    }

true =
  Index
    { typeIndex = Type2.Bool,
      constructorIndex = fromEnum True
    }

data List = Nil | Cons
  deriving (Enum, Bounded)

nil =
  Index
    { typeIndex = Type2.List,
      constructorIndex = fromEnum Nil
    }

cons =
  Index
    { typeIndex = Type2.List,
      constructorIndex = fromEnum Cons
    }

data NonEmpty = Cons1
  deriving (Enum, Bounded)

cons1 =
  Index
    { typeIndex = Type2.NonEmpty,
      constructorIndex = fromEnum Cons1
    }

data Tuple = Tuple
  deriving (Enum, Bounded)

tuple n =
  Index
    { typeIndex = Type2.Tuple n,
      constructorIndex = fromEnum Tuple
    }

data Ordering = LT | EQ | GT
  deriving (Enum, Bounded)

lt =
  Index
    { typeIndex = Type2.Ordering,
      constructorIndex = fromEnum LT
    }

eq =
  Index
    { typeIndex = Type2.Ordering,
      constructorIndex = fromEnum EQ
    }

gt =
  Index
    { typeIndex = Type2.Ordering,
      constructorIndex = fromEnum GT
    }

data Ratio = MakeRatio
  deriving (Enum, Bounded)

makeRatio =
  Index
    { typeIndex = Type2.Ratio,
      constructorIndex = fromEnum MakeRatio
    }

data IO = MakeIO
  deriving (Enum, Bounded)

makeIO =
  Index
    { typeIndex = Type2.IO,
      constructorIndex = fromEnum MakeIO
    }

data All a = All
  { bool :: Bool -> a,
    list :: List -> a,
    nonEmpty :: NonEmpty -> a,
    tuplex :: Int -> Tuple -> a,
    ordering :: Ordering -> a,
    ratio :: Ratio -> a,
    io :: IO -> a,
    sized :: String -> a
  }

run :: (Type.Index scope -> Int -> a) -> All a -> Index scope -> a
run normal All {bool, list, nonEmpty, tuplex, ordering, ratio, io, sized} Index {typeIndex, constructorIndex} =
  case typeIndex of
    Type2.Index typeIndex -> normal typeIndex constructorIndex
    Type2.Bool -> bool (toEnum constructorIndex)
    Type2.List -> list (toEnum constructorIndex)
    Type2.NonEmpty -> nonEmpty (toEnum constructorIndex)
    Type2.Tuple n -> tuplex n (toEnum constructorIndex)
    Type2.Ordering -> ordering (toEnum constructorIndex)
    Type2.Ratio -> ratio (toEnum constructorIndex)
    Type2.IO -> io (toEnum constructorIndex)
    Type2.Int8 -> sized "Int8"
    Type2.Int16 -> sized "Int16"
    Type2.Int32 -> sized "Int32"
    Type2.Int64 -> sized "Int64"
    Type2.Word -> sized "Word"
    Type2.Word8 -> sized "Word8"
    Type2.Word16 -> sized "Word16"
    Type2.Word32 -> sized "Word32"
    Type2.Word64 -> sized "Word64"
    _ -> error "bad run constructor"

instance Shift0.Functor Index where
  map = Shift.mapDefault

instance Shift.Functor Index where
  map category Index {typeIndex, constructorIndex} =
    Index
      { typeIndex = Shift.map category typeIndex,
        constructorIndex
      }

instance Shift.PartialUnshift Index where
  partialUnshift abort Index {typeIndex, constructorIndex} =
    index <$> Shift.partialUnshift abort typeIndex
    where
      index typeIndex = Index {typeIndex, constructorIndex}

unlocal :: Index (Local ':+ scope) -> Index scope
unlocal Index {typeIndex, constructorIndex} =
  Index
    { typeIndex = Type2.unlocal typeIndex,
      constructorIndex
    }
