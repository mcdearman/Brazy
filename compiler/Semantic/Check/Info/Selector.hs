{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What is known of a field selector where it is used, once checking is
-- done, for Core: the levity of the field, when it is in the same place in
-- every constructor, or else, for each constructor, where the field is in
-- it, if it has it. While checking, it is the one in
-- "Semantic.Check.Temporary.SelectorInfo".
module Semantic.Check.Info.Selector where

import qualified Core.Tree.Type as Simple
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Info.Constructor (ConstructorInfo)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data SelectorInfo scope
  = Uniform
      { levity :: !(Simple.Type scope)
      }
  | Disjoint
      { select :: !(Strict.Vector (Select scope))
      }
  deriving (Show)

instance Scope.Show SelectorInfo where
  showsPrec = showsPrec

data Select scope
  = Select
  { selectIndex :: !(Strict.Maybe Int),
    constructorInfo :: !(ConstructorInfo scope)
  }
  deriving (Show)

instance Shift0.Functor SelectorInfo where
  map = Shift.mapDefault

instance Shift.Functor SelectorInfo where
  map category = \case
    Uniform {levity} -> Uniform {levity = Shift.map category levity}
    Disjoint {select} -> Disjoint {select = Shift.map category <$> select}

instance Shift0.Functor Select where
  map = Shift.mapDefault

instance Shift.Functor Select where
  map category Select {selectIndex, constructorInfo} =
    Select
      { selectIndex,
        constructorInfo = Shift.map category constructorInfo
      }
