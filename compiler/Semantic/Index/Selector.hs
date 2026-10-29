{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- The index of a record field selector: the data type it selects from and
-- its place among that type's selectors.
module Semantic.Index.Selector where

import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data Index scope = Index
  { typeIndex :: !(Type2.Index scope),
    selectorIndex :: !Int
  }
  deriving (Show, Eq, Ord)

instance Shift0.Functor Index where
  map = Shift.mapDefault

instance Shift.Functor Index where
  map category Index {typeIndex, selectorIndex} = Index {typeIndex = Shift.map category typeIndex, selectorIndex}
