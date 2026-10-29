{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What is known of one thing a constructor holds, once checking is done:
-- its levity. While checking, it is the one in
-- "Semantic.Check.Temporary.EntryInfo".
module Semantic.Check.Info.Entry where

import qualified Core.Substitute as Substitute
import qualified Core.Tree.Type as Simple
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

newtype EntryInfo scope = EntryInfo
  { levity :: Simple.Type scope
  }
  deriving (Show)

instance Shift0.Functor EntryInfo where
  map = Shift.mapDefault

instance Shift.Functor EntryInfo where
  map = Substitute.mapDefault

instance Substitute.Functor EntryInfo where
  map category EntryInfo {levity} =
    EntryInfo
      { levity = Substitute.map category levity
      }
