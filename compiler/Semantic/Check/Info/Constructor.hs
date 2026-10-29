{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What is known of a constructor where it is used, once checking is done,
-- for Core to lay it out: the levity of each thing it holds, or that it is
-- a newtype's, or the evidence it holds for what it requires of its types.
-- The checked tree carries it at each use. While checking, it is the one in
-- "Semantic.Check.Temporary.ConstructorInfo".
module Semantic.Check.Info.Constructor where

import Core.Tree.Evidence (Evidence)
import qualified Data.Vector.Strict as Strict
import Semantic.Check.Info.Entry (EntryInfo)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data ConstructorInfo scope
  = ConstructorInfo
      { entries :: !(Strict.Vector (EntryInfo scope))
      }
  | Newtype
  | -- | A constructor that is used to make a value, when it requires
    -- something of its types. The evidence for that is what it holds last,
    -- after what it is given, and the last of the entries are for it.
    Holding
      { entries :: !(Strict.Vector (EntryInfo scope)),
        held :: !(Strict.Vector (Evidence scope))
      }
  deriving (Show)

instance Scope.Show ConstructorInfo where
  showsPrec = showsPrec

entryCount :: ConstructorInfo scope -> Int
entryCount ConstructorInfo {entries} = length entries
entryCount Newtype = 1
entryCount Holding {entries, held} = length entries - length held

instance Shift0.Functor ConstructorInfo where
  map = Shift.mapDefault

instance Shift.Functor ConstructorInfo where
  map category = \case
    ConstructorInfo {entries} ->
      ConstructorInfo
        { entries = Shift.map category <$> entries
        }
    Newtype -> Newtype
    Holding {entries, held} ->
      Holding
        { entries = Shift.map category <$> entries,
          held = Shift.map category <$> held
        }
