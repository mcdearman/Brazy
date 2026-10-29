{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What is known of a constructor where it is used, while checking: the
-- levities and the evidence it holds are still being worked out. Solving it
-- gives the one in "Semantic.Check.Info.Constructor".
module Semantic.Check.Temporary.ConstructorInfo where

import qualified Data.Vector.Strict as Strict
import qualified Semantic.Check.Info.Constructor as Solved
import Semantic.Check.Temporary.EntryInfo (EntryInfo)
import qualified Semantic.Check.Temporary.EntryInfo as EntryInfo
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data ConstructorInfo s scope
  = ConstructorInfo
      { entries :: !(Strict.Vector (EntryInfo s scope))
      }
  | Newtype
  | -- | See 'Solved.Holding'. The evidence is kept with where it is asked.
    Holding
      { entries :: !(Strict.Vector (EntryInfo s scope)),
        held :: !(Strict.Vector (Position, Unify.Evidence s scope))
      }

solve :: Context s scope -> ConstructorInfo s scope -> Unify.Solve s (Solved.ConstructorInfo scope)
solve context = \case
  ConstructorInfo {entries} -> do
    entries <- traverse (EntryInfo.solve context) entries
    pure Solved.ConstructorInfo {entries}
  Newtype -> pure Solved.Newtype
  Holding {entries, held} -> do
    entries <- traverse (EntryInfo.solve context) entries
    held <- traverse (uncurry (Unify.solveEvidence context)) held
    pure Solved.Holding {entries, held}
