module Semantic.Check.Temporary.ConstructorInfo where

import qualified Data.Vector.Strict as Strict
import qualified Semantic.Check.Simple.ConstructorInfo as Solved
import Semantic.Check.Temporary.EntryInfo (EntryInfo)
import qualified Semantic.Check.Temporary.EntryInfo as EntryInfo
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

solve :: ConstructorInfo s scope -> Unify.Solve s (Solved.ConstructorInfo scope)
solve = \case
  ConstructorInfo {entries} -> do
    entries <- traverse EntryInfo.solve entries
    pure Solved.ConstructorInfo {entries}
  Newtype -> pure Solved.Newtype
  Holding {entries, held} -> do
    entries <- traverse EntryInfo.solve entries
    held <- traverse (uncurry Unify.solveEvidence) held
    pure Solved.Holding {entries, held}
