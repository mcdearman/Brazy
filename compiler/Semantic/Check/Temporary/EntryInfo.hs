{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What is known of one thing a constructor holds, while checking: its
-- levity, still being worked out. Solving it gives the one in
-- "Semantic.Check.Info.Entry".
module Semantic.Check.Temporary.EntryInfo where

import qualified Semantic.Check.Info.Entry as Solved
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Unify as Unify (Solve, Type, solve)
import Syntax.Position (Position)

data EntryInfo s scope = EntryInfo
  { position :: !Position,
    levity :: !(Unify.Type s scope)
  }

solve :: Context s scope -> EntryInfo s scope -> Unify.Solve s (Solved.EntryInfo scope)
solve context EntryInfo {position, levity} = do
  levity <- Unify.solve context position levity
  pure Solved.EntryInfo {levity}
