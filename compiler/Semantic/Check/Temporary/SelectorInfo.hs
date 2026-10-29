{-# LANGUAGE_HAZY UnorderedRecords #-}
module Semantic.Check.Temporary.SelectorInfo where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Info.Selector as Solved
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Unify as Unify (Solve, Type, solve)
import Syntax.Position (Position)

data SelectorInfo s scope
  = Uniform
      { position :: !Position,
        levity :: !(Unify.Type s scope)
      }
  | Disjoint
      { select :: !(Strict.Vector (Select s scope))
      }

data Select s scope
  = Select
  { selectIndex :: !(Strict.Maybe Int),
    constructorInfo :: !(ConstructorInfo s scope)
  }

solve :: Context s scope -> SelectorInfo s scope -> Unify.Solve s (Solved.SelectorInfo scope)
solve context = \case
  Uniform {position, levity} -> do
    levity <- Unify.solve context position levity
    pure Solved.Uniform {levity}
  Disjoint {select} -> do
    select <- traverse (solveSelect context) select
    pure Solved.Disjoint {select}

solveSelect :: Context s scope -> Select s scope -> Unify.Solve s (Solved.Select scope)
solveSelect context Select {selectIndex, constructorInfo} = do
  constructorInfo <- ConstructorInfo.solve context constructorInfo
  pure Solved.Select {selectIndex, constructorInfo}
