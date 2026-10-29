{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What is known of a record update, while checking: what is known of each
-- constructor is still being worked out. Solving it gives the one in
-- "Semantic.Check.Info.Update".
module Semantic.Check.Temporary.UpdateInfo where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Info.Update as Solved
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Unify as Unify

newtype UpdateInfo s scope = UpdateInfo
  { updateInfo :: Strict.Vector (Update s scope)
  }

data Update s scope = Update
  { constructorInfo :: !(ConstructorInfo s scope),
    selectorIndexes :: !(Strict.Vector (Strict.Maybe Int))
  }

solve :: Context s scope -> UpdateInfo s scope -> Unify.Solve s (Solved.UpdateInfo scope)
solve context UpdateInfo {updateInfo} = do
  updateInfo <- traverse (solveUpdate context) updateInfo
  pure Solved.UpdateInfo {updateInfo}

solveUpdate :: Context s scope -> Update s scope -> Unify.Solve s (Solved.Update scope)
solveUpdate context Update {constructorInfo, selectorIndexes} = do
  constructorInfo <- ConstructorInfo.solve context constructorInfo
  pure Solved.Update {constructorInfo, selectorIndexes}
