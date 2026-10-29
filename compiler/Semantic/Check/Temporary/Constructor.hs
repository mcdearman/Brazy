{-# LANGUAGE_HAZY UnorderedRecords #-}
module Semantic.Check.Temporary.Constructor where

import Control.Monad.ST (ST)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector (empty)
import Semantic.Check.Context (Context (..))
import Semantic.Check.Temporary.Entry (Entry)
import qualified Semantic.Check.Temporary.Entry as Entry
import Semantic.Check.Temporary.Type (Type)
import qualified Semantic.Check.Temporary.Type as Type
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Constructor (Syntax)
import qualified Semantic.Tree.Constructor as Semantic (Constructor (..))
import qualified Semantic.Tree.Constructor as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import qualified Syntax.Variable as Variable

-- |
-- What type a constructor makes a value of: see 'Solved.Shape'.
data Shape s scope
  = Ordinary
  | Indexed
      { variables :: !Int,
        equated :: !(Strict.Vector (Type s scope, Type s scope)),
        arguments :: !(Strict.Vector (Type s scope))
      }

data Constructor s scope
  = Constructor
  { position :: !Position,
    name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (Entry s scope)),
    shape :: !(Shape s scope)
  }

check :: Context s scope -> Semantic.Constructor Resolve scope -> ST s (Constructor s scope)
check context constructor = case constructor of
  -- A constructor comes from the resolver this way only when it was
  -- declared with its type's parameters in scope.
  Semantic.Constructor {position, name, syntax, entries} -> do
    entries <- traverse (Entry.check context) entries
    pure Constructor {position, name, syntax, entries, shape = Ordinary}

solve :: Context s scope -> Constructor s scope -> Unify.Solve s (Solved.Constructor Check scope)
solve context Constructor {position, name, syntax, entries, shape} = do
  entries <- traverse (Entry.solve context) entries
  shape <- case shape of
    Ordinary -> pure Solved.Ordinary
    Indexed {variables, equated, arguments} -> do
      arguments <- traverse (Type.solve context) arguments
      equated <- for equated $ \(left, right) -> do
        left <- Type.solve context left
        right <- Type.solve context right
        pure (left, right)
      -- What it requires is put in by whoever knows it.
      pure Solved.Indexed {variables, asked = Strict.Vector.empty, equated, arguments}
  pure Solved.Constructor {position, name, syntax, entries, shape}
