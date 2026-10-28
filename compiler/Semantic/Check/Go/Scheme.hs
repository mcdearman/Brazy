module Semantic.Check.Go.Scheme (Scheme (..), augment, augmentNeeding) where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constraints as Simple.Constraints
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import Semantic.Check.Mask (Mask)
import Semantic.Check.Scheme (augmentNamed)
import Semantic.Scope (Environment (..), Local)
import Semantic.Stage (Check)
import Semantic.Tree.Constraints (Constraints)
import Semantic.Tree.Scheme (Scheme (..))
import Semantic.Tree.TypePattern (TypePattern (TypePattern))
import qualified Semantic.Tree.TypePattern as TypePattern
import Syntax.Position (Position)
import Prelude hiding (head)

-- |
-- The same, when what is required is not something that was written.
augmentNeeding ::
  Position ->
  Strict.Vector (TypePattern Position Check scope) ->
  Simple.Constraints.Constraints scope ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augmentNeeding position parameters =
  augmentNamed
    name
    position
    (TypePattern.typex' <$> parameters)
  where
    name i = case parameters Strict.Vector.! i of
      TypePattern {name} -> name

augment ::
  Position ->
  Strict.Vector (TypePattern Position Check scope) ->
  Constraints Position Check scope ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augment position parameters constraints =
  augmentNamed
    name
    position
    (TypePattern.typex' <$> parameters)
    (Simple.Constraints.simplify constraints)
  where
    name i = case parameters Strict.Vector.! i of
      TypePattern {name} -> name
