-- |
-- A class for Core types and the trees made of them, to change the logical
-- variables in them: the unknowns the type checker leaves in a type while
-- it works, which a finished type has none of. With it, a way to move a
-- tree and its logical variables into a larger scope.
module Core.Type.Functor where

import Data.Kind (Constraint, Type)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (Functor, map)

type Functor :: (Type -> Environment -> Type) -> Constraint
class Functor typef where
  map :: (a -> b) -> typef a scope -> typef b scope

shiftLogical ::
  (Functor typef, Shift0.Functor f, Shift0.Functor (typef (f scope1))) =>
  typef (f scope1) scope1 -> typef (f (scope ':+ scope1)) (scope ':+ scope1)
shiftLogical = mapLogical Shift0.Shift

mapLogical ::
  (Functor typef, Shift0.Functor f, Shift0.Functor (typef (f scope1))) =>
  Shift0.Category scope1 scope2 -> typef (f scope1) scope1 -> typef (f scope2) scope2
mapLogical category = map (Shift0.map category) . Shift0.map category
