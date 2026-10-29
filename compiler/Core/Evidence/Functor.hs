-- |
-- A class for Core trees that hold evidence, to change the logical
-- variables of evidence and of types in them: the unknowns the type checker
-- leaves in a tree while it works, which a finished tree has none of. With
-- it, a way to move a tree and its logical variables into a larger scope.
module Core.Evidence.Functor where

import Data.Kind (Constraint, Type)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (Functor, map)

type Functor :: (Type -> Type -> Environment -> Type) -> Constraint
class Functor typef where
  map :: (a -> b) -> (c -> d) -> typef a c scope -> typef b d scope

shiftLogical ::
  ( Functor typef,
    Shift0.Functor f1,
    Shift0.Functor f2,
    Shift0.Functor (typef (f1 scope2) (f2 scope2))
  ) =>
  typef (f1 scope2) (f2 scope2) scope2 ->
  typef (f1 (scope3 ':+ scope2)) (f2 (scope3 ':+ scope2)) (scope3 ':+ scope2)
shiftLogical = mapLogical Shift0.Shift

mapLogical ::
  ( Functor typef,
    Shift0.Functor f1,
    Shift0.Functor f2,
    Shift0.Functor (typef (f1 scope1) (f2 scope1))
  ) =>
  Shift0.Category scope1 scope2 ->
  typef (f1 scope1) (f2 scope1) scope1 ->
  typef (f1 scope2) (f2 scope2) scope2
mapLogical category = map (Shift0.map category) (Shift0.map category) . Shift0.map category
