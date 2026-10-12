module Semantic.Check.LocalBinding where

import qualified Core.Tree.Evidence as Simple (Evidence)
import Core.Tree.Type as Simple (Type)
import qualified Core.Type.Functor as Core
import qualified Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict
import Semantic.Check.Mask (Mask)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Local as Label
import qualified Semantic.Index.Local as Local (Index)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Unify as Unify (Logical, Type)

type LocalBinding :: Data.Kind.Type -> Environment -> Data.Kind.Type
data LocalBinding s scope
  = Rigid
      { label :: !(forall scope. Label.LocalBinding scope),
        rigid :: !(Simple.Type scope),
        -- | What is known of the variable, by class. There may be more
        -- than one thing known for a class, of the variable given
        -- different arguments: @Eq (f Int)@ and @Eq (f Char)@.
        constraints :: !(Map (Type2.Index scope) [Constraint scope]),
        mask :: !Mask
      }
  | Wobbly
      { label :: !(forall scope. Label.LocalBinding scope),
        wobbly :: !(Unify.Type s scope)
      }

instance Shift0.Functor (LocalBinding s) where
  map category = \case
    Rigid {label, rigid, constraints, mask} ->
      Rigid
        { label,
          rigid = Shift0.map category rigid,
          constraints =
            Map.map (map (Shift0.map category)) $ Map.mapKeysMonotonic (Shift0.map category) constraints,
          mask
        }
    Wobbly {label, wobbly} ->
      Wobbly
        { label,
          wobbly = Core.mapLogical category wobbly
        }

-- |
-- What a type variable is known to be, somewhere. Matching on a constructor
-- that says what type it makes tells what the variables of the type that
-- was matched on are, for as far as the match reaches:
--
-- > eval :: Expr a -> a
-- > eval (Number n) = n
--
-- where @Number :: Int -> Expr Int@, so that @a@ is @Int@ on that line.
--
-- The same may be known of a type that a constructor hides, which is not a
-- variable anyone wrote and has no name.
data Equality s scope
  = Equality !(Local.Index scope) !(Unify.Type s scope)
  | Hidden !(Unify.Logical s scope) !(Unify.Type s scope)

shiftEquality :: Equality s scope -> Equality s (scope' ':+ scope)
shiftEquality = \case
  Equality index typex -> Equality (Shift0.map Shift0.Shift index) (Core.shiftLogical typex)
  Hidden logical typex -> Hidden (Shift0.map Shift0.Shift logical) (Core.shiftLogical typex)

data Constraint scope = Constraint
  { arguments :: !(Strict.Vector (Simple.Type scope)),
    evidence :: !(Simple.Evidence scope)
  }
  deriving (Show)

-- |
-- Put together what is known for one class. What is known twice over, of
-- the variable given the same arguments, is kept once: the evidence is left
-- biased.
combine :: [Constraint scope] -> [Constraint scope] -> [Constraint scope]
combine left right = left ++ filter new right
  where
    new Constraint {arguments} = all (\Constraint {arguments = known} -> known /= arguments) left

instance Shift0.Functor Constraint where
  map category Constraint {arguments, evidence} =
    Constraint
      { arguments = fmap (Shift0.map category) arguments,
        evidence = (Shift0.map category) evidence
      }
