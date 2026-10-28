module Semantic.Check.Binding.Local where

import Control.Monad.ST (ST)
import qualified Core.Tree.Evidence as Simple (Evidence)
import Core.Tree.Type as Simple (Type)
import qualified Core.Type.Functor as Core
import qualified Data.Kind (Type)
import Data.Map (Map)
import Data.STRef (STRef)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict
import Semantic.Check.Mask (Mask)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Local as Label
import qualified Semantic.Index.Local as Local (Index)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Unify as Unify (Evidence, Logical, Type)

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
  | -- | Something that is known to hold of a type, with the evidence for
    -- it: a constructor that was matched on requires it of its types, and
    -- the value that was matched holds the evidence.
    Given !(Type2.Index scope) !(Unify.Type s scope) !(Unify.Evidence s scope)
  | -- | Not something that is known, but where a definition leaves what is
    -- to be checked once the rest of the definition it is inside has been:
    -- see @postponed@ in "Semantic.Check.Context". It is here because what
    -- is known is handed to everything inside a definition, and so is this.
    Late !(STRef s [ST s ()])
  | -- | Not something that is known either: this is inside types that are
    -- being checked together, where a name for a type is looked into and
    -- not asked what it was found to be. See @checkCount@ in
    -- "Semantic.Check.Temporary.Type".
    --
    -- With it are the types that are being checked, each as it is called
    -- from outside them and with the kind it has so far. What a name for a
    -- type stands for calls them that way.
    Grouping ![(Type2.Index scope, Unify.Type s scope)]
  | -- | This is inside what a name for a type stands for, which was looked
    -- into: a name that is by way of itself is found by meeting it again.
    Expanding !(Type2.Index scope)

shiftEquality :: Equality s scope -> Equality s (scope' ':+ scope)
shiftEquality = \case
  Equality index typex -> Equality (Shift0.map Shift0.Shift index) (Core.shiftLogical typex)
  Hidden logical typex -> Hidden (Shift0.map Shift0.Shift logical) (Core.shiftLogical typex)
  Given classx subject evidence ->
    Given (Shift0.map Shift0.Shift classx) (Core.shiftLogical subject) (Core.shiftLogical evidence)
  Late waiting -> Late waiting
  Grouping members ->
    Grouping [(Shift0.map Shift0.Shift index, Core.shiftLogical kind) | (index, kind) <- members]
  Expanding index -> Expanding (Shift0.map Shift0.Shift index)

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
