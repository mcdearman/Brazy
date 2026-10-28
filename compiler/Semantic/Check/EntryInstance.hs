module Semantic.Check.EntryInstance where

import Control.Monad.ST (ST)
import Core.Substitute (substituteType)
import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.Entry (Entry, EntryF (..))
import Core.Tree.Forall (ForallOver (..))
import Core.Tree.Instanciation (InstanciationF (..))
import qualified Core.Tree.Forall as Forall (mono)
import Core.Tree.Type (TypeF (Constructor))
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeaturePolymorphicComponents)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import Semantic.Check.Temporary.EntryInfo (EntryInfo (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data EntryInstance s scope = EntryInstance
  { position :: !Position,
    -- | The type of what is held, when that is a type. When it has a
    -- @forall@ of its own this is not to be asked for: whatever asks takes
    -- what is held for something of one type, and that is not done.
    entry :: Unify.Type s scope,
    -- | The type of what is held, as a type that may have a @forall@.
    scheme :: !(Unify.Forall s scope),
    -- | Whether it has one.
    quantified :: !Bool,
    strict :: !(Unify.Type s scope)
  }

instanciate :: Position -> Strict.Vector (Unify.Type s scope) -> Entry (Local ':+ scope) -> EntryInstance s scope
instanciate position fresh Entry {entry = scheme@ForallOver {parameters, constraints, result}, strict}
  | null parameters,
    None <- constraints,
    entry <- substituteType types (Shift.map (Shift.Unshift $ error "a type variable that nothing binds") result) =
      EntryInstance
        { position,
          entry,
          scheme = Forall.mono entry,
          quantified = False,
          strict = substituteType types strict
        }
  | otherwise =
      EntryInstance
        { position,
          entry = unsupportedFeaturePolymorphicComponents position,
          scheme = substituteType types scheme,
          quantified = True,
          -- What requires something is a function of the evidence for it.
          -- That function is what is held, and not something that is yet to
          -- be worked out to be it, so that it can be given the evidence
          -- wherever it is taken out.
          strict = case constraints of
            None -> substituteType types strict
            Constraints {} -> Constructor Type2.Strict
        }
  where
    types = Strict.Vector.toLazy fresh

-- |
-- The type of what is held, where it is taken out with nothing said of what
-- type it is taken at: a type with a @forall@ is taken at types that are
-- yet to be found. One that requires something cannot be taken out this
-- way, because there is nowhere here to say what evidence it is given.
taken :: Context s scope -> Position -> EntryInstance s scope -> ST s (Unify.Type s scope)
taken context position EntryInstance {entry, scheme, quantified}
  | not quantified = pure entry
  | otherwise = do
      (typex, instanciation) <- Unify.instanciate context position scheme
      case instanciation of
        Mono -> pure typex
        Instanciation {} -> unsupportedFeaturePolymorphicComponents position

info :: EntryInstance s scope -> EntryInfo s scope
info EntryInstance {position, strict} = EntryInfo {position, strict}

mark :: Context s scope -> EntryInstance s scope -> ST s ()
mark context EntryInstance {position, strict} =
  Unify.mark context position Mask.Known strict
