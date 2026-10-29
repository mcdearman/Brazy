{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- An instance, written or asked for with @deriving@, and what it requires
-- of its parameters, which checking works out.
module Semantic.Tree.Instance where

import qualified Core.Tree.Constraints as Core
import Data.Functor.Classes (Show1, showsPrec1)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity)
import Data.NaturalTransformation (NaturalTransformation (..))
import Semantic.Connect (Connect (..))
import Semantic.Functor2 (Traversable2 (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Combinators.Inferred (Inferred)
import Semantic.Tree.InstanceDefinition2 (InstanceDefinition2)
import Syntax.Position (Position)

data Instance solve loeb layout stage scope = Instance
  { startPosition :: !Position,
    -- | Whether it is asked for in a @deriving@ clause, where nothing says
    -- what it requires.
    clause :: !Bool,
    definition :: !(InstanceDefinition2 solve loeb layout stage scope),
    prerequisites :: loeb (Inferred Core.Prerequisites stage scope)
  }

instance (Show1 solve, Show1 loeb) => Show (Instance solve loeb layout stage scope) where
  showsPrec _ Instance {startPosition, definition, prerequisites} =
    showString "Instance {startPosition = "
      . showsPrec 0 startPosition
      . showString ", definition = "
      . showsPrec 0 definition
      . showString ", prerequisites = "
      . showsPrec1 0 prerequisites
      . showString "}"

instance (Functor solve, Functor loeb) => Shift0.Functor (Instance solve loeb layout stage) where
  map = Shift.mapDefault

instance (Functor solve, Functor loeb) => Shift.Functor (Instance solve loeb layout stage) where
  map category Instance {startPosition, clause, definition, prerequisites} =
    Instance
      { startPosition,
        clause,
        definition = Shift.map category definition,
        prerequisites = Shift.map category <$> prerequisites
      }

instance Traversable2 (Instance solve) where
  traverse2 (Morph f) Instance {startPosition, clause, definition, prerequisites} =
    instancex <$> traverse2 (Morph f) definition <*> getCompose (f prerequisites)
    where
      instancex definition prerequisites =
        Instance {startPosition, clause, definition, prerequisites}

instance (solve ~ Identity, loeb ~ Identity) => Connect (Instance solve loeb) where
  connect Instance {startPosition, clause, definition, prerequisites} =
    Instance
      { startPosition,
        clause,
        definition = connect definition,
        prerequisites
      }
  seperate Instance {startPosition, clause, definition, prerequisites} =
    Instance
      { startPosition,
        clause,
        definition = seperate definition,
        prerequisites
      }
