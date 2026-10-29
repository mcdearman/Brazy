{-# LANGUAGE_HAZY UnorderedRecords #-}
module Semantic.Tree.InstanceDefinition2 where

import qualified Core.Tree.Constraints as Core (Constraints)
import Data.Functor.Classes (Show1, showsPrec1)
import Data.Functor.Compose (getCompose)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.NaturalTransformation (NaturalTransformation (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Connect (Connect (..))
import Semantic.Functor2 (Traversable2 (..))
import Semantic.Layout (Layout)
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Stage)
import Semantic.Tree.Combinators.Inferred (Inferred)
import Semantic.Tree.Constraints (Constraints)
import Semantic.Tree.InstanceDefinition (InstanceDefinition)
import Semantic.Tree.MethodConcrete (Auto, Manual, Origin)
import qualified Semantic.Tree.Type as Semantic (Type)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)

data InstanceDefinition2 solve loeb layout stage scope where
  (:::) ::
    !(Annotation origin loeb layout stage scope) ->
    loeb (solve (InstanceDefinition origin layout stage scope)) ->
    InstanceDefinition2 solve loeb layout stage scope

infix 5 :::

instance (Show1 solve, Show1 loeb) => Show (InstanceDefinition2 solve loeb layout stage scope) where
  showsPrec d (annotation ::: definition) =
    showParen (d > 6) $
      showsPrec 6 annotation . showString " ::: " . showsPrec1 6 definition

instance (Functor solve, Functor loeb) => Shift0.Functor (InstanceDefinition2 solve loeb layout stage) where
  map = Shift.mapDefault

instance (Functor solve, Functor loeb) => Shift.Functor (InstanceDefinition2 solve loeb layout stage) where
  map category (annotation ::: definition) =
    Shift.map category annotation ::: fmap (fmap (Shift.map category)) definition

instance Traversable2 (InstanceDefinition2 solve) where
  traverse2 (Morph f) (annotation ::: definition) =
    (:::) <$> traverse2 (Morph f) annotation <*> getCompose (f definition)

instance (solve ~ Identity, loeb ~ Identity) => Connect (InstanceDefinition2 solve loeb) where
  connect (annotation ::: Identity (Identity definition)) =
    connect annotation ::: Identity (Identity (connect definition))
  seperate (annotation ::: Identity (Identity definition)) =
    seperate annotation ::: Identity (Identity (seperate definition))

type Annotation :: Origin -> (Type -> Type) -> Layout -> Stage -> Environment -> Type
data Annotation origin loeb layout stage scope where
  Standard :: loeb (Header stage scope) -> Annotation Manual loeb layout stage scope
  DerivedInstance :: loeb (Header stage scope) -> Annotation Auto loeb layout stage scope

instance (Show1 loeb) => Show (Annotation origin loeb layout stage scope) where
  showsPrec d = \case
    Standard header -> showParen (d > 10) $ showString "Standard " . showsPrec 11 header
    DerivedInstance header -> showParen (d > 10) $ showString "DerivedInstance " . showsPrec 11 header

instance (Functor loeb) => Shift0.Functor (Annotation origin loeb layout stage) where
  map = Shift.mapDefault

instance (Functor loeb) => Shift.Functor (Annotation origin loeb layout stage) where
  map category = \case
    Standard header -> Standard (Shift.map category <$> header)
    DerivedInstance header -> DerivedInstance (Shift.map category <$> header)

instance Connect (Annotation origin loeb) where
  connect = \case
    Standard header -> Standard header
    DerivedInstance header -> DerivedInstance header
  seperate = \case
    Standard header -> Standard header
    DerivedInstance header -> DerivedInstance header

instance Traversable2 (Annotation origin) where
  traverse2 (Morph f) = \case
    Standard header -> Standard <$> getCompose (f header)
    DerivedInstance header -> DerivedInstance <$> getCompose (f header)

data Header stage scope = Header
  { parameters :: !(Strict.Vector (TypePattern Position stage scope)),
    prerequisites :: !(Constraints Position stage scope),
    -- | The parameters that are required to be particular types, once the
    -- instance is checked: see 'Core.Prerequisites'. Before that they are
    -- among what is required.
    equated :: !(Strict.Vector (Int, Semantic.Type Position stage (Local ':+ scope))),
    -- | What the instance requires with evidence, once it is checked. For
    -- most instances this is what was written. For one from a @deriving@
    -- clause nothing was written, and it is worked out.
    needed :: !(Inferred Core.Constraints stage scope),
    -- | Whether what is required was not written, but is the class of each
    -- parameter it can be asked of: an instance from a @deriving@ clause.
    lenient :: !Bool
  }
  deriving (Show)

instance Shift0.Functor (Header stage) where
  map = Shift.mapDefault

instance Shift.Functor (Header stage) where
  map category Header {parameters, prerequisites, equated, needed, lenient} =
    Header
      { parameters = Shift.map category <$> parameters,
        prerequisites = Shift.map category prerequisites,
        needed = Shift.map category needed,
        equated = fmap (\(index, typex) -> (index, Shift.map (Shift.Over category) typex)) equated,
        lenient
      }
