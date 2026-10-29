module Semantic.Check.Binding.Type where

import Control.Monad.ST (ST)
import Core.Tree.Constraints (Prerequisites)
import qualified Core.Tree.Type as Core.Type
import qualified Core.Tree.Type as Simple (Type)
import qualified Core.Tree.TypeDeclaration as Core.TypeDeclaration
import qualified Core.Tree.TypeDeclaration as Simple (TypeDeclaration)
import qualified Core.Tree.TypeDeclarationExtra as Core.TypeDeclarationExtra
import {-# SOURCE #-} qualified Core.Tree.TypeDeclarationExtra as Simple (TypeDeclarationExtra)
import Core.Type.Functor (shiftLogical)
import qualified Core.Type.Functor as Core
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.NaturalTransformation (NaturalTransformation (..))
import Error (improperBindingGroup)
import Semantic.Connect (seperate)
import Semantic.Functor2 (traverse2)
import qualified Semantic.Index.Link.Type as Type (Link)
import qualified Semantic.Index.Link.Type as Type.Link
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Type as Label
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..), GroupType)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Instance as Semantic
import qualified Semantic.Tree.Synonym as Synonym
import qualified Semantic.Tree.Type as Type (Synonym (..))
import Semantic.Tree.TypeDeclaration (TypeDeclaration (..))
import qualified Semantic.Tree.TypeDeclaration as TypeDeclaration
import Semantic.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import Semantic.Tree.TypeDefinition2 (TypeDefinition2 (..))
import qualified Semantic.Tree.TypeGroup as TypeGroup
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import qualified Syntax.Variable as Qualifiers

data Kind s scope
  = Wobbly !(Unify.Type s scope)
  | Rigid !(Simple.Type scope)

instance Shift0.Functor (Kind s) where
  map category = \case
    Wobbly typex -> Wobbly (Core.mapLogical category typex)
    Rigid typex -> Rigid (Shift0.map category typex)

type TypeBinding :: Data.Kind.Type -> Environment -> Data.Kind.Type
data TypeBinding s scope = TypeBinding
  { label :: !(forall scope. Label.TypeBinding scope),
    kind :: ST s (Kind s scope),
    content :: ST s (Simple.TypeDeclaration scope),
    extra :: ST s (Unify.Solve s (Simple.TypeDeclarationExtra scope)),
    synonym :: ST s (Type.Synonym Check scope),
    -- | A name for a type as it was written, before it was checked. Types
    -- that are checked together look into it: they cannot ask what it was
    -- found to be if finding that waits for them.
    source :: !(Maybe (Synonym.Synonym Resolve scope)),
    dataInstances :: Map (Type2.Index scope) (Requires s scope),
    classInstances :: Map (Type2.Index scope) (Requires s scope)
  }

-- |
-- An instance, as much of it as finding one needs: what it requires, and
-- whether that was written or is worked out, as it is for an instance that
-- a @deriving@ clause asks for.
data Requires s scope = Requires
  { clause :: !Bool,
    prerequisites :: ST s (Prerequisites scope)
  }

mapRequires :: Shift.Category scope scope' -> Requires s scope -> Requires s scope'
mapRequires category Requires {clause, prerequisites} =
  Requires {clause, prerequisites = fmap (Shift.map category) prerequisites}

instance Shift0.Functor (TypeBinding s) where
  map category0 TypeBinding {label, kind, synonym, source, extra, content, dataInstances, classInstances} =
    TypeBinding
      { label,
        kind = fmap (Shift0.map category0) kind,
        synonym = fmap (Shift.map category) synonym,
        source = fmap (Shift.map category) source,
        content = fmap (Shift.map category) content,
        extra = fmap (Shift.map category) <$> extra,
        dataInstances = Map.map (mapRequires category) $ Shift.mapInstances category dataInstances,
        classInstances = Map.map (mapRequires category) $ Shift.mapInstances category classInstances
      }
    where
      category = Shift.lift category0

global ::
  ( Type.Link Locality.Global ->
    ST s (TypeGroup.Set Locality.Global Check Scope.Global)
  ) ->
  TypeDeclaration
    Locality.Global
    (ST s)
    Group
    Check
    Scope.Global ->
  ST s (Identity (TypeDeclarationExtra Group Check Scope.Global)) ->
  Map
    (Type2.Index Scope.Global)
    (Semantic.Instance Identity (ST s) Group Check Scope.Global) ->
  Map
    (Type2.Index Scope.Global)
    (Semantic.Instance Identity (ST s) Group Check Scope.Global) ->
  Maybe (Synonym.Synonym Resolve Scope.Global) ->
  TypeBinding s Scope.Global
global
  lookup
  typex@TypeDeclaration {kind}
  typeExtra
  dataInstances
  classInstances
  source =
    TypeBinding
      { label = TypeDeclaration.labelBinding Qualifiers.Local typex,
        kind = do
          kind <- kind
          case kind of
            Solved kind -> pure $ Rigid kind,
        content = do
          typex <- traverse2 (Morph $ Compose . fmap Identity) typex
          typex <- TypeDeclaration.ungroupM Type.Link.unglobal lookup typex
          pure $ Core.TypeDeclaration.simplify typex,
        extra = do
          typeExtra <- typeExtra
          pure $
            pure $
              Core.TypeDeclarationExtra.simplify $
                seperate $
                  runIdentity typeExtra,
        synonym = case typex of
          TypeDeclaration {definition = Synonym synonym} -> do
            _ Synonym.::: Synonym.SynonymBody {parameters, synonym} <- synonym
            pure $ Type.Synonym (length parameters) $ Core.Type.simplify synonym
          _ -> pure Type.NoSynonym,
        source,
        dataInstances = bindInstance <$> dataInstances,
        classInstances = bindInstance <$> classInstances
      }
    where
      bindInstance :: Semantic.Instance solve (ST s) layout Check scope -> Requires s scope
      bindInstance Semantic.Instance {clause, prerequisites} =
        Requires
          { clause,
            prerequisites = prerequisites >>= \case Solved prerequisites -> pure prerequisites
          }

local ::
  ( Type.Link Locality.Local ->
    ST s (TypeGroup.Set Locality.Local Check (Scope.Declaration ':+ scope))
  ) ->
  TypeDeclaration
    Locality.Local
    (ST s)
    Group
    Check
    (Scope.Declaration ':+ scope) ->
  ST s (Unify.Solve s (TypeDeclarationExtra Group Check (Scope.Declaration ':+ scope))) ->
  Map
    (Type2.Index (Scope.Declaration ':+ scope))
    (Semantic.Instance (Unify.Solve s) (ST s) Group Check (Scope.Declaration ':+ scope)) ->
  Map
    (Type2.Index (Scope.Declaration ':+ scope))
    (Semantic.Instance (Unify.Solve s) (ST s) Group Check (Scope.Declaration ':+ scope)) ->
  Maybe (Synonym.Synonym Resolve (Scope.Declaration ':+ scope)) ->
  TypeBinding s (Scope.Declaration ':+ scope)
local
  lookup
  typex@TypeDeclaration {kind}
  typeExtra
  dataInstances
  classInstances
  source =
    TypeBinding
      { label = TypeDeclaration.labelBinding Qualifiers.Local typex,
        kind = do
          kind <- kind
          case kind of
            Solved kind -> pure $ Rigid kind,
        content = do
          typex <- traverse2 (Morph $ Compose . fmap Identity) typex
          typex <- TypeDeclaration.ungroupM Type.Link.unlocal lookup typex
          pure $ Core.TypeDeclaration.simplify typex,
        extra = do
          typeExtra <- typeExtra
          pure $ Core.TypeDeclarationExtra.simplify . seperate <$> typeExtra,
        synonym = case typex of
          TypeDeclaration {definition = Synonym synonym} -> do
            _ Synonym.::: Synonym.SynonymBody {parameters, synonym} <- synonym
            pure $ Type.Synonym (length parameters) $ Core.Type.simplify synonym
          _ -> pure Type.NoSynonym,
        source,
        dataInstances = bindInstance <$> dataInstances,
        classInstances = bindInstance <$> classInstances
      }
    where
      bindInstance :: Semantic.Instance solve (ST s) layout Check scope -> Requires s scope
      bindInstance Semantic.Instance {clause, prerequisites} =
        Requires
          { clause,
            prerequisites = prerequisites >>= \case Solved prerequisites -> pure prerequisites
          }

group :: Position -> Label.TypeBinding scope -> Unify.Type s scopes -> TypeBinding s (GroupType ':+ scopes)
group position Label.TypeBinding {name, constructorNames} binding =
  TypeBinding
    { label = Label.TypeBinding {name, constructorNames},
      kind = pure $ Wobbly $ shiftLogical binding,
      content = abort,
      extra = abort,
      synonym = pure Type.NoSynonym,
      source = Nothing,
      dataInstances = abort,
      classInstances = abort
    }
  where
    abort :: a
    abort = improperBindingGroup position
