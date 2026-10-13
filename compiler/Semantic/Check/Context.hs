module Semantic.Check.Context where

import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import Core.Substitute (logicalType)
import qualified Core.Tree.Type as Core
import Core.Tree.TypeDeclaration (assumeData)
import qualified Data.Kind
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Semantic.Check.DataInstance as DataInstance
import Semantic.Check.LocalBinding (Equality, LocalBinding, shiftEquality)
import qualified Semantic.Check.LocalBinding as LocalBinding
import Semantic.Check.Simple.Data as Simple.Data (instanciateMark)
import Semantic.Check.TermBinding (TermBinding (..))
import qualified Semantic.Check.TermBinding as TermBinding
import Semantic.Check.TypeBinding (TypeBinding (..))
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Link.Type as Type (Link)
import qualified Semantic.Index.Link.Type as Type.Link
import qualified Semantic.Index.Table.Local as Local
import qualified Semantic.Index.Table.Term as Term
import qualified Semantic.Index.Table.Type as Type (Map (..), Table (..), map, (!))
import qualified Semantic.Index.Table.Type as Type.Table
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Type as Label (TypeBinding)
import qualified Semantic.Label.Context as Label (Context (..))
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope (Declaration, Global, GroupTerm, GroupType)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import Semantic.Tree.Declarations (Declarations (..))
import Semantic.Tree.Module (Module (..))
import qualified Semantic.Tree.Type as Type (Synonym (..))
import Semantic.Tree.TypeDeclaration (TypeDeclaration (TypeDeclaration, definition))
import qualified Semantic.Tree.TypeDefinition2 as TypeDefinition2
import qualified Semantic.Tree.TypeGroup as TypeGroup
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

type Context :: Data.Kind.Type -> Environment -> Data.Kind.Type
data Context s scope = Context
  { termEnvironment :: !(Term.Table (TermBinding s) scope),
    localEnvironment :: !(Local.Table (LocalBinding s) scope),
    typeEnvironment :: !(Type.Table (TypeBinding s) scope),
    -- | What type variables are known to be here, from the matches on
    -- constructors that this is inside.
    equalities :: ![Equality s scope]
  }

typeEnvironment_ = typeEnvironment

instance Shift.Unshift (Context s) where
  unshift Context {termEnvironment, localEnvironment, typeEnvironment, equalities} =
    Context
      { termEnvironment = Shift.unshift termEnvironment,
        localEnvironment = Shift.unshift localEnvironment,
        typeEnvironment = Shift.unshift typeEnvironment,
        -- What is known here is known further out as far as it can be
        -- said there.
        equalities = mapMaybe Unify.lowerEquality equalities
      }

globalBindings :: forall s. Vector (Module (ST s) Group Check Scope.Global) -> Context s Scope.Global
globalBindings modules =
  Context
    { termEnvironment = Term.Global $ termBindings <$> modules,
      localEnvironment = Local.Global,
      typeEnvironment = Type.Global $ typeBindings <$> modules,
      equalities = []
    }
  where
    termBindings Module {declarations = Declarations {terms}} =
      TermBinding.global <$> terms
    typeBindings
      Module {declarations = Declarations {types, typeExtras, dataInstances, classInstances}} =
        Vector.zipWith4 (TypeBinding.global lookup) types typeExtras dataInstances classInstances
    lookup ::
      Type.Link Locality.Global ->
      ST s (TypeGroup.Set Locality.Global Check Scope.Global)
    lookup (Type.Link.Global global local)
      | Module {declarations = Declarations {types}} <- modules Vector.! global,
        TypeDeclaration {definition = TypeDefinition2.Group group} <- types Vector.! local =
          do
            _ TypeGroup.:::: set <- group
            pure set
      | otherwise = error "bad lookup"

localBindings ::
  forall s scope.
  Context s scope ->
  Declarations
    (Unify.Solve s)
    (Unify.Logical s (Scope.Declaration ':+ scope))
    Locality.Local
    (ST s)
    Group
    Check
    (Scope.Declaration ':+ scope) ->
  Context s (Scope.Declaration ':+ scope)
localBindings
  Context {termEnvironment, localEnvironment, typeEnvironment, equalities}
  Declarations {terms, types, typeExtras, dataInstances, classInstances} =
    Context
      { termEnvironment = Term.Declaration termBindings termEnvironment,
        localEnvironment = Local.Declaration localEnvironment,
        typeEnvironment = Type.Declaration typeBindings typeEnvironment,
        equalities = shiftEquality <$> equalities
      }
    where
      termBindings = TermBinding.local <$> terms
      typeBindings = Vector.zipWith4 (TypeBinding.local lookup) types typeExtras dataInstances classInstances
      lookup ::
        Type.Link Locality.Local ->
        ST s (TypeGroup.Set Locality.Local Check (Scope.Declaration ':+ scope))
      lookup (Type.Link.Declaration index) = case types Vector.! index of
        TypeDeclaration {definition = TypeDefinition2.Group group} -> do
          _ TypeGroup.:::: set <- group
          pure set
        _ -> error "bad lookup"

groupTermBindings ::
  ST s () ->
  Vector (Unify.Type s scope) ->
  Context s scope ->
  Context s (Scope.GroupTerm ':+ scope)
groupTermBindings used declarations Context {termEnvironment, localEnvironment, typeEnvironment, equalities} =
  Context
    { termEnvironment = Term.GroupTerm termBindings termEnvironment,
      localEnvironment = Local.GroupTerm localEnvironment,
      typeEnvironment = Type.GroupTerm typeEnvironment,
      equalities = shiftEquality <$> equalities
    }
  where
    termBindings = TermBinding.group used <$> declarations

groupTypeBindings ::
  Vector Position ->
  Vector (Label.TypeBinding scope') ->
  Vector (Unify.Type s scope) ->
  Context s scope ->
  Context s (Scope.GroupType ':+ scope)
groupTypeBindings
  positions
  labels
  declarations
  Context {termEnvironment, localEnvironment, typeEnvironment, equalities} =
    Context
      { termEnvironment = Term.GroupType termEnvironment,
        localEnvironment = Local.GroupType localEnvironment,
        typeEnvironment = Type.GroupType typeBindings typeEnvironment,
        equalities = shiftEquality <$> equalities
      }
  where
    typeBindings = Vector.zipWith3 TypeBinding.group positions labels declarations

label :: Context s scope -> Label.Context scope
label Context {termEnvironment, localEnvironment, typeEnvironment} =
  Label.Context
    { terms = Term.map (error "term names are used in errors") termEnvironment,
      locals = Local.map (Local.Map LocalBinding.label) localEnvironment,
      types = Type.map (Type.Map TypeBinding.label) typeEnvironment
    }

lookupSynonym :: Context s scope -> Type2.Index scope -> ST s (Type.Synonym Check scope)
lookupSynonym Context {typeEnvironment} (Type2.Index index) = do
  let TypeBinding {synonym} = typeEnvironment Type.! index
  synonym
lookupSynonym _ _ = pure Type.NoSynonym

lookupKind ::
  Position ->
  Context s scope ->
  Type2.Index scope ->
  ST s (Core.TypeF (Unify.Logical s scope) scope)
lookupKind position context@Context {typeEnvironment} index = do
  let indexType index =
        case typeEnvironment Type.Table.! index of
          TypeBinding {kind} -> do
            kind <- kind
            case kind of
              TypeBinding.Rigid kind -> pure $ logicalType kind
              TypeBinding.Wobbly wobbly -> pure wobbly
      indexLift constructor@Constructor.Index {typeIndex} = do
        datax <- do
          let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
          datax <- Builtin.index pure get typeIndex
          Simple.Data.instanciateMark context position datax
        pure $ DataInstance.constructorFunction datax constructor
  Builtin.kind (pure . logicalType) indexType indexLift index
