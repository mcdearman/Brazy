-- |
-- Checking a whole program: the declarations of every module go in one
-- table of cells tied together (see "Graph.Topological"), so a declaration
-- is checked when something first needs it, whatever module it is in. Each
-- module's @default@ declaration says what its types that are never worked
-- out may be taken to be.
module Semantic.Check.Go.Module (Module (..), check) where

import Control.Monad.ST (ST)
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)
import Graph.Topological (Loeb (..), loeb)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context (..), globalBindings)
import qualified Semantic.Default as Default
import qualified Semantic.Check.Go.Declarations as Declarations
import Semantic.Check.Go.Definition4 (now)
import Semantic.Functor2 (Proper (..))
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Index.Link.Type as Link.Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Global)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Declaration (Declaration)
import Semantic.Tree.Declarations (Declarations (..))
import Semantic.Tree.Instance (Instance)
import Semantic.Tree.Module (Module (..), ModuleSet (..))
import Semantic.Tree.TypeDeclaration (TypeDeclaration)
import qualified Semantic.Tree.TypeDeclaration as TypeDeclaration (source)
import Prelude hiding (Functor)

lookupTerm ::
  Proper ModuleSet Group Check Global (ST s) ->
  Term.Link Locality.Global ->
  Declaration Identity Void Locality.Global (ST s) Group Check Global
lookupTerm (Proper (ModuleSet modules)) (Term.Global global local) =
  case modules Vector.! global of
    Module {declarations = Declarations {terms}} -> terms Vector.! local

lookupType ::
  Proper ModuleSet Group Check Global (ST s) ->
  Link.Type.Link Locality.Global ->
  TypeDeclaration Locality.Global (ST s) Group Check Global
lookupType (Proper (ModuleSet modules)) (Link.Type.Global global local) =
  case modules Vector.! global of
    Module {declarations = Declarations {types}} -> types Vector.! local

lookupDataInstance ::
  Int ->
  Proper ModuleSet Group Check Global (ST s) ->
  Int ->
  Type2.Index Global ->
  Instance Identity (ST s) Group Check Global
lookupDataInstance global (Proper (ModuleSet modules)) local index =
  case modules Vector.! global of
    Module {declarations = Declarations {dataInstances}} -> dataInstances Vector.! local Map.! index

lookupClassInstance ::
  Int ->
  Proper ModuleSet Group Check Global (ST s) ->
  Int ->
  Type2.Index Global ->
  Instance Identity (ST s) Group Check Global
lookupClassInstance global (Proper (ModuleSet modules)) local index =
  case modules Vector.! global of
    Module {declarations = Declarations {classInstances}} -> classInstances Vector.! local Map.! index

globals ::
  Vector (Module Identity Group Resolve Global) ->
  Proper ModuleSet Group Check Global (ST s) ->
  Context s Global
globals written (Proper (ModuleSet modules)) = globalBindings (sources <$> written) modules
  where
    sources Module {declarations = Declarations {types}} = TypeDeclaration.source <$> types

checkModule ::
  Vector (Module Identity Group Resolve Global) ->
  Int ->
  Module Identity Group Resolve Global ->
  Module (Topological.Formula (Proper ModuleSet Group Check Global) s) Group Check Global
checkModule written global Module {name, declarations, defaultDeclaration} =
  Module
    { name,
      defaultDeclaration,
      declarations =
        Declarations.checkImpl
          Link.Type.unglobal
          (Term.Global global)
          (Link.Type.Global global)
          now
          lookupTerm
          lookupType
          (lookupDataInstance global)
          (lookupClassInstance global)
          (\set -> (globals written set) {defaulting = listed})
          declarations
    }

      where
        -- The types the module's default declaration lists, or the
        -- report's when it has none.
        listed = case defaultDeclaration of
          Nothing -> Default.standard
          Just (_, types) -> types

check :: Vector (Module Identity Group Resolve Global) -> Vector (Module Identity Group Check Global)
check modules =
  Vector.map listable $
    runModuleSet $ runProper $ loeb $ Loeb $ Proper $ ModuleSet $ Vector.imap (checkModule modules) modules
  where
    -- What a default declaration lists is known to be types it can list
    -- once the module is checked, whether or not anything is taken to be
    -- one. It cannot be looked at while modules are resolved: what the
    -- names in it are is what is being worked out then.
    listable modulex@Module {defaultDeclaration} = case defaultDeclaration of
      Just (_, types) -> foldr seq () types `seq` modulex
      Nothing -> modulex
