module Generate.Context where

import Control.Monad.ST (ST)
import qualified Core.Index.Table.Term as Term (Table (..), (!))
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Type2 as Type2
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Generate.Binding.Evidence as Evidence (Binding (..))
import qualified Generate.Binding.Term as Term (Binding (..), binding)
import qualified Generate.Binding.Term as Term.Binding
import qualified Generate.Binding.Type as Type (Binding (..))
import Generate.GlobalType (GlobalType (GlobalType))
import qualified Generate.GlobalType as GlobalType
import Generate.LocalType (LocalType (LocalType))
import qualified Generate.LocalType as LocalType
import qualified Generate.Mangle as Mangle
import Generate.Precontext (Precontext (Precontext))
import qualified Generate.Precontext as Precontext
import Generate.Variable (Variable (..))
import qualified Semantic.Index.Evidence0 as Evidence0 (Index)
import qualified Semantic.Index.Table.Evidence0 as Evidence0 (Table (..), (!))
import qualified Semantic.Index.Table.Type as Type (Table (..), (!))
import qualified Semantic.Index.Term as Term (Index)
import qualified Semantic.Index.Type as Type (Index)
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope

data Context s scope = Context
  { terms :: !(Term.Table Term.Binding scope),
    evidence :: !(Evidence0.Table Evidence.Binding scope),
    types :: !(Type.Table Type.Binding scope),
    unique :: !(STRef s [Text])
  }

start :: Precontext -> ST s (Context s Scope.Global)
start Precontext {terms, types} = do
  unique <- newSTRef Mangle.unique
  let globalType GlobalType {classInstances, dataInstances, strictness} =
        Type.Binding
          { classInstances = Global <$> classInstances,
            dataInstances = Global <$> dataInstances,
            strictness
          }
  pure
    Context
      { terms = Term.Global (fmap (Term.binding . Global) <$> terms),
        evidence = Evidence0.Global,
        types = Type.Global (fmap globalType <$> types),
        unique
      }

fresh :: Context s scope -> ST s Text
fresh Context {unique} = do
  list <- readSTRef unique
  writeSTRef unique $! tail list
  pure $ head list

localBindings ::
  Vector Text ->
  Vector (LocalType (Scope.Declaration ':+ scope)) ->
  Context s scope ->
  Context s (Scope.Declaration ':+ scope)
localBindings names instances Context {terms, evidence, types, unique} =
  Context
    { terms = Term.Declaration (Term.binding . Local <$> names) terms,
      evidence = Evidence0.Declaration evidence,
      types = Type.Declaration (go <$> instances) types,
      unique
    }
  where
    go LocalType {classInstances, dataInstances} =
      Type.Binding
        { classInstances = Map.map Local classInstances,
          dataInstances = Map.map Local dataInstances,
          -- A type of a local scope is a variable, and no data type.
          strictness = Vector.empty
        }

singleBinding :: Text -> Context s scope -> Context s (Scope.SimpleDeclaration ':+ scope)
singleBinding name Context {terms, evidence, types, unique} =
  Context
    { terms = Term.SimpleDeclaration Term.Binding {name = Local name, strict = False, delayed = True} terms,
      evidence = Evidence0.SimpleDeclaration evidence,
      types = Type.SimpleDeclaration types,
      unique
    }

-- |
-- A name for a value that is evaluated: what a bang pattern names.
strictBinding :: Text -> Context s scope -> Context s (Scope.SimpleDeclaration ':+ scope)
strictBinding name Context {terms, evidence, types, unique} =
  Context
    { terms = Term.SimpleDeclaration Term.Binding {name = Local name, strict = True, delayed = False} terms,
      evidence = Evidence0.SimpleDeclaration evidence,
      types = Type.SimpleDeclaration types,
      unique
    }

patternBindings ::
  Strict.Vector Text ->
  [Bool] ->
  Context s scope ->
  Context s (Scope.SimplePattern ':+ scope)
patternBindings names stricts Context {terms, evidence, types, unique} =
  Context
    { terms = Term.SimplePattern patterns terms,
      evidence = Evidence0.SimplePattern evidence,
      types = Type.SimplePattern types,
      unique
    }
  where
    patterns = Vector.fromList $ zipWith bind (Strict.Vector.toList names) stricts
    bind name strict =
      Term.Binding
        { name = Local name,
          strict,
          delayed = False
        }

-- |
-- Which of so many things a constructor holds it holds strictly, as its
-- data type says. What it does not say is held lazily: what a type of the
-- language's own holds, and the evidence a GADT constructor holds after the
-- rest.
held :: Context s scope -> Constructor.Index scope -> Int -> [Bool]
held Context {types} Constructor.Index {typeIndex, constructorIndex} count = take count (fields ++ repeat False)
  where
    fields = case typeIndex of
      Type2.Index index
        | Type.Binding {strictness} <- types Type.! index,
          Just constructor <- strictness Vector.!? constructorIndex ->
            constructor
      _ -> []

evidenceBindings ::
  Vector Text ->
  Context s scope ->
  Context s (Scope.Local ':+ scope)
evidenceBindings names Context {terms, evidence, types, unique} =
  Context
    { terms = Term.Local terms,
      evidence = Evidence0.Assumed (Evidence.Binding <$> names) evidence,
      types = Type.Local types,
      unique
    }

(!-) :: Context s scope -> Term.Index scope -> Term.Binding scope
Context {terms} !- index = terms Term.! index

(!=.) :: Context s scope -> Type.Index scope -> Type.Binding scope
Context {types} !=. index = types Type.! index

(!~) :: Context s scope -> Evidence0.Index scope -> Evidence.Binding scope
Context {evidence} !~ index = evidence Evidence0.! index
