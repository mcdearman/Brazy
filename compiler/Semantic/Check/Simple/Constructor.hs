module Semantic.Check.Simple.Constructor where

import Control.Monad.ST (ST)
import Core.Substitute (substituteType)
import Core.Tree.Constructor (Constructor (..), Shape (..))
import qualified Core.Tree.Type as Core
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import qualified Semantic.Check.EntryInstance as EntryInstance
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

instanciate ::
  Position ->
  Brand ->
  Strict.Vector (Unify.Type s scope) ->
  Constructor (Local ':+ scope) ->
  ST s (ConstructorInstance s scope)
instanciate position brand fresh Constructor {name, syntax, entries, shape} = case shape of
  Ordinary ->
    pure
      ConstructorInstance
        { brand,
          name,
          syntax,
          entries = EntryInstance.instanciate position fresh <$> entries,
          result = fresh
        }
  -- The constructor's own variables are unknowns, of kinds that are not
  -- known either: what they are is found from how the constructor is used.
  Indexed {variables, arguments} -> do
    own <- Strict.Vector.replicateM variables $ do
      level <- Unify.fresh Core.universe
      kind <- Unify.fresh (Core.typeWith level)
      Unify.fresh kind
    pure
      ConstructorInstance
        { brand,
          name,
          syntax,
          entries = EntryInstance.instanciate position own <$> entries,
          result = substituteType (Strict.Vector.toLazy own) <$> arguments
        }
