-- |
-- Using a type that is already checked, in Core's form: make a fresh
-- instance of it, or bring its type variables into scope as rigid ones,
-- with what its constraints and their superclasses say is known of them.
module Semantic.Check.Scheme where

import Control.Monad (zipWithM)
import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Builtin as Builtin
import Core.Substitute (logicalType)
import qualified Core.Tree.Class as Class (Class (..))
import Core.Tree.Constraint (ConstraintF (..))
import Core.Tree.Constraints (Constraints, ConstraintsF (..))
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Core.Tree.Forall (Forall, ForallOver (..))
import qualified Core.Tree.Instanciation as Instanciation
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type (strict)
import Core.Tree.TypeDeclaration (assumeClass)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Order (orderWithInt)
import Semantic.Check.Context (Context (..), shiftDefaulting)
import Semantic.Check.Binding.Local (shiftEquality)
import qualified Semantic.Check.Binding.Local as LocalBinding
import Semantic.Check.Mask (Mask)
import qualified Semantic.Check.Binding.Type as TypeBinding
import qualified Semantic.Index.Evidence as Evidence (assumed)
import qualified Semantic.Index.Table.Local as Local
import qualified Semantic.Index.Table.Term as Term
import qualified Semantic.Index.Table.Type as Type (Table (..), (!))
import qualified Semantic.Label.Binding.Local as Label
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (shift)
import qualified Semantic.Unify as Unify
import Syntax.Lexer (variableIdentifier)
import Syntax.Position (Position)
import Syntax.Variable (VariableIdentifier)
import Prelude hiding (head)

instanciate :: Context s scope -> Position -> Forall scope -> ST s (Unify.Type s scope, Unify.Instanciation s scope)
instanciate context position = Unify.instanciate context position . logicalType

augmentNamed ::
  (Int -> VariableIdentifier) ->
  Position ->
  Strict.Vector (Type scope) ->
  Constraints scope ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augmentNamed name _ parameters constraints mask Context {termEnvironment, localEnvironment, typeEnvironment, equalities, defaulting}
  | parameters <- Strict.Vector.toLazy parameters = do
      let entailed classx arguments evidence = do
            Class.Class {constraints} <- do
              let get index = assumeClass <$> TypeBinding.content (typeEnvironment Type.! index)
              Builtin.index pure get classx
            children <- for (zip [0 ..] $ toList constraints) $
              \(index, Constraint {classx, arguments = arguments'}) ->
                entailed classx (arguments <> arguments') Evidence.Super {base = evidence, index}
            let root =
                  LocalBinding.Constraint
                    { arguments,
                      evidence
                    }
            pure $ (shift classx, [root]) : concat children
          collect index Constraint {classx, head, arguments} = do
            let evidence =
                  Evidence.Variable
                    { variable = Evidence.assumed index,
                      instanciation = Instanciation.Mono {levity = Type.strict}
                    }
            entail <- entailed classx arguments evidence
            pure (head, entail)
      collected <- case constraints of
        Constraints constraints -> zipWithM collect [0 ..] $ toList constraints
        Value {} -> pure []
      let ordered = orderWithInt (++) [] (length parameters) collected
          constraints = Vector.map (Map.fromListWith $ flip LocalBinding.combine) ordered
          rigid index typex constraints =
            LocalBinding.Rigid
              { label = Label.LocalBinding {name = name index},
                rigid = shift typex,
                constraints,
                mask
              }
          locals = Vector.izipWith rigid parameters constraints
      () <- pure $ foldr (\map () -> foldr seq () map) () constraints
      pure
        Context
          { termEnvironment = Term.Local termEnvironment,
            localEnvironment = Local.Local locals localEnvironment,
            typeEnvironment = Type.Local typeEnvironment,
            equalities = shiftEquality <$> equalities,
            defaulting = shiftDefaulting defaulting
          }

augment ::
  Position ->
  Strict.Vector (Type scope) ->
  Constraints scope ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augment = augmentNamed name
  where
    name i = variableIdentifier $ pack $ "__rigid_" ++ show i

augmentForall ::
  Position ->
  ForallOver typex Void scope ->
  Mask ->
  Context s scope ->
  ST s (Context s (Local ':+ scope))
augmentForall position ForallOver {parameters, constraints} =
  augment position parameters constraints
