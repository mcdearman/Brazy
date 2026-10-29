module Semantic.Check.Go.Definition3 where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constructor (ConstructorF (..), Shape (..))
import qualified Core.Tree.Data (Data (..), DefinitionF (..))
import Core.Tree.TypeDeclaration (assumeData)
import Data.Foldable (toList)
import qualified Core.Builtin as Builtin
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Binding.Type as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Table.Type as Type
import qualified Semantic.Shift as Shift
import qualified Semantic.Tree.Alternative as Alternative (Alternative (..))
import qualified Semantic.Tree.Body as Body (Body (..))
import qualified Semantic.Tree.Expression as Expression (Expression (..))
import qualified Semantic.Tree.Pattern as Pattern (Bindings (..), Field (..), Pattern (..))
import qualified Semantic.Tree.RightHandSide as RightHandSide (RightHandSide (..))
import qualified Semantic.Check.Go.Definition2 as Definition2
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), GroupTerm, Local)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Definition as Clauses (Definition (..))
import Semantic.Tree.Definition2 (Annotated, Inferred)
import qualified Semantic.Tree.Definition2 as Whole (Definition2 (..))
import qualified Semantic.Tree.Definition3 as Semantic (Definition3 (..))
import qualified Semantic.Tree.Function as Function (Function (..))
import qualified Semantic.Tree.Definition3 as Solved (Definition3 (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

checkManual ::
  Position ->
  Context s (Local ':+ scopes) ->
  Unify.Type s (Local ':+ scopes) ->
  Semantic.Definition3 Annotated Group Resolve scopes ->
  ST s (Unify.Solve s (Solved.Definition3 Annotated Group Check (Local ':+ scopes)))
checkManual position context typex (Semantic.Label info definition) = do
  definition <- Definition2.checkManual position context typex definition
  pure $ Solved.Label info <$> definition

checkAuto ::
  Position ->
  Context s scopes ->
  Unify.Type s scopes ->
  Semantic.Definition3 Inferred Group Resolve scopes ->
  ST s (Unify.Solve s (Solved.Definition3 Inferred Group Check scopes))
checkAuto position context typex (Semantic.Label info definition) = do
  definition <- Definition2.checkAuto position context typex definition
  pure $ Solved.Label info <$> definition

-- |
-- Whether a definition is of a function: whether it is written with
-- something it is given.
functional :: Semantic.Definition3 mark layout stage scope -> Bool
functional (Semantic.Label _ definition) = case definition of
  Whole.Definition definition -> case definition of
    Clauses.Definition {definition = Function.Bound {}} -> True
    Clauses.Alternative {definition = Function.Bound {}} -> True
    _ -> False
  _ -> False

-- |
-- Whether a definition is a @case@, or a function by cases, that takes
-- apart a value of a type whose constructors say what type they make.
indexed :: Context s scope -> Semantic.Definition3 mark layout Resolve (GroupTerm ':+ scope) -> ST s Bool
indexed Context {typeEnvironment} (Semantic.Label _ definition) = case definition of
  Whole.Definition
    Clauses.Definition
      { definition =
          Function.Plain
            { rightHandSide = RightHandSide.RightHandSide {body = Body.Body {body}}
            }
      } -> case body of
      Expression.Case {cases} -> anyM alternative (toList cases)
      Expression.LambdaCase {cases} -> anyM alternative (toList cases)
      _ -> pure False
  _ -> pure False
  where
    anyM check = \case
      [] -> pure False
      first : rest -> do
        found <- check first
        if found then pure True else anyM check rest
    alternative Alternative.Alternative {parameter} = anyM constructor (constructors parameter)
    constructors = \case
      Pattern.Match {match} -> case match of
        Pattern.Constructor {constructor, patterns} -> constructor : concatMap constructors (toList patterns)
        Pattern.Record {constructor, fields} ->
          constructor : concat [constructors field | Pattern.Field {field} <- toList fields]
        Pattern.List {items} -> concatMap constructors (toList items)
        _ -> []
      Pattern.Signature {patternx} -> constructors patternx
      Pattern.Wildcard {} -> []
    -- The patterns are two scopes in: that of the definitions this one is
    -- defined with, and that of what it defines for itself. A type that
    -- is defined in the second is not looked for.
    constructor Constructor.Index {typeIndex} = case Shift.partialUnshift Nothing typeIndex >>= Shift.partialUnshift Nothing of
      Nothing -> pure False
      Just typeIndex -> do
        let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
        Core.Tree.Data.Data {definition = Core.Tree.Data.Definition {constructors = declared}} <- Builtin.index pure get typeIndex
        pure $ any telling declared
    telling Core.Tree.Constructor.Constructor {shape} = case shape of
      Core.Tree.Constructor.Ordinary -> False
      Core.Tree.Constructor.Indexed {} -> True
