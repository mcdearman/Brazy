module Semantic.Check.Temporary.Definition3 where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constructor (Constructor (..), Shape (..))
import qualified Core.Tree.Data (Data (..))
import Core.Tree.TypeDeclaration (assumeData)
import Data.Foldable (toList)
import qualified Core.Builtin as Builtin
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Table.Type as Type
import qualified Semantic.Shift as Shift
import qualified Semantic.Tree.Alternative as Alternative (Alternative (..))
import qualified Semantic.Tree.Body as Body (Body (..))
import qualified Semantic.Tree.Expression as Expression (Expression (..))
import qualified Semantic.Tree.Pattern as Pattern (Pattern (..))
import qualified Semantic.Tree.PatternField as PatternField (Field (..))
import qualified Semantic.Tree.RightHandSide as RightHandSide (RightHandSide (..))
import Semantic.Check.Temporary.Definition2 (Definition2)
import qualified Semantic.Check.Temporary.Definition2 as Definition2
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), GroupTerm, Local)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Definition as Clauses (Definition (..))
import Semantic.Tree.Definition2 (Annotated, Inferred)
import qualified Semantic.Tree.Definition2 as Whole (Definition2 (..))
import Semantic.Tree.Definition3 (Info)
import qualified Semantic.Tree.Definition3 as Semantic (Definition3 (..))
import qualified Semantic.Tree.Function as Function (Function (..))
import qualified Semantic.Tree.Definition3 as Solved (Definition3 (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Definition3 mark s scope where
  Label :: !(Info source) -> !(Definition2 source mark s scope) -> Definition3 mark s scope

checkManual ::
  Context s (Local ':+ scopes) ->
  Unify.Type s (Local ':+ scopes) ->
  Semantic.Definition3 Annotated Group Resolve scopes ->
  ST s (Definition3 Annotated s (Local ':+ scopes))
checkManual context typex (Semantic.Label info definition) = do
  definition <- Definition2.checkManual context typex definition
  pure $ Label info definition

checkAuto ::
  Context s scopes ->
  Unify.Type s scopes ->
  Semantic.Definition3 Inferred Group Resolve scopes ->
  ST s (Definition3 Inferred s scopes)
checkAuto context typex (Semantic.Label info definition) = do
  definition <- Definition2.checkAuto context typex definition
  pure $ Label info definition

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
      Pattern.Constructor {constructor, patterns} -> constructor : concatMap constructors (toList patterns)
      Pattern.Record {constructor, fields} ->
        constructor : concat [constructors patternx | PatternField.Field {patternx} <- toList fields]
      Pattern.List {items} -> concatMap constructors (toList items)
      _ -> []
    -- The patterns are two scopes in: that of the definitions this one is
    -- defined with, and that of what it defines for itself. A type that
    -- is defined in the second is not looked for.
    constructor (Constructor.Index typeIndex _) = case Shift.partialUnshift Nothing typeIndex >>= Shift.partialUnshift Nothing of
      Nothing -> pure False
      Just typeIndex -> do
        let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
        Core.Tree.Data.Data {constructors = declared} <- Builtin.index pure get typeIndex
        pure $ any telling declared
    telling Core.Tree.Constructor.Constructor {shape} = case shape of
      Core.Tree.Constructor.Ordinary -> False
      Core.Tree.Constructor.Indexed {} -> True

solve :: Position -> Definition3 mark s scope -> Unify.Solve s (Solved.Definition3 mark Group Check scope)
solve position (Label info definition) = do
  definition <- Definition2.solve position definition
  pure $ Solved.Label info definition
