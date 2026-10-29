module Semantic.Check.Derive.Show where

import {-# SOURCE #-} qualified Builtin.Show as Show
import Control.Monad.ST (ST)
import qualified Core.Tree.Class as Class
import Core.Tree.Data (Data)
import Core.Tree.Evidence (Evidence)
import Core.Tree.Expression (Direction (..), Expression)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Instanciation as Instanciation
import qualified Core.Tree.Statements as Statements
import qualified Core.Tree.Type as Type
import qualified Core.Tree.Type as Simple (TypeF (Variable))
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromString, fromText, toLazyText)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context)
import Semantic.Check.Info.Constructor (ConstructorInfo (..))
import qualified Semantic.Check.Info.Constructor as Semantic
import Semantic.Check.Instanciate.Constructor (ConstructorInstance (..))
import Semantic.Check.Info.Entry (EntryInfo (..))
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import Semantic.Check.Instanciate.Data (DataInstance (..))
import Semantic.Check.Instanciate.Entry (entry)
import Semantic.Check.Simple.Data (instanciateApart)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Check.Temporary.Pattern as Apart (within)
import qualified Semantic.Index.Constructor as Constructor (Index (..), cons)
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Constructor as Constructor (Syntax (..))
import Semantic.Tree.MethodConcrete (Auto, MethodConcrete (Generated))
import qualified Semantic.Unify as Unify
import Syntax.Lexer (runConstructorIdentifier, runConstructorSymbol, runVariableIdentifier, runVariableSymbol)
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity (..))
import qualified Syntax.Variable as Variable
import Prelude hiding (Eq)

printConstructor :: Bool -> Variable.Constructor -> Builder
printConstructor infixed = \case
  Variable.ConstructorIdentifier name -> open <> fromText (runConstructorIdentifier name) <> close
    where
      open =
        fromString $
          if infixed
            then
              "`"
            else ""
      close =
        fromString $
          if infixed
            then
              "`"
            else ""
  Variable.ConstructorSymbol name -> open <> fromText (runConstructorSymbol name) <> close
    where
      open =
        fromString $
          if infixed
            then
              ""
            else "("
      close =
        fromString $
          if infixed
            then
              ""
            else ")"

printField :: Variable.Variable -> Builder
printField = \case
  Variable.VariableIdentifier name -> fromText (runVariableIdentifier name)
  Variable.VariableSymbol name -> open <> fromText (runVariableSymbol name) <> close
    where
      open = fromString "("
      close = fromString ")"

showConstructor ::
  Constructor.Syntax ->
  Variable.Constructor ->
  Strict.Vector.Vector (Evidence scope) ->
  Expression scope ->
  (Int -> Expression scope) ->
  Expression scope
showConstructor syntax name evidence numberVariable constructorVariable
  | null evidence = show [] []
  | otherwise = show [bracket '('] [bracket ')']
  where
    letter character = Expression.Lambda $ before character
    -- A bracket, which is written when the constructor is where it needs
    -- brackets. Each of the two finds that out for itself, so that what is
    -- between them is written once: it was written twice, with brackets and
    -- without, and that was half of every instance.
    bracket character =
      Expression.Lambda $
        Expression.ifx
          ( Expression.lessThenEqualInt
              (shift numberVariable)
              (Expression.int precedence)
          )
          Expression.lambdaVariable
          (before character)
    before character =
        Expression.simplifyConstructorExact
          Constructor.cons
          ConstructorInfo
            { entries =
                Strict.Vector.fromList
                  [ EntryInfo {levity = Type.Constructor Type2.Lazy},
                    EntryInfo {levity = Type.Constructor Type2.Lazy}
                  ]
            }
          ( Strict.Vector.fromList
              [ Expression.Character {character},
                Expression.lambdaVariable
              ]
          )
    space = letter ' '
    identifier infixed =
      map letter $ unpack $ toStrict $ toLazyText $ printConstructor infixed name
    item = itemAt (precedence + 1)
    -- What a constructor holds is shown in brackets if it needs them to be
    -- an argument. What a field is equal to never does.
    itemAt level index evidence =
      Expression.Method
        { method = Method.showsPrec,
          evidence,
          instanciation = Instanciation.monoLazy,
          constraintCount = Class.info $ Show.definition
        }
        `Expression.call` Expression.int level
        `Expression.call` constructorVariable index
    sections start end = case syntax of
      Constructor.Standard ->
        concat
          [ start,
            identifier False,
            concat $ map (\item -> [space, item]) $ zipWith item [0 ..] $ toList evidence,
            end
          ]
      Constructor.Infix {} ->
        concat
          [ start,
            [item 0 (evidence Strict.Vector.! 0)],
            [space],
            identifier True,
            [space],
            [item 1 (evidence Strict.Vector.! 1)],
            end
          ]
      Constructor.Record fields ->
        concat
          [ start,
            identifier False,
            [space],
            [letter '{'],
            concat $
              intersperse [letter ',', space] $
                zipWith3 field [0 ..] (toList fields) (toList evidence),
            [letter '}'],
            end
          ]
        where
          field index name evidence =
            concat
              [ map letter $ unpack $ toStrict $ toLazyText $ printField name,
                [space],
                [letter '='],
                [space],
                [itemAt 0 index evidence]
              ]
    precedence = case syntax of
      Constructor.Standard -> 10
      Constructor.Infix Fixity {precedence} -> precedence
      Constructor.Record {} -> 10
    show start end =
      Expression.Lambda
        { body =
            let main = map shift (sections start end)
             in foldr Expression.call Expression.lambdaVariable main
        }

showsPrec ::
  forall s scope layout.
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
showsPrec context position typeIndex datax = do
  datay@DataInstance {types, constructors} <- instanciateApart context position (shift datax)
  let unify index typex = Unify.unify context position typex (Simple.Variable $ Local.Local index)
  sequence_ $ zipWith unify [0 ..] (toList types)
  let generate constructorIndex constructor@ConstructorInstance {name, syntax, entries} = do
        let types = entry <$> entries
        inside <- Apart.within context position datay (shift typeIndex) constructor
        evidence <- traverse (Unify.constrain inside position Type2.Show) types
        pure $ do
          evidence <- traverse (Unify.solveEvidence context position) evidence
          info <- ConstructorInfo.solve context $ ConstructorInstance.info constructor
          let constructor = Constructor.Index {typeIndex, constructorIndex}
          pure $ case info of
            Semantic.ConstructorInfo {entries} ->
              Statements.Bind
                { constructor = shift $ shift $ shift constructor,
                  entryCount = length entries,
                  check = Expression.lambdaVariable,
                  thenx =
                    Statements.Done
                      { done =
                          let field index
                                | EntryInfo {levity} <- entries Strict.Vector.! index =
                                    Expression.patternVariableAt (shift $ shift levity) index
                           in showConstructor
                                syntax
                                name
                                (shift . shift . shift <$> evidence)
                                (shift $ shift $ Expression.lambdaVariable)
                                field
                      }
                }
            Semantic.Holding {} -> error "a derived instance of a constructor that holds evidence"
            Semantic.Newtype {} ->
              Statements.Done
                { done =
                    showConstructor
                      syntax
                      name
                      (shift . shift <$> evidence)
                      (shift Expression.lambdaVariable)
                      $ const
                      $ Expression.Newtype
                        { constructor = shift $ shift $ shift constructor,
                          argument = Expression.lambdaVariable,
                          direction = Destruct
                        }
                }
  definitions <- sequence $ zipWith generate [0 ..] (toList constructors)
  pure $ do
    definitions <- sequence definitions
    let branch left right = Statements.Branch {left, right}
        body =
          Expression.Lambda
            { body =
                Expression.Lambda
                  { body =
                      Expression.Join
                        { statements =
                            if null definitions
                              then Statements.Bottom
                              else foldr1 branch definitions
                        }
                  }
            }

    pure $ Generated $ Solved $ TypeLambda.mono Type.lazy body
