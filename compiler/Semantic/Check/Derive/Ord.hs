module Semantic.Check.Derive.Ord where

import Control.Monad.ST (ST)
import Core.Tree.Data (Data)
import Core.Tree.Expression (Direction (..), lambdaVariable)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Statements as Statements
import qualified Core.Tree.Type as Simple (TypeF (Variable))
import Semantic.Check.Info.Entry (EntryInfo (..))
import qualified Core.Tree.Type as Type
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context)
import Semantic.Check.Derive.Enum (fromEnum')
import Semantic.Check.Info.Constructor (ConstructorInfo (..))
import Semantic.Check.Instanciate.Constructor (ConstructorInstance (..))
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import Semantic.Check.Instanciate.Data (DataInstance (..))
import Semantic.Check.Instanciate.Entry (entry)
import Semantic.Check.Simple.Data (instanciateApart)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Check.Temporary.Pattern as Apart (within)
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.MethodConcrete (Auto, MethodConcrete (Generated))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Prelude hiding (Eq)

compare ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
compare context position typeIndex datax = do
  datay@DataInstance {types, constructors} <- instanciateApart context position (shift datax)
  let unify index typex = Unify.unify context position typex (Simple.Variable $ Local.Local index)
  sequence_ $ zipWith unify [0 ..] (toList types)
  let generate constructorIndex constructor@ConstructorInstance {entries} = do
        let types = entry <$> entries
        inside <- Apart.within context position datay (shift typeIndex) constructor
        evidence <- traverse (Unify.constrain inside position Type2.Ord) types
        pure $ do
          evidence <- traverse (Unify.solveEvidence position) evidence
          info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
          let index =
                Constructor.Index
                  { typeIndex,
                    constructorIndex
                  }
          pure $ case info of
            ConstructorInfo {entries} ->
              Statements.Bind
                { constructor = shift $ shift $ shift index,
                  entryCount = length entries,
                  check = shift $ lambdaVariable,
                  thenx =
                    Statements.Bind
                      { constructor = shift $ shift $ shift $ shift index,
                        entryCount = length entries,
                        check = shift $ lambdaVariable,
                        thenx =
                          Statements.Done
                            { done =
                                let compare index evidence
                                      | EntryInfo {levity} <- entries Strict.Vector.! index,
                                        levity <- shift $ shift levity =
                                          Expression.compare
                                            (shift $ shift $ shift $ shift evidence)
                                            (shift $ Expression.patternVariableAt levity index)
                                            (Expression.patternVariableAt (shift levity) index)
                                 in foldr
                                      Expression.orderingCombine
                                      Expression.orderingEqual
                                      (zipWith compare [0 ..] (toList evidence))
                            }
                      }
                }
            Holding {} -> error "a derived instance of a constructor that holds evidence"
            Newtype ->
              Statements.Done
                { done =
                    Expression.compare
                      (shift $ shift $ Strict.Vector.head evidence)
                      Expression.Newtype
                        { constructor = shift $ shift $ shift index,
                          argument = shift $ lambdaVariable,
                          direction = Destruct
                        }
                      Expression.Newtype
                        { constructor = shift $ shift $ shift $ index,
                          argument = lambdaVariable,
                          direction = Destruct
                        }
                }
  definitions <- sequence $ zipWith generate [0 ..] (toList constructors)
  pure $ do
    definitions <- sequence definitions
    fromEnum <- shift . shift <$> fromEnum' (shift typeIndex) constructors
    body <-
      pure $
        Expression.Lambda $
          Expression.Lambda $
            if null definitions
              then Expression.orderingEqual
              else
                Expression.Join
                  { statements =
                      let branch left right = Statements.Branch {left, right}
                          done =
                            Statements.Done
                              { done =
                                  Expression.compareInt
                                    (Expression.call fromEnum $ shift Expression.lambdaVariable)
                                    (Expression.call fromEnum Expression.lambdaVariable)
                              }
                       in foldr branch done definitions
                  }
    pure $ Generated $ Solved $ TypeLambda.mono Type.lazy body
