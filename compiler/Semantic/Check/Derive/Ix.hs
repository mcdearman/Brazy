-- |
-- @Ix@, derived, by another type's instance. A type whose constructors hold
-- nothing has its places numbered as the constructors are, in the order
-- they are written, and an @Int@'s instance is asked of the numbers. A type
-- of one constructor has the places of the tuple of what it holds, and the
-- tuple's instance is asked of that: the last of what it holds changes
-- fastest, as the report has it.
module Semantic.Check.Derive.Ix
  ( range,
    index,
    inRange,
  )
where

import {-# SOURCE #-} qualified Builtin.Functor as Functor
import {-# SOURCE #-} qualified Builtin.Ix as Ix
import Control.Monad.ST (ST)
import Core.Temporary.Definition (Definition (..))
import qualified Core.Temporary.Definition as Definition
import Core.Temporary.Function (Function (..))
import Core.Temporary.Pattern (Pattern (..))
import qualified Core.Temporary.Pattern as Pattern
import Core.Temporary.RightHandSide (RightHandSide (..))
import qualified Core.Tree.Class as Class
import qualified Core.Tree.ConstructorInfo as Core.ConstructorInfo
import Core.Tree.Data (Data)
import Core.Tree.EntryInfo (EntryInfo (..))
import Core.Tree.Evidence (Evidence, EvidenceF (Variable, instanciation, variable))
import Core.Tree.Expression (Expression)
import qualified Core.Tree.Expression as Expression
import Core.Tree.Instanciation (InstanciationF (..))
import qualified Core.Tree.Type as Core
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import Data.Text.Lazy.Builder (fromString)
import qualified Data.Vector.Strict as Strict.Vector
import Error (cannotDerive)
import Semantic.Check.Instanciate.Constructor (ConstructorInstance (..))
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.Instanciate.Data (DataInstance (..))
import qualified Semantic.Check.Derive.Enum as Enum
import Semantic.Check.Instanciate.Entry (EntryInstance (..))
import Semantic.Check.Info.Constructor (ConstructorInfo (..))
import Semantic.Check.Simple.Data (instanciateApart)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Check.Temporary.Pattern as Apart (within)
import qualified Semantic.Index.Constructor as Constructor (Index (..), tuple)
import qualified Semantic.Index.Evidence as Index.Evidence
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.MethodConcrete (Auto, MethodConcrete (Generated))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

-- |
-- How a type's places are found by another type's: the evidence for that
-- type's instance, a function to that type from the type, and one back.
data Through scope = Through
  { evidence :: Evidence scope,
    to :: Expression scope,
    back :: Expression scope
  }

-- |
-- What a tuple of so many things is matched with or made with. A tuple
-- holds what it holds lazily.
tupleInfo :: Int -> ConstructorInfo scope
tupleInfo count =
  ConstructorInfo
    { entries = Strict.Vector.replicate count EntryInfo {strict = Core.Constructor Type2.Lazy}
    }

tuple :: [Expression scope] -> Expression scope
tuple elements =
  Expression.Constructor
    { constructor = Constructor.tuple (length elements),
      arguments = Strict.Vector.fromList elements,
      constructorInfo =
        Core.ConstructorInfo.ConstructorInfo
          { entries =
              Strict.Vector.replicate (length elements) EntryInfo {strict = Core.Constructor Type2.Lazy}
          }
    }

-- |
-- A tuple of so many things, matched, with each thing named by its place.
matchTuple :: Int -> Pattern scope
matchTuple count =
  Match
    { irrefutable = False,
      match =
        Pattern.Constructor
          { constructor = Constructor.tuple count,
            patterns = Strict.Vector.replicate count Wildcard,
            constructorInfo = tupleInfo count
          }
    }

through ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (Through (Local ':+ scope)))
through context position typeIndex datax = do
  datay@DataInstance {types, constructors} <- instanciateApart context position (shift datax)
  let unify place typex = Unify.unify context position typex (Core.Variable $ Local.Local place)
  sequence_ $ zipWith unify [0 ..] (toList types)
  let nullary ConstructorInstance {entries} = null entries
  case toList constructors of
    found
      | not (null found),
        all nullary found -> do
          back <- Enum.toEnum' position (shift typeIndex) constructors
          pure $ do
            to <- Enum.fromEnum' (shift typeIndex) constructors
            back <- back
            pure
              Through
                { evidence = Variable {variable = Index.Evidence.Direct Type2.Ix Type2.Int, instanciation = Mono},
                  to,
                  back
                }
    [constructorx@ConstructorInstance {entries}]
      | length entries <= 5 -> do
          inside <- Apart.within context position datay (shift typeIndex) constructorx
          let held = [entry | EntryInstance {entry} <- toList entries]
              count = length held
              typex = case held of
                [one] -> one
                _ -> foldl Core.Call (Core.Constructor (Type2.Tuple count)) held
          evidence <- Unify.constrain inside position Type2.Ix typex
          pure $ do
            evidence <- Unify.solveEvidence position evidence
            info <- ConstructorInfo.solve $ ConstructorInstance.info constructorx
            let constructor = Constructor.Index {typeIndex = shift typeIndex, constructorIndex = 0}
                matched =
                  Match
                    { irrefutable = False,
                      match =
                        Pattern.Constructor
                          { constructor,
                            patterns = Strict.Vector.replicate count Wildcard,
                            constructorInfo = info
                          }
                    }
                held' = [Expression.patternVariableAt' place | place <- [0 .. count - 1]]
                function patternx done =
                  Definition.desugar
                    Definition {definition = Bound {patternx, body = Plain {plain = Done {done}}}}
                to = function matched $ case held' of
                  [one] -> one
                  _ -> tuple held'
                made = Expression.simplifyConstructorExact (shift constructor) (shift info) (Strict.Vector.fromList held')
                back = case count of
                  1 ->
                    Expression.Lambda $
                      Expression.simplifyConstructorExact
                        (shift constructor)
                        (shift info)
                        (Strict.Vector.singleton Expression.lambdaVariable)
                  _ -> function (matchTuple count) made
            pure Through {evidence, to, back}
    _ ->
      cannotDerive position $
        fromString "Ix: only a type of constructors that hold nothing, or of one constructor that holds at most five things, is"

-- |
-- The methods, at an instance of the other type.
method :: Method.Index scope -> Evidence scope -> Expression scope
method methodIndex evidence =
  Expression.Method
    { method = methodIndex,
      evidence,
      instanciation = Mono,
      methodInfo = Class.info Ix.definition
    }

-- |
-- > range (l, u) = map back (range (to l, to u))
range ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
range context position typeIndex datax = do
  found <- through context position typeIndex datax
  pure $ do
    Through {evidence, to, back} <- found
    let low = Expression.patternVariableAt' 0
        high = Expression.patternVariableAt' 1
        done =
          Expression.Method
            { method = Method.fmap,
              evidence = Variable {variable = Index.Evidence.Direct Type2.Functor Type2.List, instanciation = Mono},
              instanciation = Mono,
              methodInfo = Class.info Functor.definition
            }
            `Expression.call` shift back
            `Expression.call` ( method Method.ixRange (shift evidence)
                                  `Expression.call` tuple [shift to `Expression.call` low, shift to `Expression.call` high]
                              )
        body = Definition.desugar Definition {definition = Bound {patternx = matchTuple 2, body = Plain {plain = Done {done}}}}
    pure $ Generated $ Solved $ TypeLambda.mono body

-- |
-- > index (l, u) i = index (to l, to u) (to i)
-- > inRange (l, u) i = inRange (to l, to u) (to i)
index,
  inRange ::
    Context s (Local ':+ scope) ->
    Position ->
    Type2.Index scope ->
    Data scope ->
    ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
index = compared Index
inRange = compared InRange

-- | Which of the two is asked.
data Asked = Index | InRange

compared ::
  Asked ->
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
compared which context position typeIndex datax = do
  found <- through context position typeIndex datax
  pure $ do
    Through {evidence, to} <- found
    let methodIndex = case which of
          Index -> Method.ixIndex
          InRange -> Method.ixInRange
        to' = shift (shift to)
        low = shift (Expression.patternVariableAt' 0)
        high = shift (Expression.patternVariableAt' 1)
        done =
          method methodIndex (shift (shift evidence))
            `Expression.call` tuple [to' `Expression.call` low, to' `Expression.call` high]
            `Expression.call` (to' `Expression.call` Expression.patternVariable)
        body =
          Definition.desugar
            Definition
              { definition =
                  Bound
                    { patternx = matchTuple 2,
                      body =
                        Bound
                          { patternx = Pattern.Wildcard,
                            body = Plain {plain = Done {done}}
                          }
                    }
              }
    pure $ Generated $ Solved $ TypeLambda.mono body
