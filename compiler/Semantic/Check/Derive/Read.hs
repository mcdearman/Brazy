-- |
-- @Read@, derived. A value is read as it is shown: each constructor is an
-- alternative, of the words it is written with and what it holds, read one
-- after another. The words are read by the runtime (@derivedExpect@, in
-- @Hazy.Helper@), as @lex@ has them.
module Semantic.Check.Derive.Read (readsPrec) where

import {-# SOURCE #-} qualified Builtin.Read as Read
import {-# SOURCE #-} qualified Builtin.Semigroup as Semigroup
import Control.Monad.ST (ST)
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
import Data.Text (pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromString, toLazyText)
import qualified Data.Vector.Strict as Strict.Vector
import Error (cannotDerive)
import Semantic.Check.ConstructorInstance (ConstructorInstance (..))
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.DataInstance (DataInstance (..))
import Semantic.Check.Derive.Show (printConstructor, printField)
import Semantic.Check.EntryInstance (entry)
import Semantic.Check.Info.Constructor (ConstructorInfo)
import Semantic.Check.Simple.Data (instanciateApart)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Check.Temporary.Pattern as Apart (within)
import qualified Semantic.Index.Constructor as Constructor (Index (..), cons, nil, tuple)
import qualified Semantic.Index.Evidence as Index.Evidence
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
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity (..))
import Prelude hiding (readsPrec)

-- |
-- What is read, one after another: a word, or something a constructor
-- holds, at a precedence, by the evidence for its type's instance.
data Step scope
  = Expect String
  | Field (Evidence scope) Int

shiftStep :: Step scope -> Step (scope' ':+ scope)
shiftStep = \case
  Expect word -> Expect word
  Field evidence precedence -> Field (shift evidence) precedence

-- |
-- A constructor as it is read: in brackets if the precedence is past this,
-- the steps it is read in, and what is made at the end.
data Alternative scope = Alternative
  { past :: Maybe Int,
    steps :: [Step scope],
    constructor :: Constructor.Index scope,
    info :: ConstructorInfo scope
  }

lazy :: Int -> Core.ConstructorInfo.ConstructorInfo scope
lazy count =
  Core.ConstructorInfo.ConstructorInfo
    { entries = Strict.Vector.replicate count EntryInfo {strict = Core.Constructor Type2.Lazy}
    }

string :: String -> Expression scope
string = foldr cons nil
  where
    cons character rest =
      Expression.Constructor
        { constructor = Constructor.cons,
          arguments = Strict.Vector.fromList [Expression.Character {character}, rest],
          constructorInfo = lazy 2
        }

nil :: Expression scope
nil =
  Expression.Constructor
    { constructor = Constructor.nil,
      arguments = Strict.Vector.empty,
      constructorInfo = lazy 0
    }

helper :: String -> Expression scope
helper name = Expression.Helper {helperName = pack name}

-- |
-- What is left to be read, of the steps that are left, given what has been
-- read so far and what is left of the text. At the end the constructor is
-- made of what was read.
chain ::
  [Step scope] ->
  [Expression scope] ->
  Constructor.Index scope ->
  ConstructorInfo scope ->
  Expression scope ->
  Expression scope
chain [] held constructor info rest =
  Expression.Constructor
    { constructor = Constructor.cons,
      arguments =
        Strict.Vector.fromList
          [ Expression.Constructor
              { constructor = Constructor.tuple 2,
                arguments =
                  Strict.Vector.fromList
                    [ Expression.simplifyConstructorExact constructor info (Strict.Vector.fromList (reverse held)),
                      rest
                    ],
                constructorInfo = lazy 2
              },
            nil
          ],
      constructorInfo = lazy 2
    }
chain (step : steps) held constructor info rest =
  helper "derivedBind"
    `Expression.call` first
    `Expression.call` Expression.Lambda
      ( Expression.Lambda
          ( chain
              (map (shiftStep . shiftStep) steps)
              (more ++ map (shift . shift) held)
              (shift (shift constructor))
              (shift (shift info))
              Expression.lambdaVariable
          )
      )
  where
    (first, more) = case step of
      Expect word -> (helper "derivedExpect" `Expression.call` string word `Expression.call` rest, [])
      Field evidence precedence ->
        ( Expression.Method
            { method = Method.readsPrec,
              evidence,
              instanciation = Mono,
              methodInfo = Class.info Read.definition
            }
            `Expression.call` Expression.int precedence
            `Expression.call` rest,
          [shift Expression.lambdaVariable]
        )

-- |
-- The words a name is written with, as a constructor or as a field: a
-- symbol is written in brackets, and a name that is infix in backquotes.
words' :: String -> [String]
words' = \case
  '(' : symbol | not (null symbol), last symbol == ')' -> ["(", init symbol, ")"]
  '`' : name | not (null name), last name == '`' -> ["`", init name, "`"]
  name -> [name]

-- |
-- > readsPrec d r = readParen (d > p1) (\s -> ...) r <> readParen (d > p2) (\s -> ...) r <> ...
readsPrec ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
readsPrec context position typeIndex datax = do
  datay@DataInstance {types, constructors} <- instanciateApart context position (shift datax)
  let unify place typex = Unify.unify context position typex (Core.Variable $ Local.Local place)
  sequence_ $ zipWith unify [0 ..] (toList types)
  let generate index constructorx@ConstructorInstance {name, syntax, entries} = do
        inside <- Apart.within context position datay (shift typeIndex) constructorx
        evidence <- traverse (Unify.constrain inside position Type2.Read . entry) (toList entries)
        pure $ do
          evidence <- traverse (Unify.solveEvidence position) evidence
          info <- ConstructorInfo.solve $ ConstructorInstance.info constructorx
          let printed infixed = unpack $ toStrict $ toLazyText $ printConstructor infixed name
              expect = map Expect
              (past, steps) = case syntax of
                Constructor.Standard
                  | null evidence -> (Nothing, expect (words' (printed False)))
                  | otherwise -> (Just 10, expect (words' (printed False)) ++ [Field held 11 | held <- evidence])
                Constructor.Infix Fixity {precedence} -> case evidence of
                  [left, right] ->
                    ( Just precedence,
                      [Field left (precedence + 1)] ++ expect (words' (printed True)) ++ [Field right (precedence + 1)]
                    )
                  _ -> cannotDerive position $ fromString "Read: an infix constructor that does not hold two things"
                Constructor.Record fields ->
                  ( Just 11,
                    expect (words' (printed False))
                      ++ [Expect "{"]
                      ++ concat
                        [ [Expect "," | place > 0]
                            ++ expect (words' (unpack $ toStrict $ toLazyText $ printField field))
                            ++ [Expect "=", Field held 0]
                        | (place, field, held) <- zip3 [0 :: Int ..] (toList fields) evidence
                        ]
                      ++ [Expect "}"]
                  )
          pure
            Alternative
              { past,
                steps,
                constructor = Constructor.Index {typeIndex = shift typeIndex, constructorIndex = index},
                info
              }
  alternatives <- sequence $ zipWith generate [0 ..] (toList constructors)
  pure $ do
    alternatives <- sequence alternatives
    let precedence = shift Expression.lambdaVariable
        text = Expression.lambdaVariable
        -- One constructor's way, read in brackets when the precedence is
        -- past what the constructor is written at, and in them or not
        -- otherwise.
        alternative Alternative {past, steps, constructor, info} =
          helper "derivedParen"
            `Expression.call` ( case past of
                                  Nothing -> Expression.false
                                  Just level ->
                                    Expression.ifx
                                      (Expression.lessThenEqualInt precedence (Expression.int level))
                                      Expression.false
                                      Expression.true
                              )
            `Expression.call` Expression.Lambda
              ( chain
                  (map (shiftStep . shiftStep . shiftStep) steps)
                  []
                  (shift (shift (shift constructor)))
                  (shift (shift (shift info)))
                  Expression.lambdaVariable
              )
            `Expression.call` text
        append left right =
          Expression.Method
            { method = Method.combine,
              evidence = Variable {variable = Index.Evidence.Direct Type2.Semigroup Type2.List, instanciation = Mono},
              instanciation = Mono,
              methodInfo = Class.info Semigroup.definition
            }
            `Expression.call` left
            `Expression.call` right
        body = case alternatives of
          [] -> nil
          _ -> foldr1 append (map alternative alternatives)
    pure $ Generated $ Solved $ TypeLambda.mono $ Expression.Lambda $ Expression.Lambda body
