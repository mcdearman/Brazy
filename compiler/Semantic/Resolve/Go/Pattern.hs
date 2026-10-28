{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Pattern where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict.Vector
import Error (duplicateVariableEntries)
import qualified Semantic.Index.Constructor as Constructor (cons, nil, tuple)
import qualified Semantic.Index.Term as Term (Bound (..), Index (..))
import qualified Semantic.Index.Term2 as Term2
import qualified Semantic.Resolve.Binding.Constructor as Constructor (BindingF (..), Detail (..))
import qualified Semantic.Resolve.Binding.Term as Term (BindingF (..), Detail (..), Selector (..))
import Semantic.Resolve.Bindings (constructors, stability, terms, types, (</>))
import qualified Semantic.Resolve.Bindings as Bindings
import Semantic.Resolve.Context
  ( Context (..),
    (!=),
    (!=~),
  )
import qualified Semantic.Resolve.Go.PatternField as Field (resolve)
import {-# SOURCE #-} qualified Semantic.Resolve.Temporary.PatternInfix as Infix (fix, resolve)
import qualified Semantic.Resolve.Go.Scheme as Scheme (resolve)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope (Pattern)
import Semantic.Shift (shift)
import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Pattern (Bindings (..), Failable (..), Field (..), Pattern (..), lazy, variable)
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Pattern as Syntax (Pattern (..))
import qualified Syntax.Tree.PatternInfix as Syntax.Infix
import Syntax.Variable (Variable)
import Prelude hiding (Bool (False, True), Either (Left, Right), head, tail)
import qualified Prelude

augment :: Pattern Position Resolve scope' -> Context scope -> Context (Scope.Pattern ':+ scope)
augment patternx context
  | context@Context {locals} <- shift context =
      context
        { locals = bindings patternx </> locals
        }

bindings ::
  (Monoid stability) =>
  Pattern Position Resolve scope' ->
  Bindings.Bindings stability (Scope.Pattern ':+ scope)
bindings patternx =
  Bindings.Bindings
    { terms = Map.map termIndex (selections patternx),
      constructors = Map.empty,
      types = Map.empty,
      stability = mempty
    }
  where
    termIndex (position :@ x) =
      position
        Term.:@ Identity
          Term.Binding
            { index = Term2.index $ Term.Pattern x,
              fixity = Fixity {associativity = Left, precedence = 9},
              selector = Term.Normal
            }

selections :: Pattern Position Resolve scope -> Map Variable (Marked Term.Bound Position)
selections patternx = Map.map single (selections patternx)
  where
    single = \case
      [] -> error "missing bound"
      [(position, patternx)] -> position :@ patternx
      patterns -> duplicateVariableEntries (map fst patterns)

    selections :: Pattern Position Resolve scope -> Map Variable [(Position, Term.Bound)]
    selections patternx = Map.unionWith (++) outer inner
      where
        outer = Map.map (\position -> [(position, Term.At)]) (names patternx)
        select :: [Pattern Position Resolve scope] -> Map Variable [(Position, Term.Bound)]
        select patterns = foldr (Map.unionWith (++)) Map.empty $ zipWith pick patterns [0 ..]
          where
            pick :: Pattern Position Resolve scope -> Int -> Map Variable [(Position, Term.Bound)]
            pick patternx i = (Map.map . map) (\(position, x) -> (position, Term.Select i x)) (selections patternx)
        inner = case patternx of
          Wildcard {} -> select []
          Match {match} -> case match of
            Constructor {patterns} -> select (toList patterns)
            Record {fields} -> select [field | Field {field} <- toList fields]
            Integer {} -> select []
            Float {} -> select []
            Character {} -> select []
            String {} -> select []
            List {items} -> select (toList items)
          -- What says its type is matched against the same.
          Signature {patternx} -> selections patternx

resolve :: Context scope -> Syntax.Pattern Position -> Pattern Position Resolve scope
resolve context = \case
  Syntax.Variable {variable = position :@ name} -> variable position name
  Syntax.At {variable = position :@ variable, patternx} ->
    let pre = resolve context patternx
     in pre {names = Map.insert variable position (names pre)}
  Syntax.Wildcard {startPosition} ->
    Wildcard
      { startPosition,
        levity = Inferred,
        names = Map.empty,
        forced = Prelude.False
      }
  -- A bang pattern. Matching anything but a name, or nothing, evaluates
  -- what is matched already, so a bang only changes those. A bang on a
  -- lazy pattern is only the lazy pattern, which does not evaluate what it
  -- is matched against first, as GHC's does.
  Syntax.Strict {patternx} -> case resolve context patternx of
    Wildcard {startPosition, levity, names} -> Wildcard {startPosition, levity, names, forced = Prelude.True}
    patternx -> patternx
  Syntax.Irrefutable {patternx} -> lazy (resolve context patternx)
  Syntax.Infix {startPosition, left, operator, operatorPosition, right} ->
    Infix.fix $
      Infix.resolve
        context
        Syntax.Infix.Infix
          { startPosition,
            left,
            operator,
            operatorPosition,
            right
          }
  Syntax.InfixCons {startPosition, left, operatorPosition, right} ->
    Infix.fix $
      Infix.resolve
        context
        Syntax.Infix.InfixCons
          { startPosition,
            left,
            operatorPosition,
            right
          }
  -- The type a pattern says is resolved as a signature's is. A type
  -- variable that is not in scope is one of its own, so a pattern may say
  -- only part of its type.
  Syntax.Signature {startPosition, patternx, signature} ->
    Signature
      { names = Map.empty,
        patternx = resolve context patternx,
        startPosition,
        signature = Scheme.resolve context signature
      }
  patternx ->
    Match
      { startPosition = Syntax.startPosition patternx,
        levity = Inferred,
        names = Map.empty,
        irrefutable = Prelude.False,
        match = case patternx of
          Syntax.Constructor {startPosition, constructor, patterns} ->
            case context !=~ startPosition :@ constructor of
              _ Constructor.:@ Identity Constructor.Binding {index = constructor, failable} ->
                Constructor
                  { constructorPosition = startPosition,
                    constructor,
                    patterns = fmap (resolve context) patterns,
                    failable,
                    constructorInfo = Inferred
                  }
          Syntax.Record {startPosition, constructor, fields} ->
            case context != startPosition :@ constructor of
              _
                Constructor.:@ Identity
                                 ( binding@Constructor.Binding
                                     { index = constructor,
                                       failable
                                     }
                                   ) ->
                  Record
                    { constructorPosition = startPosition,
                      constructor,
                      fields = fmap (Field.resolve context binding) fields,
                      failable,
                      constructorInfo = Inferred
                    }
          Syntax.Integer {integer} ->
            Integer
              { integer,
                evidence = Inferred,
                equal = Inferred
              }
          Syntax.Float {float} ->
            Float
              { float,
                evidence = Inferred,
                equal = Inferred
              }
          Syntax.NegativeInteger {integer} ->
            Integer
              { integer = -integer,
                evidence = Inferred,
                equal = Inferred
              }
          Syntax.NegativeFloat {float} ->
            Float
              { float = -float,
                evidence = Inferred,
                equal = Inferred
              }
          Syntax.Character {character} ->
            Character
              { character
              }
          Syntax.String {string} ->
            String
              { string
              }
          Syntax.Unit {startPosition} ->
            Constructor
              { constructorPosition = startPosition,
                constructor = Constructor.tuple 0,
                patterns = Strict.Vector.empty,
                failable = Exhaustive,
                constructorInfo = Inferred
              }
          Syntax.Tuple {startPosition, elements} ->
            Constructor
              { constructorPosition = startPosition,
                constructor = Constructor.tuple (length elements),
                patterns = resolve context <$> elements,
                failable = Exhaustive,
                constructorInfo = Inferred
              }
          Syntax.List {startPosition, items}
            | null items ->
                Constructor
                  { constructorPosition = startPosition,
                    constructor = Constructor.nil,
                    patterns = Strict.Vector.empty,
                    failable = Failable,
                    constructorInfo = Inferred
                  }
            | otherwise ->
                List
                  { items = fmap (resolve context) items
                  }
          Syntax.Cons {startPosition, head, tail} ->
            Constructor
              { constructorPosition = startPosition,
                constructor = Constructor.cons,
                patterns = Strict.Vector.fromList [resolve context head, resolve context tail],
                failable = Failable,
                constructorInfo = Inferred
              }
      }
