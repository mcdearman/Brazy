-- |
-- Write a definition that is another given some of its arguments as a
-- function of the rest.
--
-- How many arguments a definition takes is counted from how it is written:
-- see "Core.Arity". So
--
-- > total = foldl add 0
--
-- is counted as taking none, though @foldl@ takes three and has two. Every
-- call of @total@ makes the function that is @foldl@ with two arguments,
-- and then gives it the third. Here @total@ is written
--
-- > total = \list -> foldl add 0 list
--
-- which is counted as taking one, so that a call of it goes on to a call
-- of @foldl@ with all three.
--
-- This is only done where nothing is worked out a second time for it.
-- The definition that is given the arguments is known to be so many
-- functions one inside the next, so giving it fewer does nothing but keep
-- them. And each argument it is given is a name, a number, a character or
-- a constructor of nothing: anything more would be worked out once as the
-- definition was written, and once for every call as it is written here.
-- The same goes for the evidence the other definition is given, which has
-- to be evidence that is there already and not evidence that is made.
--
-- A number that is written counts as there already, though it is made of
-- the whole number that was written: so does a method of evidence that is
-- there already.
--
-- Only definitions at the top of a module are looked at, and only ones
-- that are another definition at the top of a module given arguments.
module Core.Eta (expand, standing) where

import Core.Arity (Arity (..), alias)
import qualified Core.Arity as Arity (measure)
import Core.Tree.Declaration (Declaration (..))
import Core.Tree.Declarations (Declarations (..))
import qualified Core.Tree.Declarations as Declarations (none)
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Core.Tree.Expression (Expression (..), lambdaVariable)
import Core.Tree.Instanciation (Instanciation, InstanciationF (Instanciation, Mono))
import qualified Core.Tree.Statements as Flow (Statements (..))
import qualified Core.Tree.Module as Core (Module (..))
import Core.Tree.TypeLambda (TypeLambdaOver (..))
import Data.Text (pack)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Semantic.Index.Method as Method (fromInteger)
import qualified Semantic.Index.Term as Term (Index (..))
import Semantic.Scope (Global)
import Semantic.Shift (shift)
import Syntax.Lexer (FullQualifiers (..), Qualifiers (..), constructorIdentifier)

-- |
-- How many arguments each definition of each module takes.
type Known = Vector (Vector Int)

expand :: Vector Core.Module -> Vector Core.Module
expand modules = Vector.map rewrite modules
  where
    -- The primitives are declared and not defined, so there is nothing of
    -- them to look at.
    primitives = Local :.. constructorIdentifier (pack "Hazy")
    written :: Known
    written = flip Vector.map modules $ \Core.Module {name, declarations = Declarations {terms}} ->
      if name == primitives
        then Vector.map (const 0) terms
        else Vector.map (\Declaration {arity = Arity {lambdas}} -> lambdas) terms
    -- What is known once more of it has been found: a definition takes
    -- what it is written to take and what is left over of the definition
    -- it is, going by what that was known to take before.
    further :: Known -> Known
    further known = Vector.zipWith row modules written
      where
        row Core.Module {name, declarations = Declarations {terms}} before
          | name == primitives = before
          | otherwise = Vector.zipWith (\declaration own -> own + left known declaration) terms before
    -- A definition may be one that is one that is another. This is as far
    -- as that is followed, and it is a number so that definitions that are
    -- each other come to an end.
    known = further (further (further written))
    rewrite modulex@Core.Module {name, declarations}
      | name == primitives = modulex
      | Declarations {terms, types, typeExtras, classInstances, dataInstances} <- declarations =
          Core.Module
            { name,
              declarations =
                Declarations
                  { terms = Vector.map (declaration known) terms,
                    types,
                    typeExtras,
                    classInstances,
                    dataInstances
                  }
            }

-- |
-- How many more arguments a definition takes than it is written to.
left :: Known -> Declaration Global -> Int
left known Declaration {definition = TypeLambdaOver {result}} = under result
  where
    under :: Expression scope -> Int
    under = \case
      Lambda {body} -> under body
      Newtype {argument} -> under argument
      Join {statements} -> flow statements
      expression -> wanting known expression
    flow :: Flow.Statements scope -> Int
    flow = \case
      Flow.Done {done} -> under done
      Flow.Let {declarations, letBody} | Declarations.none declarations -> flow letBody
      Flow.LetOne {declaration, body} | alias declaration -> flow body
      _ -> 0

-- |
-- The definition at the top of a module that a definition is, given
-- arguments or given none, written out as taking them or not: its module
-- and its place there.
standing :: Declaration Global -> Maybe (Int, Int)
standing Declaration {definition = TypeLambdaOver {result}} = under result
  where
    under :: Expression scope -> Maybe (Int, Int)
    under = \case
      Lambda {body} -> under body
      Newtype {argument} -> under argument
      Join {statements} -> flow statements
      expression -> heading expression
    flow :: Flow.Statements scope -> Maybe (Int, Int)
    flow = \case
      Flow.Done {done} -> under done
      Flow.Let {declarations, letBody} | Declarations.none declarations -> flow letBody
      Flow.LetOne {declaration, body} | alias declaration -> flow body
      _ -> Nothing
    heading :: Expression scope -> Maybe (Int, Int)
    heading = \case
      Call {function} -> heading function
      Variable {variable} -> top variable
      _ -> Nothing

-- |
-- A definition written as taking everything it takes. The functions it is
-- made of are got to as they are where they are counted: see "Core.Arity".
declaration :: Known -> Declaration Global -> Declaration Global
declaration known original@Declaration {name, definition = TypeLambdaOver {parameters, constraints, result}, typex}
  | left known original == 0 = original
  | otherwise =
      let definition = TypeLambdaOver {parameters, constraints, result = under result}
       in Declaration {name, definition, typex, arity = Arity.measure definition}
  where
    under :: Expression scope -> Expression scope
    under = \case
      Lambda {body} -> Lambda {body = under body}
      Newtype {constructor, argument, direction} -> Newtype {constructor, argument = under argument, direction}
      Join {statements} -> Join {statements = flow statements}
      expression -> wrap (wanting known expression) expression
    flow :: Flow.Statements scope -> Flow.Statements scope
    flow = \case
      Flow.Done {done} -> Flow.Done {done = under done}
      Flow.Let {declarations, letBody}
        | Declarations.none declarations -> Flow.Let {declarations, letBody = flow letBody}
      Flow.LetOne {declaration = named, body}
        | alias named -> Flow.LetOne {declaration = named, body = flow body}
      other -> other
    wrap :: Int -> Expression scope -> Expression scope
    wrap count expression
      | count <= 0 = expression
      | otherwise =
          Lambda {body = wrap (count - 1) Call {function = shift expression, argument = lambdaVariable}}

-- |
-- How many arguments an expression is still to be given, if it is a
-- definition that is known given fewer than it takes, and nothing is
-- worked out to give it them.
wanting :: Known -> Expression scope -> Int
wanting known expression = case spine expression [] of
  (Variable {variable, instanciation}, given)
    | Just (global, local) <- top variable,
      ready instanciation,
      all atom given ->
        max 0 (known Vector.! global Vector.! local - length given)
  _ -> 0
  where
    spine :: Expression scope -> [Expression scope] -> (Expression scope, [Expression scope])
    spine function given = case function of
      Call {function = inner, argument} -> spine inner (argument : given)
      _ -> (function, given)

-- |
-- The module and the place in it of a definition at the top of a module.
top :: Term.Index scope -> Maybe (Int, Int)
top = \case
  Term.Shift index -> top index
  Term.Global global local -> Just (global, local)
  _ -> Nothing

-- |
-- Whether an argument is there already, with nothing to speak of to work
-- out.
atom :: Expression scope -> Bool
atom = \case
  Variable {instanciation = Mono} -> True
  Integer {} -> True
  Character {} -> True
  Constructor {arguments} -> null arguments
  -- A method of evidence that is there.
  Method {evidence, instanciation = Mono} -> held evidence
  -- A number that is written.
  Call {function = Method {method, evidence, instanciation = Mono}, argument = Integer {}} ->
    method == Method.fromInteger && held evidence
  _ -> False

-- |
-- Whether the evidence a definition is given is there already.
ready :: Instanciation scope -> Bool
ready = \case
  Mono -> True
  Instanciation evidence -> all held evidence

-- |
-- Whether evidence is there already, and not made of other evidence.
held :: Evidence.EvidenceF logicalEvidence logicalType scope -> Bool
held = \case
  Evidence.Variable {instanciation = Mono} -> True
  Evidence.Super {base} -> held base
  _ -> False
