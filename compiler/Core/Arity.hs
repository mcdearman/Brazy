-- |
-- How many arguments a definition takes.
--
-- A function of several arguments is a function that gives back a function,
-- and nothing in a type says how a definition of one was written: as
-- functions inside functions, which can be given every argument at once, or
-- as something that works a function out. That is found here, once, when a
-- definition is made, and kept with it. What makes Javascript reads it to
-- make the form of a definition that takes every argument at once, and to
-- send a call there.
--
-- What is counted is what is written: the functions a definition is made
-- of, one inside the next. A newtype is nothing when a program runs, so the
-- functions are counted through one.
module Core.Arity (Arity (..), measure) where

import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.Expression (Expression (..))
import Core.Tree.TypeLambda (TypeLambda, TypeLambdaOver (..))

data Arity = Arity
  { -- | How much evidence the definition requires. It is given all of it
    -- at once, before anything else.
    evidences :: !Int,
    -- | How many functions the definition is, one inside the next.
    lambdas :: !Int
  }
  deriving (Eq, Show)

measure :: TypeLambda scope -> Arity
measure TypeLambdaOver {constraints, result} =
  Arity
    { evidences = case constraints of
        Constraints required -> length required
        None -> 0,
      lambdas = depth result
    }
  where
    depth :: Expression scope' -> Int
    depth = \case
      Lambda {body} -> 1 + depth body
      Newtype {argument} -> depth argument
      _ -> 0
