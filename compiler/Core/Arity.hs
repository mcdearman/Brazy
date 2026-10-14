-- |
-- How many arguments a definition takes.
--
-- A function of several arguments is a function that gives back a function,
-- and nothing in a type says how a definition of one was written: as
-- functions inside functions, which can be given every argument at once, or
-- as something that works a function out. That is found here, once, when a
-- definition is made, and kept with it.
--
-- What is counted is what is written: the functions a definition is made
-- of, one inside the next. They are counted through what is nothing when a
-- program runs, or next to nothing:
--
-- * a newtype;
-- * the one way out of a match that matches nothing, which is how what is
--   written after an @=@ comes from the check;
-- * the definitions of a @where@ that has none;
-- * and a name for a name, which is how an argument that was written as a
--   variable is called by that variable.
--
-- Whatever goes by what is counted here has to get to the functions the
-- same way: see "Core.Eta".
module Core.Arity (Arity (..), measure, alias) where

import Core.Tree.Constraints (ConstraintsF (..))
import {-# SOURCE #-} qualified Core.Tree.Declarations as Declarations (none)
import Core.Tree.Expression (Expression (..))
import Core.Tree.Instanciation (InstanciationF (Mono))
import qualified Core.Tree.Statements as Flow (Statements (..))
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
      Join {statements} -> flow statements
      _ -> 0
    flow :: Flow.Statements scope' -> Int
    flow = \case
      Flow.Done {done} -> depth done
      Flow.Let {declarations, letBody} | Declarations.none declarations -> flow letBody
      Flow.LetOne {declaration, body} | alias declaration -> flow body
      _ -> 0

-- |
-- Whether an expression is only a name, so that a name for it is a name
-- for a name.
alias :: Expression scope -> Bool
alias = \case
  Variable {instanciation = Mono} -> True
  _ -> False
