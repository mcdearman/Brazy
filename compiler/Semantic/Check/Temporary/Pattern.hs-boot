{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Pattern where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Semantic.Check.Context (Context)
import Semantic.Check.LocalBinding (Equality)
import Semantic.Check.TermBinding (TermBinding)
import qualified Semantic.Index.Table.Term as Term
import Semantic.Index.Term (Bound)
import Semantic.Scope (Environment ((:+)))
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Pattern as Semantic
import qualified Semantic.Tree.Pattern as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

type role Pattern nominal nominal

type Pattern :: Type -> Environment -> Type
data Pattern s scope

(!) :: Pattern s scope -> Bound -> Unify.Type s scope
learnt :: Pattern s scope -> [Equality s scope]
owned :: Pattern s scope -> [(Position, Unify.Type s scope)]
augmentPattern :: Pattern s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
check :: Context s scope -> Unify.Type s scope -> Semantic.Pattern Resolve scope -> ST s (Pattern s scope)
solve :: Pattern s scope -> Unify.Solve s (Solved.Pattern Check scope)
