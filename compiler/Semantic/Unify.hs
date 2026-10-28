-- |
-- Unification public api
module Semantic.Unify
  ( Logical,
    LogicalEvidence,
    Type,
    Forall,
    ForallOver,
    Constraints,
    Constraint,
    Evidence,
    Instanciation,
    fresh,
    skolem,
    sealed,
    mark,
    unify,
    refine,
    absorb,
    lowerEquality,
    alike,
    constrain,
    Zonk (..),
    Zonker,
    Generalizable (..),
    Generalize (..),
    Body ((:::)),
    instanciate,
    generalizeBody,
    Solve,
    later,
    runSolve,
    SolveType (..),
    solve,
    SolveEvidence (..),
    solveEvidence,
    MapForall (..),
    mapForall,
  )
where

import Core.Tree.Forall (ForallOver)
import Semantic.Unify.Constraint (Constraint)
import Semantic.Unify.Constraints (Constraints)
import Semantic.Unify.Evidence (Evidence)
import qualified Semantic.Unify.Evidence as Evidence
import {-# SOURCE #-} Semantic.Unify.Forall
  ( Body ((:::)),
    Forall,
    Generalize (..),
    MapForall (..),
    generalizeBody,
    instanciate,
    mapForall,
  )
import {-# SOURCE #-} Semantic.Unify.Generalizable
  ( Generalizable (..),
  )
import Semantic.Unify.Instanciation
  ( Instanciation,
  )
import {-# SOURCE #-} Semantic.Unify.Solve
  ( Solve,
    SolveEvidence (..),
    SolveType (..),
    later,
    runSolve,
    solve,
    solveEvidence,
  )
import {-# SOURCE #-} Semantic.Unify.Type
  ( Logical,
    Type,
    constrain,
    fresh,
    skolem,
    sealed,
    mark,
    unify,
    refine,
    absorb,
    lowerEquality,
    alike,
  )
import {-# SOURCE #-} Semantic.Unify.Zonk
  ( Zonk (..),
    Zonker,
  )

type LogicalEvidence = Evidence.Logical
