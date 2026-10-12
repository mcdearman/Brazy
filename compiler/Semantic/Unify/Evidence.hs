module Semantic.Unify.Evidence where

import Control.Monad.ST (ST)
import Core.Tree.Evidence (EvidenceF (..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Unify.Instanciation as Instanciation

type Evidence s scope = EvidenceF (Logical s scope) scope

data Logical s scope where
  Box :: !(STRef s (Box s scope)) -> Logical s scope
  Shift :: !(Logical s scopes) -> Logical s (scope ':+ scopes)

instance Shift0.Functor (Logical s) where
  map category index = case category of
    Shift0.Id -> index
    Shift0.Shift -> Shift index
    after Shift0.:. before -> Shift0.map after (Shift0.map before index)

data Box s scope
  = Solved !(Evidence s scope)
  | -- | Evidence that is not known yet, because the type it is for is not.
    -- If the type is never known, the evidence may still be had by taking
    -- the type to be @Integer@: see 'Fallback'.
    Unsolved {fallback :: !(Maybe (Fallback s))}

-- |
-- What evidence is when the type it is for is never worked out and is taken
-- to be @Integer@, as the Haskell report's defaulting has it. The class is
-- one that @Integer@ is a builtin instance of, and the action says whether
-- the type may be defaulted: that depends on every class the type is
-- constrained by, which is only known at the end.
data Fallback s = Fallback
  { classx :: !Defaultable,
    permitted :: ST s Bool
  }

-- |
-- The classes whose constraints do not stop a type from being defaulted.
data Defaultable
  = Num
  | Real
  | Integral
  | Enum
  | Eq
  | Ord
  | Show
  | Read

defaultable :: Type2.Index scope -> Maybe Defaultable
defaultable = \case
  Type2.Num -> Just Num
  Type2.Real -> Just Real
  Type2.Integral -> Just Integral
  Type2.Enum -> Just Enum
  Type2.Eq -> Just Eq
  Type2.Ord -> Just Ord
  Type2.Show -> Just Show
  Type2.Read -> Just Read
  _ -> Nothing

-- |
-- Whether a class is one of numbers. A type is only defaulted if it is
-- constrained by one.
numeric :: Defaultable -> Bool
numeric = \case
  Num -> True
  Real -> True
  Integral -> True
  _ -> False

classOf :: Defaultable -> Type2.Index scope
classOf = \case
  Num -> Type2.Num
  Real -> Type2.Real
  Integral -> Type2.Integral
  Enum -> Type2.Enum
  Eq -> Type2.Eq
  Ord -> Type2.Ord
  Show -> Type2.Show
  Read -> Type2.Read

-- unification between evidence should never fail
unify :: Evidence s scope -> Evidence s scope -> ST s ()
unify (Logical (Box reference)) (Logical (Box reference'))
  | reference == reference' = pure ()
  | otherwise = do
      box <- readSTRef reference
      box' <- readSTRef reference'
      combine box box'
  where
    combine (Solved evidence) (Solved evidence') = unify evidence evidence'
    combine Unsolved {} _ =
      writeSTRef reference $ Solved $ Logical (Box reference')
    combine _ Unsolved {} =
      writeSTRef reference' $ Solved $ Logical (Box reference)
unify (Logical (Box reference)) evidence' =
  readSTRef reference >>= \case
    Solved evidence -> unify evidence evidence'
    Unsolved {} -> writeSTRef reference (Solved evidence')
unify evidence (Logical (Box reference')) =
  readSTRef reference' >>= \case
    Solved evidence' -> unify evidence evidence'
    Unsolved {} -> writeSTRef reference' (Solved evidence)
unify (Variable index instanciation) (Variable index' instanciation')
  | index == index' = do
      Instanciation.unify instanciation instanciation'
unify (Logical (Shift logical)) evidence' = do
  evidence' <- unshift evidence'
  unify (Logical logical) evidence'
unify evidence (Logical (Shift logical')) = do
  evidence <- unshift evidence
  unify evidence (Logical logical')
unify (Held term) (Held term')
  | term == term' = pure ()
unify _ _ = error "unify evidence can't fail"

unshift :: Evidence s (scope ':+ scopes) -> ST s (Evidence s scopes)
unshift = \case
  Variable variable instanciation -> do
    let fail = error "unshift can't fail"
    instanciation <- Instanciation.unshift instanciation
    pure $ Variable (Shift.map (Shift.Unshift fail) variable) instanciation
  Logical (Box reference) -> do
    readSTRef reference >>= \case
      Solved evidence -> unshift evidence
      Unsolved {fallback} -> do
        box <- newSTRef Unsolved {fallback}
        writeSTRef reference (Solved $ Logical $ Shift $ Box box)
        pure $ Logical (Box box)
  Logical (Shift logical) -> pure (Logical logical)
  Super evidence index -> do
    evidence <- unshift evidence
    pure $ Super evidence index
  Held term -> do
    let fail = error "unshift can't fail"
    pure $ Held (Shift.map (Shift.Unshift fail) term)
