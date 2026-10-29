module Semantic.Unify.Evidence where

import Control.Monad.ST (ST)
import Core.Tree.Evidence (EvidenceF (..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Semantic.Default (Default)
import qualified Semantic.Index.Type2 as Type2
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Unify.Instanciation as Instanciation
import {-# SOURCE #-} qualified Semantic.Unify.Type as Type
import Syntax.Position (Position)

type Evidence s scope = EvidenceF (Logical s scope) (Type.Logical s scope) scope

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
    -- the type to be @Integer@ or @Double@: see 'Fallback'.
    Unsolved {fallback :: !(Maybe (Fallback s))}

-- |
-- What evidence is when the type it is for is never worked out and is taken
-- to be @Integer@ or @Double@, as the Haskell report's defaulting has it.
-- The class is one that both are builtin instances of where they can be,
-- and the action says which the type is taken to be, if either: that
-- depends on every class the type is constrained by, which is only known at
-- the end.
data Fallback s = Fallback
  { classx :: !Defaultable,
    permitted :: ST s (Maybe Default)
  }

-- |
-- The classes whose constraints do not stop a type from being defaulted.
data Defaultable
  = Num
  | Real
  | Integral
  | Fractional
  | Floating
  | RealFrac
  | RealFloat
  | Enum
  | Bounded
  | Eq
  | Ord
  | Show
  | Read

defaultable :: Type2.Index scope -> Maybe Defaultable
defaultable = \case
  Type2.Num -> Just Num
  Type2.Real -> Just Real
  Type2.Integral -> Just Integral
  Type2.Fractional -> Just Fractional
  Type2.Floating -> Just Floating
  Type2.RealFrac -> Just RealFrac
  Type2.RealFloat -> Just RealFloat
  Type2.Enum -> Just Enum
  Type2.Bounded -> Just Bounded
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
  Fractional -> True
  Floating -> True
  RealFrac -> True
  RealFloat -> True
  _ -> False

-- |
-- Whether a class is one that @Integer@ is not an instance of, and
-- @Double@ is.
fractional :: Defaultable -> Bool
fractional = \case
  Fractional -> True
  Floating -> True
  RealFrac -> True
  RealFloat -> True
  _ -> False

classOf :: Defaultable -> Type2.Index scope
classOf = \case
  Num -> Type2.Num
  Real -> Type2.Real
  Integral -> Type2.Integral
  Fractional -> Type2.Fractional
  Floating -> Type2.Floating
  RealFrac -> Type2.RealFrac
  RealFloat -> Type2.RealFloat
  Enum -> Type2.Enum
  Bounded -> Type2.Bounded
  Eq -> Type2.Eq
  Ord -> Type2.Ord
  Show -> Type2.Show
  Read -> Type2.Read

-- unification between evidence should never directly fail
-- it may fail due to type unification
unify :: Context s scope -> Position -> Evidence s scope -> Evidence s scope -> ST s ()
unify context position (Logical (Box reference)) (Logical (Box reference'))
  | reference == reference' = pure ()
  | otherwise = do
      box <- readSTRef reference
      box' <- readSTRef reference'
      combine box box'
  where
    combine (Solved evidence) (Solved evidence') = unify context position evidence evidence'
    combine Unsolved {} _ =
      writeSTRef reference $ Solved $ Logical (Box reference')
    combine _ Unsolved {} =
      writeSTRef reference' $ Solved $ Logical (Box reference)
unify context position (Logical (Box reference)) evidence' =
  readSTRef reference >>= \case
    Solved evidence -> unify context position evidence evidence'
    Unsolved {} -> writeSTRef reference (Solved evidence')
unify context position evidence (Logical (Box reference')) =
  readSTRef reference' >>= \case
    Solved evidence' -> unify context position evidence evidence'
    Unsolved {} -> writeSTRef reference' (Solved evidence)
unify context position (Variable index instanciation) (Variable index' instanciation')
  | index == index' = do
      Instanciation.unify context position instanciation instanciation'
unify context position (Logical (Shift logical)) evidence' = do
  evidence' <- unshift context position evidence'
  unify (Shift.unshift context) position (Logical logical) evidence'
unify context position evidence (Logical (Shift logical')) = do
  evidence <- unshift context position evidence
  unify (Shift.unshift context) position evidence (Logical logical')
unify _ _ (Held term) (Held term')
  | term == term' = pure ()
unify context position (Applied function arguments) (Applied function' arguments') = do
  unify context position function function'
  sequence_ (zipWith (unify context position) arguments arguments')
unify _ _ _ _ = error "unify evidence can't fail"

unshift ::
  Context s (scope ':+ scopes) ->
  Position ->
  Evidence s (scope ':+ scopes) ->
  ST s (Evidence s scopes)
unshift context position = \case
  Variable variable instanciation -> do
    let fail = error "unshift can't fail"
    instanciation <- Instanciation.unshift context position instanciation
    pure $ Variable (Shift.map (Shift.Unshift fail) variable) instanciation
  Logical (Box reference) -> do
    readSTRef reference >>= \case
      Solved evidence -> unshift context position evidence
      Unsolved {fallback} -> do
        box <- newSTRef Unsolved {fallback}
        writeSTRef reference (Solved $ Logical $ Shift $ Box box)
        pure $ Logical (Box box)
  Logical (Shift logical) -> pure (Logical logical)
  Super evidence index -> do
    evidence <- unshift context position evidence
    pure $ Super evidence index
  Held term -> do
    let fail = error "unshift can't fail"
    pure $ Held (Shift.map (Shift.Unshift fail) term)
  Applied function arguments -> do
    function <- unshift context position function
    arguments <- traverse (unshift context position) arguments
    pure $ Applied function arguments
  -- It is only made of evidence that is worked out already.
  Abstract {} -> error "unshift of evidence that is a function"
