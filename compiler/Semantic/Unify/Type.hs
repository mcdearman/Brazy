module Semantic.Unify.Type where

import Control.Monad (foldM, when, zipWithM)
import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Core.Tree.Type (TypeF (..))
import qualified Core.Tree.Type as Simple (Type)
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import Core.Type.Functor (shiftLogical)
import Data.Void (Void)
import Error (unsupportedFeatureConstraintedTypeDefaulting)
import qualified Semantic.Check.Binding.Local as Local (LocalBinding (..))
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Context as Context
import qualified Semantic.Check.Binding.Local as Local (Equality (..))
import qualified Semantic.Index.Local as Local.Index (Index)
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Table.Local as Local.Table
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import {-# SOURCE #-} Semantic.Unify.Error (Error (..), abort)
import Semantic.Unify.Evidence (Evidence)
import qualified Semantic.Unify.Evidence as Evidence
  ( Box (..),
    Defaultable (..),
    Fallback (..),
    Logical (..),
    defaultable,
    fractional,
    numeric,
    unify,
    unshift,
  )
import qualified Semantic.Default as Default
import Semantic.Default (Default)
import Data.List (find)
import {-# SOURCE #-} Semantic.Unify.Type1 (constrainWith)
import Syntax.Position (Position)
import Prelude hiding (Functor, head, map)

type Type s scope = TypeF (Logical s scope) scope

data Logical s scopes where
  Box :: !(STRef s (Box s scopes)) -> Logical s scopes
  -- |
  -- Bring a type from a higher scope into current scope
  Shift :: !(Logical s scopes) -> Logical s (scope ':+ scopes)

data Box s scope
  = Unsolved
      { kind :: !(Type s scope),
        -- | What is asked of the variable, by class. A class may be
        -- asked of it more than once, given different arguments.
        constraints :: !(Map (Type2.Index scope) [Delay s scope]),
        erasure :: !Mask.Erasure
      }
  | Solved !(Type s scope)
  | -- | A type that stands for itself alone: a type variable of a
    -- constructor that is not the type of anything outside it, where the
    -- constructor is matched on. Nothing is known of it but that it is some
    -- type, so it is the same as itself and as nothing else, and it is
    -- never worked out to be anything.
    Skolem {kind :: !(Type s scope)}

data Delay s scope = Delay
  { arguments :: [Type s scope],
    evidence :: Evidence s scope
  }

instance Shift0.Functor (Logical s) where
  map category index = case category of
    Shift0.Id -> index
    Shift0.Shift -> Shift index
    after Shift0.:. before -> Shift0.map after (Shift0.map before index)

fresh :: Type s scope -> ST s (Type s scope)
fresh kind = do
  box <- newSTRef $! Unsolved {kind, constraints = Map.empty, erasure = Mask.Erased}
  pure $ Logical $ Box box

-- |
-- A new type that stands for itself alone: see 'Skolem'.
skolem :: Type s scope -> ST s (Type s scope)
skolem kind = do
  box <- newSTRef $! Skolem {kind}
  pure $ Logical $ Box box

-- | Unify two types
--
-- The first argument is the expected type.
-- The second argument is the actual type.
--
-- Both arguments must be well kinded, though they may have different kinds.
-- Alternatively, they may be untypeable, in which case they never unify with
-- unification variables and instead only do syntatic equality.
unify :: forall s scope. Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
unify context_ position term1_ term2_ = unifyWith context_ term1_ term2_
  where
    unifyWith ::
      forall scope.
      Context s scope ->
      Type s scope ->
      Type s scope ->
      ST s ()
    unifyWith context = unify
      where
        unify ::
          Type s scope ->
          Type s scope ->
          ST s ()
        unify term term' = do
          left <- hidden term
          right <- hidden term'
          case (left, right) of
            (Nothing, Nothing) -> plain term term'
            (Just logical, Just logical') | same logical logical' -> pure ()
            -- One that is known here to be some type is that type.
            (Just logical, _) | Just known <- equatedHidden context logical -> unify known term'
            (_, Just logical') | Just known <- equatedHidden context logical' -> unify term known
            (Just _, Just _) -> mismatch
            (Just _, Nothing) -> against term term'
            (Nothing, Just _) -> against term' term
        -- A type that stands for itself alone against one that does not. An
        -- unknown is made to be it, and nothing else is the same as it.
        against :: Type s scope -> Type s scope -> ST s ()
        against rigid = \case
          Logical logical ->
            solution logical >>= \case
              Just known -> unify rigid known
              Nothing -> assign logical rigid
          Variable index | Just known <- equated context index -> unify rigid known
          _ -> mismatch
        assign :: Logical s scope -> Type s scope -> ST s ()
        assign (Box reference) term =
          readSTRef reference >>= \case
            Unsolved {kind, constraints, erasure} -> do
              occurs context position erasure reference term
              typeCheck context position kind term
              reconstrain context position constraints term
              writeSTRef reference $ Solved term
            Solved {} -> error "assigned twice"
            Skolem {} -> error "assigned to a type that stands for itself"
        assign (Shift logical) term = do
          term <- unshift context position term
          unifyWith (Shift.unshift context) (Logical logical) term
        plain ::
          Type s scope ->
          Type s scope ->
          ST s ()
        plain (Logical (Box reference)) (Logical (Box reference'))
          | reference == reference' = pure ()
          | otherwise = do
              box <- readSTRef reference
              box' <- readSTRef reference'
              combine box box'
          where
            combine (Solved term) (Solved term') = unify term term'
            combine (Solved (Logical logical)) Unsolved {} = do
              unify (Logical logical) (Logical (Box reference'))
            combine Unsolved {} (Solved (Logical logical')) = do
              unify (Logical (Box reference)) (Logical logical')
            combine (Solved term) Unsolved {kind, constraints, erasure} = do
              occurs context position erasure reference' term
              typeCheck context position kind term
              reconstrain context position constraints term
              writeSTRef reference' $! Solved term
            combine Unsolved {kind, constraints, erasure} (Solved term') = do
              occurs context position erasure reference term'
              typeCheck context position kind term'
              reconstrain context position constraints term'
              writeSTRef reference $! Solved term'
            combine
              Unsolved {kind = kind1, constraints = constraints1, erasure = erasure1}
              Unsolved {kind = kind2, constraints = constraints2, erasure = erasure2}
                | constraints1 <- fmap pure constraints1,
                  constraints2 <- fmap pure constraints2,
                  -- What is asked of both, given the same arguments, is
                  -- asked once. The rest is asked as well.
                  let merge left right = do
                        left <- left
                        right <- right
                        let add known delay@Delay {arguments = arguments2, evidence = evidence2} = do
                              found <- firstM (\Delay {arguments = arguments1} -> sameAll arguments1 arguments2) known
                              case found of
                                Just Delay {evidence = evidence1} -> do
                                  Evidence.unify context position evidence1 evidence2
                                  pure known
                                Nothing -> pure (known ++ [delay])
                        foldM add left right =
                    do
                      Semantic.Unify.Type.unify context position kind1 kind2
                      constraints <- sequence $ Map.unionWith merge constraints1 constraints2
                      writeSTRef reference' $! Unsolved {kind = kind1, constraints, erasure = erasure1 <> erasure2}
                      writeSTRef reference $! Solved (Logical (Box reference'))
            combine Skolem {} _ = error "plain unification of a type that stands for itself"
            combine _ Skolem {} = error "plain unification of a type that stands for itself"
        plain (Logical (Box reference)) term' =
          readSTRef reference >>= \case
            Unsolved {kind, constraints, erasure} -> do
              occurs context position erasure reference term'
              typeCheck context position kind term'
              reconstrain context position constraints term'
              writeSTRef reference $ Solved term'
            Solved term -> unify term term'
            Skolem {} -> error "plain unification of a type that stands for itself"
        plain term (Logical (Box reference')) =
          readSTRef reference' >>= \case
            Unsolved {kind, constraints, erasure} -> do
              occurs context position erasure reference' term
              typeCheck context position kind term
              reconstrain context position constraints term
              writeSTRef reference' $ Solved term
            Solved term' -> unify term term'
            Skolem {} -> error "plain unification of a type that stands for itself"
        -- Types involving a higher scope must be solved in said scope.
        -- One that has been solved already is the type it was found to be,
        -- and is taken as that here, where more may be known of the type
        -- variables in it than is known further out.
        plain (Logical (Shift logical)) term' =
          solution logical >>= \case
            Just term -> unify (shiftLogical term) term'
            Nothing -> do
              term' <- unshift context position term'
              unifyWith (Shift.unshift context) (Logical logical) term'
        plain term (Logical (Shift logical)) =
          solution logical >>= \case
            Just term' -> unify term (shiftLogical term')
            Nothing -> do
              term <- unshift context position term
              unifyWith (Shift.unshift context) term (Logical logical)
        plain (Variable index) (Variable index')
          | index == index' = pure ()
        -- A variable that is known here to be some type is that type.
        plain (Variable index) term'
          | Just known <- equated context index = unify known term'
        plain term (Variable index')
          | Just known <- equated context index' = unify term known
        plain (Constructor index) (Constructor index')
          | index == index' = pure ()
          | otherwise = mismatch
        plain (Call term1 term2) (Call term1' term2') = do
          unify term1 term1'
          unify term2 term2'
        plain (Function type1 type2) (Function type1' type2') = do
          unify type1 type1'
          unify type2 type2'
        plain (Function type1 type2) (Call function' type2') = do
          typeCheck context position (Type Small) type1
          typeCheck context position (Type Small) type2
          unify (Call (Constructor Type2.Arrow) type1) function'
          unify type2 type2'
        plain (Call function type2) (Function type1' type2') = do
          typeCheck context position (Type Small) type1'
          typeCheck context position (Type Small) type2'
          unify function (Call (Constructor Type2.Arrow) type1')
          unify type2 type2'
        plain (Type universe) (Type universe') = do
          unify universe universe'
        plain Constraint Constraint = pure ()
        plain Small Small = pure ()
        plain Large Large = pure ()
        plain Universe Universe = pure ()
        plain Levity Levity = pure ()
        plain _ _ = mismatch

    mismatch :: ST s ()
    mismatch = abort position (Unify context_ term1_ term2_)

-- |
-- What a type variable is known to be where this is: see 'Local.Equality'.
equated :: Context s scope -> Local.Index.Index scope -> Maybe (Type s scope)
equated Context {equalities} index = case [typex | Local.Equality index' typex <- equalities, index' == index] of
  typex : _ -> Just typex
  [] -> Nothing

-- |
-- What a type that stands for itself alone is known to be where this is.
equatedHidden :: Context s scope -> Logical s scope -> Maybe (Type s scope)
equatedHidden Context {equalities} logical =
  case [typex | Local.Hidden logical' typex <- equalities, same logical logical'] of
    typex : _ -> Just typex
    [] -> Nothing

-- |
-- Take in what a match tells of a type its own constructor hides, if this
-- is that, and say whether it was. Such a type is only ever met inside the
-- match, and there it is what the match says it is: it need not be kept
-- as something known for the branch, and is made to be that type for good.
-- The type may then be part of the types that are seen from outside, as
-- the type it is.
--
-- > Reach :: Collected s scopes -> Collected s (scope ':+ scopes)
--
-- matched against a @Collected s (a ':+ b)@ hides nothing: its @scopes@ is
-- @b@.
absorb :: [Type s scope] -> Local.Equality s scope -> ST s Bool
absorb own = \case
  Local.Hidden logical@(Box reference) typex
    | any (is logical) own -> do
        writeSTRef reference $! Solved typex
        pure True
  _ -> pure False
  where
    is logical = \case
      Logical other -> same logical other
      _ -> False

-- |
-- Whether two unknowns are the one unknown.
same :: Logical s scope -> Logical s scope -> Bool
same (Box reference) (Box reference') = reference == reference'
same (Shift logical) (Shift logical') = same logical logical'
same _ _ = False

-- |
-- A type that stands for itself alone, if a type is one, however it was
-- come by.
hidden :: Type s scope -> ST s (Maybe (Logical s scope))
hidden = \case
  Logical logical ->
    solution logical >>= \case
      Just typex -> hidden typex
      Nothing -> do
        rigid <- rigid logical
        pure (if rigid then Just logical else Nothing)
  _ -> pure Nothing
  where
    rigid :: Logical s scope' -> ST s Bool
    rigid = \case
      Box reference ->
        readSTRef reference >>= \case
          Skolem {} -> pure True
          _ -> pure False
      Shift logical -> rigid logical

-- |
-- Check that none of some types that stand for themselves alone is part of
-- a type. They are what a constructor hides, and the type is one that is
-- seen from outside the match on that constructor, where there is no saying
-- what they are.
sealed :: forall s scope. Context s scope -> Position -> [Type s scope] -> Type s scope -> ST s ()
sealed context position own typex = do
  found <- part typex
  when found $ abort position (Escape context typex)
  where
    part :: Type s scope -> ST s Bool
    part = \case
      Logical logical ->
        solution logical >>= \case
          Just known -> part known
          Nothing -> pure (any (is logical) own)
      Call function argument -> either (part function) (part argument)
      Function argument result -> either (part argument) (part result)
      Type universe -> part universe
      _ -> pure False
    is logical = \case
      Logical other -> same logical other
      _ -> False
    either left right = do
      left <- left
      if left then pure True else right

-- |
-- What is known of a type variable, as it is known one scope further out,
-- if it can be said there: the variable has to be from there, and so does
-- every variable and every unknown in what it is known to be.
lowerEquality :: Local.Equality s (scope' ':+ scope) -> Maybe (Local.Equality s scope)
lowerEquality = \case
  Local.Equality index typex -> Local.Equality <$> Shift.partialUnshift Nothing index <*> lower typex
  Local.Hidden (Shift logical) typex -> Local.Hidden logical <$> lower typex
  Local.Hidden (Box _) _ -> Nothing
  -- The evidence is named by the match, and there is no match further out.
  Local.Given {} -> Nothing
  Local.Late waiting -> Just (Local.Late waiting)
  Local.Grouping members ->
    Local.Grouping <$> traverse (\(index, kind) -> (,) <$> Shift.partialUnshift Nothing index <*> lower kind) members
  Local.Expanding index -> Local.Expanding <$> Shift.partialUnshift Nothing index
  where
    lower :: Type s (scope' ':+ scope) -> Maybe (Type s scope)
    lower = \case
      Logical (Shift logical) -> Just (Logical logical)
      Logical (Box _) -> Nothing
      Variable index -> Variable <$> Shift.partialUnshift Nothing index
      Constructor index -> Constructor <$> Shift.partialUnshift Nothing index
      Call function argument -> Call <$> lower function <*> lower argument
      Function argument result -> Function <$> lower argument <*> lower result
      Type universe -> Type <$> lower universe
      Constraint -> Just Constraint
      Small -> Just Small
      Large -> Just Large
      Universe -> Just Universe
      Levity -> Just Levity

-- |
-- Match a type that the type of what is taken apart is given against what
-- a constructor says it is given, and say what that tells of the types in
-- the first.
--
-- Where the first has an unknown, the unknown is made what the second has,
-- as when two types are made the same. Where it has a type variable and the
-- second something else, nothing can be made the same: the variable is the
-- signature's and stands for any type. But in the branch the match leads to
-- it is known to be what the constructor says, and that is what is given
-- back, for the branch to be checked with. A type that some constructor
-- hides is like a variable in this.
--
-- If the two differ in a way that cannot be, no value of that constructor
-- has the type that is taken apart, and the branch is never reached.
-- Nothing is said of that here.
refine :: forall s scope. Context s scope -> Position -> Type s scope -> Type s scope -> ST s [Local.Equality s scope]
refine context position = go []
  where
    go :: [Local.Equality s scope] -> Type s scope -> Type s scope -> ST s [Local.Equality s scope]
    go found left right = do
      left <- settled left
      right <- settled right
      leftHidden <- hidden left
      rightHidden <- hidden right
      let Context {equalities} = context
          here = context {equalities = found ++ equalities}
          plain = found <$ unify here position left right
          -- A type cannot be known to be something it is part of.
          learn equality typex = do
            circular <- within here equality typex
            pure (if circular then found else equality typex : found)
      case (left, right) of
        _ | Just logical <- leftHidden, Just logical' <- rightHidden, same logical logical' -> pure found
        _ | Just logical <- leftHidden, Just known <- equatedHidden here logical -> go found known right
        _ | Just logical <- rightHidden, Just known <- equatedHidden here logical -> go found left known
        (Variable index, Variable index') | index == index' -> pure found
        (Variable index, _) | Just known <- equated here index -> go found known right
        (_, Variable index') | Just known <- equated here index' -> go found left known
        (Logical _, _) | Nothing <- leftHidden -> plain
        (_, Logical _) | Nothing <- rightHidden -> plain
        _ | Just logical <- rightHidden -> learn (Local.Hidden logical) left
        _ | Just logical <- leftHidden -> learn (Local.Hidden logical) right
        (Variable index, _) -> learn (Local.Equality index) right
        (_, Variable index') -> learn (Local.Equality index') left
        (Call function argument, Call function' argument') -> do
          found <- go found function function'
          go found argument argument'
        (Function argument result, Function argument' result') -> do
          found <- go found argument argument'
          go found result result'
        (Function {}, Call {}) -> plain
        (Call {}, Function {}) -> plain
        (Type universe, Type universe') -> go found universe universe'
        -- Anything else is either the same on both sides or cannot be.
        _ -> pure found
    -- A type as far as it has been worked out. An unknown from a scope
    -- further out that has been worked out is the type it was found to
    -- be: the type of a variable that was bound further out comes this way.
    settled :: Type s scope -> ST s (Type s scope)
    settled = \case
      Logical logical ->
        solution logical >>= \case
          Just typex -> settled typex
          Nothing -> pure (Logical logical)
      other -> pure other

-- |
-- Whether what would be known to be a type is part of that type.
within ::
  forall s scope.
  Context s scope ->
  (Type s scope -> Local.Equality s scope) ->
  Type s scope ->
  ST s Bool
within context equality typex = part typex
  where
    part :: Type s scope -> ST s Bool
    part = \case
      Logical logical ->
        solution logical >>= \case
          Just known -> part known
          Nothing -> case equality typex of
            Local.Hidden subject _
              | same subject logical -> pure True
              | Just known <- equatedHidden context logical -> part known
            _ -> pure False
      Variable index -> case equality typex of
        Local.Equality subject _ | subject == index -> pure True
        _ | Just known <- equated context index -> part known
        _ -> pure False
      Call function argument -> either (part function) (part argument)
      Function argument result -> either (part argument) (part result)
      Type universe -> part universe
      _ -> pure False
    either left right = do
      left <- left
      if left then pure True else right

-- |
-- What an unknown was found to be, if it has been.
solution :: Logical s scope -> ST s (Maybe (Type s scope))
solution = \case
  Box reference ->
    readSTRef reference >>= \case
      Solved typex -> pure (Just typex)
      Unsolved {} -> pure Nothing
      Skolem {} -> pure Nothing
  Shift logical -> fmap shiftLogical <$> solution logical

-- todo merge this with Semantic.Check.Temporary.Type checking somehow
typeCheck :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
typeCheck context_ position = typeCheckWith context_
  where
    typeCheckWith :: Context s scope -> Type s scope -> Type s scope -> ST s ()
    typeCheckWith context@Context {localEnvironment} = typeCheck
      where
        typeCheck kind = \case
          Logical (Box reference) ->
            readSTRef reference >>= \case
              Solved typex -> typeCheck kind typex
              Unsolved {kind = kind'} -> do
                unify context position kind kind'
              Skolem {kind = kind'} -> do
                unify context position kind kind'
          Logical (Shift logical) -> do
            kind <- unshift context position kind
            typeCheckWith (Shift.unshift context) kind (Logical logical)
          Variable index -> case localEnvironment Local.Table.! index of
            Local.Rigid {rigid} -> do
              unify context position kind (logicalType rigid)
            Local.Wobbly {wobbly} -> do
              unify context position kind wobbly
          Constructor constructor -> do
            kind' <- Context.lookupKind position context constructor
            unify context position kind' kind
          Constraint -> unify context position (Type Large) kind
          Levity -> unify context position (Type Large) kind
          Small -> unify context position Universe kind
          Large -> unify context position Universe kind
          Universe -> error "type checking universe"
          Call functionx parameter -> do
            level <- fresh Universe
            parameterKind <- fresh (Type level)
            typeCheck (Function parameterKind kind) functionx
            typeCheck parameterKind parameter
          Function argument result -> do
            level <- fresh Universe
            level' <- fresh Universe
            unify context position (Type level') kind
            typeCheck (Type level) argument
            typeCheck (Type level') result
          Type universe -> do
            unify context position Small universe
            unify context position (Type Large) kind

-- |
-- Marking a type with `Known` forces a type to not be erased so that the
-- runtime can know what it is. `mark` only forces the head normal form of a
-- type to be known. So `mark (List a)` will only force `List` to be known, not
-- `a`.
--
-- This is done to allow type class variable to not be counted as runtime
-- variables in type class default methods.
mark :: forall s scope. Context s scope -> Position -> Mask.Erasure -> Type s scope -> ST s ()
mark context position erasure term_ = mark context term_
  where
    mark :: Context s scope' -> Type s scope' -> ST s ()
    mark context@Context {localEnvironment} = \case
      Logical (Box reference')
        | otherwise ->
            readSTRef reference' >>= \case
              Unsolved {kind, constraints, erasure = erasure'} -> do
                writeSTRef reference' Unsolved {kind, constraints, erasure = erasure <> erasure'}
              Solved typex -> mark context typex
              Skolem {} -> pure ()
      Logical (Shift logical) -> do
        mark (Shift.unshift context) (Logical logical)
      Variable index -> case localEnvironment Local.Table.! index of
        Local.Rigid {mask}
          | Mask.valid mask erasure -> pure ()
          | otherwise -> mismask
        Local.Wobbly {} -> case erasure of
          Mask.Erased -> pure ()
          Mask.Known -> error "runtime mask on wobbly"
      Constructor _ -> pure ()
      Call term1 _ -> mark context term1
      Function {} -> pure ()
      Type {} -> pure ()
      Constraint -> pure ()
      Small -> pure ()
      Large -> pure ()
      Universe -> pure ()
      Levity -> pure ()
    mismask = abort position (Mismask context term_)

occurs ::
  Context s scope ->
  Position ->
  Mask.Erasure ->
  STRef s (Box s scope) ->
  Type s scope ->
  ST s ()
occurs context position erasure reference term_ = do
  occurs term_
  mark context position erasure term_
  where
    occurs = \case
      Logical (Box reference')
        | reference == reference' -> occurrenece
        | otherwise ->
            readSTRef reference' >>= \case
              Unsolved {} -> pure ()
              Solved typex -> occurs typex
              Skolem {} -> pure ()
      Logical Shift {} -> pure ()
      Variable _ -> pure ()
      Constructor _ -> pure ()
      Call term1 term2 -> do
        occurs term1
        occurs term2
      Function argument result -> do
        occurs argument
        occurs result
      Type universe -> do
        occurs universe
      Constraint -> pure ()
      Small -> pure ()
      Large -> pure ()
      Universe -> pure ()
      Levity -> pure ()
    occurrenece = abort position (Occurs context reference term_)

reconstrain ::
  Context s scope ->
  Position ->
  Map (Type2.Index scope) [Delay s scope] ->
  Type s scope ->
  ST s ()
reconstrain context position constraints term =
  for_ (Map.toList constraints) $ \(classx, delays) ->
    for_ delays $ \Delay {arguments, evidence} -> do
      evidence' <- constrainWith context position classx term arguments
      Evidence.unify context position evidence evidence'

-- |
-- The first of some things that passes a test.
firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstM test = \case
  [] -> pure Nothing
  first : rest -> do
    passed <- test first
    if passed then pure (Just first) else firstM test rest

-- |
-- Whether two lists of types are the same as they stand: nothing is made
-- to be anything. Types that are not worked out yet are the same only if
-- they are the same unknown.
sameAll :: [Type s scope] -> [Type s scope] -> ST s Bool
sameAll = comparedAll False

-- |
-- Whether two lists of types could be the same: a type that is not worked
-- out yet could be anything.
alikeAll :: [Type s scope] -> [Type s scope] -> ST s Bool
alikeAll = comparedAll True

-- |
-- Whether two types could be the same, with nothing made to be anything.
alike :: Type s scope -> Type s scope -> ST s Bool
alike = compared True

comparedAll :: Bool -> [Type s scope] -> [Type s scope] -> ST s Bool
comparedAll open left right
  | length left /= length right = pure False
  | otherwise = and <$> zipWithM (compared open) left right

compared :: forall s scope. Bool -> Type s scope -> Type s scope -> ST s Bool
compared open = go
  where
    go :: forall scope'. Type s scope' -> Type s scope' -> ST s Bool
    go left right = case (left, right) of
      (Logical (Box reference), _) ->
        readSTRef reference >>= \case
          Solved left -> go left right
          Unsolved {} -> unknown reference right
          Skolem {} -> itself reference right
      (_, Logical (Box reference)) ->
        readSTRef reference >>= \case
          Solved right -> go left right
          Unsolved {} -> pure open
          Skolem {} -> case left of
            Logical _ -> pure open
            _ -> pure False
      (Logical (Shift left), Logical (Shift right)) -> go (Logical left) (Logical right)
      -- One is from a scope further out and the other is not: if it is
      -- worked out, it is looked at as it is here.
      (Logical (Shift logical), _) ->
        known logical >>= \case
          Just left -> go (shiftLogical left) right
          Nothing -> pure open
      (_, Logical (Shift logical)) ->
        known logical >>= \case
          Just right -> go left (shiftLogical right)
          Nothing -> pure open
      (Variable left, Variable right) -> pure (left == right)
      (Constructor left, Constructor right) -> pure (left == right)
      (Call function argument, Call function' argument') -> both (go function function') (go argument argument')
      (Function argument result, Function argument' result') -> both (go argument argument') (go result result')
      (Type left, Type right) -> go left right
      (Constraint, Constraint) -> pure True
      (Small, Small) -> pure True
      (Large, Large) -> pure True
      (Universe, Universe) -> pure True
      (Levity, Levity) -> pure True
      _ -> pure False
    both left right = do
      left <- left
      if left then right else pure False
    -- What an unknown is, if it has been worked out.
    known :: forall scope'. Logical s scope' -> ST s (Maybe (Type s scope'))
    known = \case
      Box reference ->
        readSTRef reference >>= \case
          Solved typex -> pure (Just typex)
          Unsolved {} -> pure Nothing
          Skolem {} -> pure Nothing
      Shift logical -> fmap shiftLogical <$> known logical
    -- An unknown on the left: the same as itself, and otherwise only what
    -- an unknown could be.
    unknown :: forall scope'. STRef s (Box s scope') -> Type s scope' -> ST s Bool
    unknown reference = \case
      Logical (Box reference')
        | reference == reference' -> pure True
        | otherwise ->
            readSTRef reference' >>= \case
              Solved right -> unknown reference right
              Unsolved {} -> pure open
              Skolem {} -> pure open
      _ -> pure open
    -- A type that stands for itself alone on the left: the same as itself,
    -- and otherwise only what an unknown could be.
    itself :: forall scope'. STRef s (Box s scope') -> Type s scope' -> ST s Bool
    itself reference = \case
      Logical (Box reference')
        | reference == reference' -> pure True
        | otherwise ->
            readSTRef reference' >>= \case
              Solved right -> itself reference right
              Unsolved {} -> pure open
              Skolem {} -> pure False
      Logical (Shift _) -> pure open
      _ -> pure False

unshiftBox ::
  Context s (scope1 ':+ scopes) ->
  Position ->
  ST s Void ->
  (Type s (scope1 ':+ scopes) -> ST s (Type s scopes)) ->
  STRef s (Box s (scope2 ':+ scopes)) ->
  TypeF (Logical s (scope1 ':+ scopes)) (scope1 ':+ scopes) ->
  Map (Type2.Index (scope3 ':+ scopes)) [Delay s (scope1 ':+ scopes)] ->
  Mask.Erasure ->
  ST s (Logical s scopes)
unshiftBox context position misshift unshift reference kind constraints erasure = do
  let unshiftDelay (key, delays) = do
        key <- Shift.partialUnshift misshift key
        delays <- for delays $ \Delay {arguments, evidence} -> do
          arguments <- traverse unshift arguments
          evidence <- Evidence.unshift context position evidence
          pure Delay {arguments, evidence}
        pure (key, delays)
  kind <- unshift kind
  -- Type variables may link to constraints that mention the variable
  -- itself. To allow unshifting these cycles, the unshifted box is
  -- initialized with undefined then later let to it's proper value.
  box <- newSTRef $ error "unshift cycle"
  writeSTRef reference $! Solved $ Logical $ Shift $ Box box
  -- there's no Map.traverseKeysMonotonic
  constraints <- Map.fromAscList <$> traverse unshiftDelay (Map.toAscList constraints)
  writeSTRef box $! Unsolved {kind, constraints, erasure}
  pure $ Box box

unshift ::
  forall s scope scopes.
  Context s (scope ':+ scopes) ->
  Position ->
  Type s (scope ':+ scopes) ->
  ST s (Type s scopes)
unshift context position typex = unshift typex
  where
    unshift = \case
      Logical (Box reference) ->
        readSTRef reference >>= \case
          Unsolved {kind, constraints, erasure} -> do
            box <- unshiftBox context position misshift unshift reference kind constraints erasure
            pure $ Logical box
          Solved term -> unshift term
          -- It is of this scope, and cannot be said further out.
          Skolem {} -> abort position $ Unshift context typex
      Logical (Shift logical) -> pure (Logical logical)
      Variable index -> Variable <$> Shift.partialUnshift misshift index
      Constructor index -> Constructor <$> Shift.partialUnshift misshift index
      Call term1 term2 -> do
        term1 <- unshift term1
        term2 <- unshift term2
        pure (Call term1 term2)
      Function argument result -> do
        argument <- unshift argument
        result <- unshift result
        pure (Function argument result)
      Type universe -> do
        universe <- unshift universe
        pure (Type universe)
      Constraint -> pure Constraint
      Small -> pure Small
      Large -> pure Large
      Universe -> pure Universe
      Levity -> pure Levity
    misshift = do
      abort position $ Unshift context typex

-- |
-- What a type variable that was never worked out may be taken to be, if
-- anything. This is the Haskell report's defaulting, with its default list
-- of @Integer@ and then @Double@: every class the variable is constrained
-- by must be one that does not stop it, at least one must be a class of
-- numbers, and the type is the first of the two that is an instance of
-- every one.
defaults :: [Default] -> STRef s (Box s scope) -> ST s (Maybe Default)
defaults listed reference =
  readSTRef reference >>= \case
    Solved typex -> follow typex
    Unsolved {constraints} -> pure (firstFitting listed constraints)
    Skolem {} -> pure Nothing
  where
    -- The variable may have been made the same as another since.
    follow :: Type s scope -> ST s (Maybe Default)
    follow = \case
      Logical (Box reference) -> defaults listed reference
      Logical (Shift logical) -> follow (Logical logical)
      _ -> pure Nothing

-- |
-- The first of the types listed that is an instance of every class asked:
-- one that is not a fraction, where none of the classes is of fractions,
-- and one that is, where none is @Integral@.
firstFitting :: [Default] -> Map (Type2.Index scope) [Delay s scope] -> Maybe Default
firstFitting listed constraints = do
  classes <- traverse Evidence.defaultable (Map.keys constraints)
  if all plain (concat (Map.elems constraints)) && any Evidence.numeric classes
    then find (fits classes) listed
    else Nothing
  where
    plain Delay {arguments} = null arguments
    fits classes candidate = all (instanceOf candidate) classes
    -- Which of the classes that do not stop defaulting each type is an
    -- instance of.
    instanceOf candidate = \case
      Evidence.Integral -> not (Default.fractional candidate)
      Evidence.Bounded -> candidate == Default.Int
      classx
        | Evidence.fractional classx -> Default.fractional candidate
        | otherwise -> True

-- |
-- What a variable's evidence was taken to be, if it was: the type it is
-- taken to be is the same, as the module it was asked in has it.
chosen :: Map (Type2.Index scope) [Delay s scope] -> ST s (Maybe Default)
chosen constraints = go (concat (Map.elems constraints))
  where
    go = \case
      [] -> pure Nothing
      Delay {evidence} : rest ->
        fallbackOf evidence >>= \case
          Just found -> pure (Just found)
          Nothing -> go rest
    fallbackOf :: Evidence s scope' -> ST s (Maybe Default)
    fallbackOf = \case
      Evidence.Logical logical -> logicalOf logical
      _ -> pure Nothing
    logicalOf :: Evidence.Logical s scope' -> ST s (Maybe Default)
    logicalOf = \case
      Evidence.Box reference ->
        readSTRef reference >>= \case
          Evidence.Unsolved {fallback = Just Evidence.Fallback {permitted}} -> permitted
          _ -> pure Nothing
      Evidence.Shift logical -> logicalOf logical

defaultFrom,
  defaultUniverse ::
    Position ->
    Map (Type2.Index scope') [Delay s scope'] ->
    Type s scope'' ->
    ST s (Simple.Type scope)
defaultFrom position constraints kind = case kind of
  Logical (Shift logical) -> defaultFrom position constraints (Logical logical)
  Logical (Box reference) ->
    readSTRef reference >>= \case
      Solved kind -> defaultFrom position constraints kind
      Unsolved {} -> unsupportedFeatureConstraintedTypeDefaulting position
      Skolem {} -> unsupportedFeatureConstraintedTypeDefaulting position
  Type universe -> defaultUniverse position constraints universe
  Levity -> pure $ Constructor Type2.Lazy
  _ -> unsupportedFeatureConstraintedTypeDefaulting position
defaultUniverse position constraints universe = case universe of
  Logical (Shift logical) -> defaultUniverse position constraints (Logical logical)
  Logical (Box reference) ->
    readSTRef reference >>= \case
      Solved kind -> defaultUniverse position constraints kind
      Unsolved {}
        | null constraints -> pure $ Type Small
        | otherwise -> fromChosen
      Skolem {} -> unsupportedFeatureConstraintedTypeDefaulting position
  Small
    | null constraints -> pure $ Constructor (Type2.Tuple 0)
    | otherwise -> fromChosen
  Large
    | null constraints -> pure $ Type Small
    | otherwise -> error "unexpected kind constraints"
  _ -> error "unexpected universe kind"
  where
    fromChosen =
      chosen constraints >>= \case
        Just typex -> pure $ Constructor (Default.typeOf typex)
        Nothing -> unsupportedFeatureConstraintedTypeDefaulting position
