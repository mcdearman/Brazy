-- |
-- Evaluate at once what is certain to be evaluated.
--
-- Haskell is lazy, so an argument that is not a value already is passed as
-- a thunk, and so is anything that is given a name. A thunk is an object,
-- forcing it needs a consumer that is an object too, and what it holds is
-- kept until it is forced. But a great many are forced whatever happens:
-- the function they are passed to looks at them first thing, or the name is
-- used on every path. Those can be evaluated where they are written, and
-- nothing is made.
--
-- This is safe when the value is certain to be forced before anything that
-- can be seen from outside happens: before the function gives its result
-- back, and before anything is printed. If evaluating it early does not
-- end, or stops the program, then evaluating it later would have done the
-- same, with nothing seen in between. All that can differ is which of two
-- failures a program that was going to fail stops with.
--
-- What can be seen from outside is done only by running an action, and an
-- action is a function of no arguments. So a function that is given
-- arguments only works a value out, and what follows a call of one still
-- counts as before anything is seen. Running an action does not, and
-- nothing after it is counted.
--
-- There are two parts. 'demand' says which of a function's parameters it is
-- certain to force. 'strengthen' then changes a definition: where a known
-- function is called with every argument, the arguments it is certain to
-- force are evaluated first, and a thunk that is given a name and is certain
-- to be forced after is evaluated there. It also keeps track of the names
-- that hold a value, and takes away the forcing of those. A name that has
-- been forced is written from there on as the name of its value, so that
-- what is kept and passed on is the value and not the thunk it was.
--
-- A function is called the same way as before, and still forces what it is
-- given, so a caller that knows nothing of the function and passes thunks
-- is as right as it was. The one exception is the entry of a definition
-- that takes every argument together ('Sequent.Uncurry'). Nothing can get
-- to that but a call that names the definition, and every such call is
-- changed here, so that entry takes what it is certain to force as values
-- and does not force them: see 'valued'.
module Sequent.Strict (Demand, demand, strengthen, valued) where

import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Generate.Global as Global (Global)
import qualified Sequent.Primitive as Primitive (Primitive (..))
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Producer (..),
    Statement (..),
    Strategy (..),
    free,
  )

-- |
-- For each parameter of a function, whether the function is certain to
-- force it before anything it does can be seen.
type Demand = [Bool]

-- |
-- What is known of the definitions something mentions: the demand of each
-- that is a function, and whether calling it with every argument runs an
-- action.
data Known = Known
  { demands :: Global.Global -> Maybe Demand,
    acts :: Global.Global -> Bool
  }

-- |
-- The variables that are certain to be forced before anything is seen.
data Forced
  = -- | Nothing is ever seen, because the program stops: any variable may
    -- be said to be forced.
    Everything
  | Only (Set Text)

none :: Forced
none = Only Set.empty

-- |
-- What is forced whichever of two ways is gone.
either' :: Forced -> Forced -> Forced
either' Everything other = other
either' other Everything = other
either' (Only left) (Only right) = Only (Set.intersection left right)

-- |
-- What is forced by one thing and then another.
then' :: Forced -> Forced -> Forced
then' Everything _ = Everything
then' _ Everything = Everything
then' (Only left) (Only right) = Only (Set.union left right)

certain :: Text -> Forced -> Bool
certain name = \case
  Everything -> True
  Only names -> Set.member name names

-- |
-- What running a statement is certain to force. It is given what each
-- consumer that has a name is certain to force once it has a value.
running :: Known -> Map Text Forced -> Statement -> Forced
running known consumers = \case
  Halt -> none
  Abort -> Everything
  LetRec _ body -> running known consumers body
  Primitive primitive _ result -> case primitive of
    Primitive.Error -> Everything
    Primitive.PutStrLn -> none
    _ -> given known consumers result
  Cut producer consumer -> case (producer, consumer) of
    (Variable name, Force inner) -> then' (Only (Set.singleton name)) (given known consumers inner)
    (Mu name body, _) -> running known (Map.insert name (given known consumers consumer) consumers) body
    -- A thunk that is certain to be forced forces what is in it.
    (Thunk _ body, MuTilde Binder {name} rest) ->
      let after = running known consumers rest
       in if certain name after then then' after (running known Map.empty body) else after
    (Global global, Apply arguments result)
      | Just strict <- demands known global,
        length strict == length arguments ->
          let passed = foldr then' none [argument known value | (True, value) <- zip strict arguments]
           in if acts known global then passed else then' passed (given known consumers result)
    _ -> given known consumers consumer

-- |
-- What a consumer is certain to force once it is given a value.
given :: Known -> Map Text Forced -> Consumer -> Forced
given known consumers = \case
  Covariable name -> Map.findWithDefault none name consumers
  MuTilde _ body -> running known consumers body
  Case alternatives otherwise ->
    foldr
      either'
      (maybe Everything (running known consumers) otherwise)
      [running known consumers body | Alternative _ _ body <- alternatives]
  -- Running an action is where something can be seen.
  Apply [] _ -> none
  Apply _ result -> given known consumers result
  Force result -> given known consumers result

-- |
-- What evaluating an argument forces.
argument :: Known -> Producer -> Forced
argument known = \case
  Variable name -> Only (Set.singleton name)
  Thunk _ body -> running known Map.empty body
  _ -> none

-- |
-- The demand of a definition, if it is a function. It is given the demand
-- of the other definitions and whether each runs an action, and its own
-- name: a call of itself is taken to force what the whole is found to, which
-- is right because a function that only ever calls itself gives nothing
-- back.
demand :: (Global.Global -> Maybe Demand) -> (Global.Global -> Bool) -> Global.Global -> Producer -> Maybe Demand
demand others acting self = \case
  Function parameters _ body ->
    let lazy = [strategy == ByNeed | Binder {strategy} <- parameters]
        step assumed =
          let demands global
                | global == self = Just assumed
                | otherwise = others global
              forced = running Known {demands, acts = acting} Map.empty body
           in [wanted && certain name forced | (wanted, Binder {name}) <- zip lazy parameters]
        settle assumed =
          let next = step assumed
           in if next == assumed then assumed else settle next
     in Just (settle lazy)
  _ -> Nothing

-- |
-- A function that takes as values the parameters it is certain to force.
-- This is only for a function whose every caller is known to evaluate them
-- first: it no longer forces them itself.
valued :: Demand -> Producer -> Producer
valued strict = \case
  Function parameters result body ->
    let taken (wanted, binder) = if wanted then binder {strategy = ByValue} else binder
     in Function (map taken (zip strict parameters)) result body
  other -> other

-- |
-- Where something is: the names that hold a value, the names that are
-- written as another, and what the consumers that have a name are certain
-- to force.
data Scope = Scope
  { values :: Set Text,
    written :: Map Text Text,
    consumers :: Map Text Forced
  }

-- |
-- A definition with what is certain to be evaluated evaluated at once. It
-- is given which time over this is, for the names it makes.
strengthen :: Int -> (Global.Global -> Maybe Demand) -> (Global.Global -> Bool) -> Producer -> Producer
strengthen time demanded acting producer =
  runST (strengthen' time Known {demands = demanded, acts = acting} producer)

strengthen' :: Int -> Known -> Producer -> ST s Producer
strengthen' time known definition = do
  supply <- newSTRef (0 :: Int)
  let fresh = do
        number <- readSTRef supply
        modifySTRef' supply (+ 1)
        pure $ Text.pack ("v!" ++ show time ++ "." ++ show number)
      current Scope {written} variable = Map.findWithDefault variable variable written
      holding scope binders =
        scope {values = values scope <> Set.fromList [variable | Binder {name = variable, strategy = ByValue} <- binders]}
      valued scope variable = scope {values = Set.insert variable (values scope)}
      -- From here on one name is written as another.
      writing scope old new =
        let change other = if other == old then new else other
         in scope {written = Map.insert old new (Map.map change (written scope))}
      -- Inside a function or a thunk, which is run from somewhere else.
      inside scope = scope {consumers = Map.empty}
      evaluated binder = binder {strategy = ByValue}
      producerx scope = \case
        Variable variable -> pure $ Variable (current scope variable)
        Constructor tag fields -> Constructor tag <$> traverse (producerx scope) fields
        Mu covariable body -> Mu covariable <$> statement scope body
        Function parameters result body ->
          Function parameters result <$> statement (holding (inside scope) parameters) body
        Thunk result body -> Thunk result <$> statement (inside scope) body
        other -> pure other
      consumer scope = \case
        Covariable covariable -> pure $ Covariable covariable
        MuTilde binder body -> MuTilde binder <$> statement (holding scope [binder]) body
        Case alternatives otherwise -> do
          alternatives <- for alternatives $ \(Alternative tag binders body) ->
            Alternative tag binders <$> statement (holding scope binders) body
          otherwise <- traverse (statement scope) otherwise
          pure $ Case alternatives otherwise
        Apply arguments result -> Apply <$> traverse (producerx scope) arguments <*> consumer scope result
        Force result -> Force <$> consumer scope result
      -- The arguments of a call, with those that are certain to be forced
      -- evaluated before it, and what does that.
      prepare _ [] = pure ([], id)
      prepare scope ((strict, value) : rest) = do
        (value, first) <- case (strict, value) of
          (True, Variable variable)
            | Set.notMember (current scope variable) (values scope) -> do
                new <- fresh
                let binder = Binder {name = new, strategy = ByValue}
                pure (Variable new, Cut (Variable (current scope variable)) . Force . MuTilde binder)
          (True, Thunk result body) -> do
            new <- fresh
            body <- statement scope {consumers = Map.insert result none (consumers scope)} body
            let binder = Binder {name = new, strategy = ByValue}
            pure (Variable new, Cut (Mu result body) . MuTilde binder)
          -- A definition that is not a function may not be evaluated yet.
          (True, Global global)
            | Nothing <- demands known global -> do
                new <- fresh
                let binder = Binder {name = new, strategy = ByValue}
                pure (Variable new, Cut (Global global) . Force . MuTilde binder)
          (True, Mu result body) -> do
            new <- fresh
            body <- statement scope {consumers = Map.insert result none (consumers scope)} body
            let binder = Binder {name = new, strategy = ByValue}
            pure (Variable new, Cut (Mu result body) . MuTilde binder)
          _ -> do
            value <- producerx scope value
            pure (value, id)
        (values', later) <- prepare scope rest
        pure (value : values', first . later)
      statement scope = \case
        Halt -> pure Halt
        Abort -> pure Abort
        LetRec bindings body -> do
          bindings <- for bindings $ \(local, value) -> do
            value <- producerx scope value
            pure (local, value)
          LetRec bindings <$> statement scope body
        Primitive primitive arguments result -> do
          arguments <- traverse (producerx scope) arguments
          case (primitive, result) of
            -- A field may be a thunk. Everything else gives a value.
            (Primitive.Field _, _) -> Primitive primitive arguments <$> consumer scope result
            (_, MuTilde binder@Binder {name = variable} body) ->
              Primitive primitive arguments . MuTilde (evaluated binder) <$> statement (valued scope variable) body
            _ -> Primitive primitive arguments <$> consumer scope result
        Cut (Variable variable) (Force inner)
          -- A value that is forced is itself.
          | Set.member (current scope variable) (values scope) ->
              Cut (Variable (current scope variable)) <$> consumer scope inner
          | otherwise -> case inner of
              -- What the value is called is what the variable is from here.
              MuTilde binder@Binder {name = new} body -> do
                body <- statement (writing (valued scope new) (current scope variable) new) body
                pure $ Cut (Variable (current scope variable)) (Force (MuTilde (evaluated binder) body))
              -- If the variable is used again the value is given a name,
              -- so that it is.
              _
                | any (\other -> current scope other == current scope variable) (Set.toList (free inner)) -> do
                    new <- fresh
                    let binder = Binder {name = new, strategy = ByValue}
                    statement scope (Cut (Variable variable) (Force (MuTilde binder (Cut (Variable new) inner))))
                | otherwise -> Cut (Variable (current scope variable)) . Force <$> consumer scope inner
        -- A thunk that is certain to be forced is evaluated where it is.
        Cut (Thunk result body) (MuTilde binder@Binder {name = variable} rest)
          | forced <- running known (consumers scope) rest,
            certain variable forced -> do
              body <- statement scope {consumers = Map.insert result forced (consumers scope)} body
              rest <- statement (valued scope variable) rest
              pure $ Cut (Mu result body) (MuTilde (evaluated binder) rest)
        Cut (Global global) (Apply arguments result)
          | Just strict <- demands known global,
            length strict == length arguments,
            or strict -> do
              (arguments, first) <- prepare scope (zip strict arguments)
              result <- consumer scope result
              pure $ first (Cut (Global global) (Apply arguments result))
        Cut (Mu covariable body) other -> do
          let forced = given known (consumers scope) other
          body <- statement scope {consumers = Map.insert covariable forced (consumers scope)} body
          other <- consumer scope other
          pure $ Cut (Mu covariable body) other
        Cut producer (MuTilde binder@Binder {name = variable} rest)
          | plain producer -> do
              producer <- producerx scope producer
              rest <- statement (valued scope variable) rest
              pure $ Cut producer (MuTilde (evaluated binder) rest)
        Cut producer other -> Cut <$> producerx scope producer <*> consumer scope other
  producerx Scope {values = Set.empty, written = Map.empty, consumers = Map.empty} definition

-- |
-- Whether a producer is a value as it stands, with nothing to evaluate.
plain :: Producer -> Bool
plain = \case
  Literal _ -> True
  Constructor _ _ -> True
  Function {} -> True
  _ -> False
