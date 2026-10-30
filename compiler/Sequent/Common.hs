-- |
-- Two thunks that work out the same thing are one, as GHC's common
-- subexpressions are. A tree whose two halves are made by the same call,
-- @Node (make (d - 1)) (make (d - 1))@, makes the half once and holds it
-- twice, and the work of making it is done once.
--
-- Thunks are the same if they are written the same but for the names they
-- bind. Two that are the fields of one constructor, or the arguments of one
-- call, are named once before it. A thunk that is named where one that is
-- the same was named, with nothing it uses named again in between, is a
-- name for that one, which "Sequent.Simplify" then writes as it.
--
-- A thunk that calls something with no arguments is never one with another:
-- what it calls is an action, which runs each time it is called, or a value
-- that is called by name, as @byname :: () => Int@ is, which is worked out
-- each time it is asked for.
module Sequent.Common (common) where

import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
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
-- A thunk that is named, what it uses, and its name.
data Named = Named
  { thunk :: Producer,
    uses :: Set Text,
    named :: Text
  }

common :: Producer -> Producer
common definition = runST $ do
  counter <- newSTRef (0 :: Int)
  producer (names definition) counter [] definition

-- |
-- What is named where something is, with what a binder there names taken
-- out: a thunk that uses one of them, or is named by one, is not seen past
-- it.
hide :: [Text] -> [Named] -> [Named]
hide hidden = filter visible
  where
    gone = Set.fromList hidden
    visible Named {uses, named} = Set.notMember named gone && Set.null (uses `Set.intersection` gone)

-- |
-- A name of one's own, that is not any name the definition has.
fresh :: Set Text -> STRef s Int -> ST s Text
fresh taken counter = do
  number <- readSTRef counter
  modifySTRef' counter (+ 1)
  let name = Text.pack ("common%" ++ show number)
  if Set.member name taken then fresh taken counter else pure name

producer :: Set Text -> STRef s Int -> [Named] -> Producer -> ST s Producer
producer taken counter seen = \case
  Constructor tag fields -> Constructor tag <$> traverse (producer taken counter seen) fields
  Mu covariable body -> Mu covariable <$> statement taken counter (hide [covariable] seen) body
  Function parameters covariable body ->
    Function parameters covariable
      <$> statement taken counter (hide (covariable : [name | Binder {name} <- parameters]) seen) body
  Thunk covariable body -> Thunk covariable <$> statement taken counter (hide [covariable] seen) body
  other -> pure other

consumer :: Set Text -> STRef s Int -> [Named] -> Consumer -> ST s Consumer
consumer taken counter seen = \case
  MuTilde binder@Binder {name} body -> MuTilde binder <$> statement taken counter (hide [name] seen) body
  Case alternatives otherwise ->
    Case
      <$> for alternatives (\(Alternative tag binders body) -> Alternative tag binders <$> statement taken counter (hide [name | Binder {name} <- binders] seen) body)
      <*> traverse (statement taken counter seen) otherwise
  Apply arguments result -> Apply <$> traverse (producer taken counter seen) arguments <*> consumer taken counter seen result
  Force result -> Force <$> consumer taken counter seen result
  other -> pure other

statement :: Set Text -> STRef s Int -> [Named] -> Statement -> ST s Statement
statement taken counter seen = \case
  -- A thunk named where one that is the same was named is a name for it.
  Cut made@Thunk {} (MuTilde binder@Binder {name, strategy = ByNeed} rest)
    | Just Named {named} <- earlier made ->
        Cut (Variable named) . MuTilde binder <$> statement taken counter (hide [name] seen) rest
    | otherwise -> do
        made' <- producer taken counter seen made
        let new = [Named {thunk = made, uses = free made, named = name} | not (runs made)]
        Cut made' . MuTilde binder <$> statement taken counter (new ++ hide [name] seen) rest
  -- The same thunk given twice to one constructor or call is named once.
  Cut (Constructor tag fields) result
    | Just shared <- twice fields ->
        share shared (\fields -> Cut (Constructor tag fields) result) fields
  Cut function (Apply arguments result)
    | Just shared <- twice arguments ->
        share shared (\arguments -> Cut function (Apply arguments result)) arguments
  Cut left right -> Cut <$> producer taken counter seen left <*> consumer taken counter seen right
  Primitive primitive arguments result ->
    Primitive primitive <$> traverse (producer taken counter seen) arguments <*> consumer taken counter seen result
  LetRec bindings body -> do
    let inside = hide (map fst bindings) seen
    bindings <- for bindings $ \(name, value) -> do
      value <- producer taken counter inside value
      pure (name, value)
    LetRec bindings <$> statement taken counter inside body
  other -> pure other
  where
    earlier made = case [named | named@Named {thunk} <- seen, same thunk made] of
      found : _ -> Just found
      [] -> Nothing
    -- A thunk that is given more than once, or that was named already.
    twice values =
      case [ value
             | (index, value@Thunk {}) <- zip [0 :: Int ..] values,
               not (runs value),
               any (same value) (drop (index + 1) values) || any (same value . thunk) seen
           ] of
        value : _ -> Just value
        [] -> Nothing
    share shared rebuild values = case [named | named@Named {thunk} <- seen, same thunk shared] of
      Named {named} : _ -> statement taken counter seen (rebuild (map (replace named) values))
      [] -> do
        name <- fresh taken counter
        let binder = Binder {name, strategy = ByNeed}
        statement taken counter seen (Cut shared (MuTilde binder (rebuild (map (replace name) values))))
      where
        replace name value
          | same value shared = Variable name
          | otherwise = value

-- |
-- Whether something calls anything with no arguments.
runs :: Producer -> Bool
runs = producerRuns
  where
    producerRuns = \case
      Constructor _ fields -> any producerRuns fields
      Mu _ body -> statementRuns body
      Function _ _ body -> statementRuns body
      Thunk _ body -> statementRuns body
      _ -> False
    consumerRuns = \case
      MuTilde _ body -> statementRuns body
      Case alternatives otherwise -> or [statementRuns body | Alternative _ _ body <- alternatives] || any statementRuns otherwise
      Apply [] _ -> True
      Apply arguments result -> any producerRuns arguments || consumerRuns result
      Force result -> consumerRuns result
      Covariable _ -> False
    statementRuns = \case
      Cut left right -> producerRuns left || consumerRuns right
      Primitive _ arguments result -> any producerRuns arguments || consumerRuns result
      LetRec bindings body -> any (producerRuns . snd) bindings || statementRuns body
      _ -> False

-- |
-- Whether two thunks, or anything else, are written the same but for the
-- names they bind.
same :: Producer -> Producer -> Bool
same = producerSame Map.empty

type Renaming = Map Text Text

variable :: Renaming -> Text -> Text -> Bool
variable renaming left right = case Map.lookup left renaming of
  Just bound -> bound == right
  Nothing -> left == right && right `notElem` Map.elems renaming

binding :: Renaming -> [Binder] -> [Binder] -> Maybe Renaming
binding renaming lefts rights
  | length lefts == length rights,
    and [leftStrategy == rightStrategy | (Binder {strategy = leftStrategy}, Binder {strategy = rightStrategy}) <- zip lefts rights] =
      Just (foldr (\(Binder {name = left}, Binder {name = right}) -> Map.insert left right) renaming (zip lefts rights))
  | otherwise = Nothing

producerSame :: Renaming -> Producer -> Producer -> Bool
producerSame renaming left right = case (left, right) of
  (Variable left, Variable right) -> variable renaming left right
  (Global left, Global right) -> left == right
  (Literal left, Literal right) -> left == right
  (Constructor leftTag lefts, Constructor rightTag rights) ->
    leftTag == rightTag && length lefts == length rights && and (zipWith (producerSame renaming) lefts rights)
  (Mu leftCovariable leftBody, Mu rightCovariable rightBody) ->
    statementSame (Map.insert leftCovariable rightCovariable renaming) leftBody rightBody
  (Function leftParameters leftCovariable leftBody, Function rightParameters rightCovariable rightBody)
    | Just inner <- binding renaming leftParameters rightParameters ->
        statementSame (Map.insert leftCovariable rightCovariable inner) leftBody rightBody
  (Thunk leftCovariable leftBody, Thunk rightCovariable rightBody) ->
    statementSame (Map.insert leftCovariable rightCovariable renaming) leftBody rightBody
  _ -> False

consumerSame :: Renaming -> Consumer -> Consumer -> Bool
consumerSame renaming left right = case (left, right) of
  (Covariable left, Covariable right) -> variable renaming left right
  (MuTilde leftBinder leftBody, MuTilde rightBinder rightBody)
    | Just inner <- binding renaming [leftBinder] [rightBinder] ->
        statementSame inner leftBody rightBody
  (Case lefts leftOtherwise, Case rights rightOtherwise) ->
    length lefts == length rights
      && and (zipWith alternative lefts rights)
      && case (leftOtherwise, rightOtherwise) of
        (Nothing, Nothing) -> True
        (Just left, Just right) -> statementSame renaming left right
        _ -> False
  (Apply lefts leftResult, Apply rights rightResult) ->
    length lefts == length rights
      && and (zipWith (producerSame renaming) lefts rights)
      && consumerSame renaming leftResult rightResult
  (Force left, Force right) -> consumerSame renaming left right
  _ -> False
  where
    alternative (Alternative leftTag leftBinders leftBody) (Alternative rightTag rightBinders rightBody)
      | leftTag == rightTag,
        Just inner <- binding renaming leftBinders rightBinders =
          statementSame inner leftBody rightBody
      | otherwise = False

statementSame :: Renaming -> Statement -> Statement -> Bool
statementSame renaming left right = case (left, right) of
  (Cut leftProducer leftConsumer, Cut rightProducer rightConsumer) ->
    producerSame renaming leftProducer rightProducer && consumerSame renaming leftConsumer rightConsumer
  (Primitive leftPrimitive lefts leftResult, Primitive rightPrimitive rights rightResult) ->
    leftPrimitive == rightPrimitive
      && length lefts == length rights
      && and (zipWith (producerSame renaming) lefts rights)
      && consumerSame renaming leftResult rightResult
  (Abort, Abort) -> True
  (Halt, Halt) -> True
  -- Functions that refer to each other are not looked into.
  _ -> False

-- |
-- Every name a definition has, bound or used.
names :: Producer -> Set Text
names = producerNames
  where
    producerNames = \case
      Variable name -> Set.singleton name
      Constructor _ fields -> foldMap producerNames fields
      Mu covariable body -> Set.insert covariable (statementNames body)
      Function parameters covariable body ->
        Set.insert covariable (Set.fromList [name | Binder {name} <- parameters] <> statementNames body)
      Thunk covariable body -> Set.insert covariable (statementNames body)
      _ -> Set.empty
    consumerNames = \case
      Covariable name -> Set.singleton name
      MuTilde Binder {name} body -> Set.insert name (statementNames body)
      Case alternatives otherwise ->
        mconcat [Set.fromList [name | Binder {name} <- binders] <> statementNames body | Alternative _ binders body <- alternatives]
          <> foldMap statementNames otherwise
      Apply arguments result -> foldMap producerNames arguments <> consumerNames result
      Force result -> consumerNames result
    statementNames = \case
      Cut left right -> producerNames left <> consumerNames right
      Primitive _ arguments result -> foldMap producerNames arguments <> consumerNames result
      LetRec bindings body -> Set.fromList (map fst bindings) <> foldMap (producerNames . snd) bindings <> statementNames body
      _ -> Set.empty
