-- |
-- Small changes to the sequent calculus that leave a program meaning the
-- same and making less.
--
-- Two things that the translation from Core writes, because it does not
-- look ahead, are undone here.
--
-- A consumer that is named and then used once is put where it is used. The
-- translation names the place a failed match goes on to, and the consumer
-- of a choice; when there turns out to be one use, naming it made an object
-- for nothing.
--
-- A thunk that is forced once, and not from inside a function, is evaluated
-- where it is forced. The translation delays the condition of an @if@ and
-- then looks at it straight away.
module Sequent.Simplify (simplify) where

import Data.Text (Text)
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Producer (..),
    Statement (..),
  )

class Simplify a where
  simplify :: a -> a

  -- | How many times a variable or a covariable is mentioned. Every name
  -- in a definition is bound once, so a mention is of the binding meant.
  count :: Text -> a -> Int

  -- | Put a consumer in the place of a covariable.
  put :: Text -> Consumer -> a -> a

  -- | Evaluate a thunk where it is forced: the place of the one force of a
  -- variable is given the thunk's body, with the consumer it had there.
  -- Nothing is done inside a function, which may run many times where the
  -- thunk would have been evaluated once.
  unfold :: Text -> Text -> Statement -> a -> a

instance (Simplify a) => Simplify [a] where
  simplify = map simplify
  count name = sum . map (count name)
  put name consumer = map (put name consumer)
  unfold name result body = map (unfold name result body)

instance (Simplify a) => Simplify (Maybe a) where
  simplify = fmap simplify
  count name = maybe 0 (count name)
  put name consumer = fmap (put name consumer)
  unfold name result body = fmap (unfold name result body)

instance Simplify Producer where
  simplify = \case
    Constructor tag fields -> Constructor tag (simplify fields)
    Mu covariable body -> Mu covariable (simplify body)
    Function parameters covariable body -> Function parameters covariable (simplify body)
    Thunk covariable body -> Thunk covariable (simplify body)
    other -> other
  count name = \case
    Variable other -> if other == name then 1 else 0
    Global _ -> 0
    Literal _ -> 0
    Constructor _ fields -> count name fields
    Mu _ body -> count name body
    Function _ _ body -> count name body
    Thunk _ body -> count name body
  put name consumer = \case
    Constructor tag fields -> Constructor tag (put name consumer fields)
    Mu covariable body -> Mu covariable (put name consumer body)
    Function parameters covariable body -> Function parameters covariable (put name consumer body)
    Thunk covariable body -> Thunk covariable (put name consumer body)
    other -> other
  unfold name result body = \case
    Constructor tag fields -> Constructor tag (unfold name result body fields)
    Mu covariable inner -> Mu covariable (unfold name result body inner)
    Thunk covariable inner -> Thunk covariable (unfold name result body inner)
    other -> other

instance Simplify Consumer where
  simplify = \case
    Covariable name -> Covariable name
    MuTilde binder body -> MuTilde binder (simplify body)
    Case alternatives otherwise -> Case (simplify alternatives) (simplify otherwise)
    Apply arguments result -> Apply (simplify arguments) (simplify result)
    Force result -> Force (simplify result)
  count name = \case
    Covariable other -> if other == name then 1 else 0
    MuTilde _ body -> count name body
    Case alternatives otherwise -> count name alternatives + count name otherwise
    Apply arguments result -> count name arguments + count name result
    Force result -> count name result
  put name consumer = \case
    Covariable other
      | other == name -> consumer
      | otherwise -> Covariable other
    MuTilde binder body -> MuTilde binder (put name consumer body)
    Case alternatives otherwise -> Case (put name consumer alternatives) (put name consumer otherwise)
    Apply arguments result -> Apply (put name consumer arguments) (put name consumer result)
    Force result -> Force (put name consumer result)
  unfold name result body = \case
    Covariable other -> Covariable other
    MuTilde binder inner -> MuTilde binder (unfold name result body inner)
    Case alternatives otherwise -> Case (unfold name result body alternatives) (unfold name result body otherwise)
    Apply arguments consumer -> Apply (unfold name result body arguments) (unfold name result body consumer)
    Force consumer -> Force (unfold name result body consumer)

instance Simplify Alternative where
  simplify (Alternative tag binders body) = Alternative tag binders (simplify body)
  count name (Alternative _ _ body) = count name body
  put name consumer (Alternative tag binders body) = Alternative tag binders (put name consumer body)
  unfold name result body (Alternative tag binders inner) = Alternative tag binders (unfold name result body inner)

instance Simplify Statement where
  simplify = \case
    Cut producer consumer -> case (simplify producer, simplify consumer) of
      -- A consumer that is named and used once, or not at all.
      (Mu name body, consumer) -> case (count name body, consumer) of
        (0, _) -> body
        (1, _) -> put name consumer body
        (_, Covariable _) -> put name consumer body
        _ -> Cut (Mu name body) consumer
      -- A thunk whose one use is to be forced, outside any function.
      (Thunk result body, MuTilde Binder {name} rest)
        | count name rest == 1,
          forces name rest == 1 ->
            unfold name result body rest
      (producer, consumer) -> Cut producer consumer
    Primitive primitive arguments result -> Primitive primitive (simplify arguments) (simplify result)
    LetRec bindings body -> LetRec [(name, simplify value) | (name, value) <- bindings] (simplify body)
    Abort -> Abort
    Halt -> Halt
  count name = \case
    Cut producer consumer -> count name producer + count name consumer
    Primitive _ arguments result -> count name arguments + count name result
    LetRec bindings body -> count name (map snd bindings) + count name body
    Abort -> 0
    Halt -> 0
  put name consumer = \case
    Cut producer other -> Cut (put name consumer producer) (put name consumer other)
    Primitive primitive arguments result ->
      Primitive primitive (put name consumer arguments) (put name consumer result)
    LetRec bindings body ->
      LetRec [(local, put name consumer value) | (local, value) <- bindings] (put name consumer body)
    Abort -> Abort
    Halt -> Halt
  unfold name result body = \case
    Cut (Variable other) (Force consumer)
      | other == name -> Cut (Mu result body) consumer
    Cut producer consumer -> Cut (unfold name result body producer) (unfold name result body consumer)
    Primitive primitive arguments consumer ->
      Primitive primitive (unfold name result body arguments) (unfold name result body consumer)
    LetRec bindings inner ->
      LetRec [(local, unfold name result body value) | (local, value) <- bindings] (unfold name result body inner)
    Abort -> Abort
    Halt -> Halt

-- |
-- How many times a variable is forced where 'unfold' would reach it: as the
-- whole of a statement, and outside any function.
forces :: Text -> Statement -> Int
forces name = statement
  where
    statement = \case
      Cut (Variable other) (Force consumer)
        | other == name -> 1 + inConsumer consumer
      Cut producer consumer -> inProducer producer + inConsumer consumer
      Primitive _ arguments consumer -> sum (map inProducer arguments) + inConsumer consumer
      LetRec bindings body -> sum (map (inProducer . snd) bindings) + statement body
      Abort -> 0
      Halt -> 0
    inProducer = \case
      Constructor _ fields -> sum (map inProducer fields)
      Mu _ body -> statement body
      Thunk _ body -> statement body
      _ -> 0
    inConsumer = \case
      Covariable _ -> 0
      MuTilde _ body -> statement body
      Case alternatives otherwise ->
        sum [statement body | Alternative _ _ body <- alternatives] + maybe 0 statement otherwise
      Apply arguments consumer -> sum (map inProducer arguments) + inConsumer consumer
      Force consumer -> inConsumer consumer
