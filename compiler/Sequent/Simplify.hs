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
--
-- The rest is for what is left when a function's body is put where it is
-- called ('Sequent.Inline'). A value that is constructed and at once taken
-- apart is not constructed: the alternative for it is gone to. A consumer
-- that is named and only ever given constructed values, each choosing an
-- alternative of its own, is put where they are, so that a comparison goes
-- straight on to what its answer chooses. A value that is forced is itself.
-- A name for a name, a number, a constructor with no fields or a definition
-- is written as what it names. A thunk that is forced as soon as it is
-- named is evaluated first. A function that is written where it is called
-- is its body. And one thing in a @let@ that does not refer to itself is
-- only named, which lets the rest of this see it.
--
-- A thunk that does nothing but construct a value is the value, and so is
-- one that first names things that are only made, which are then named
-- outside it. The translation delays everything a constructor is given and
-- everything that is named, and a value that is written out has nothing to
-- delay: each letter of a name that a derived @Show@ writes was a thunk,
-- and a function for the thunk, to make one cell of a list.
module Sequent.Simplify (simplify) where

import Data.List (nub)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Sequent.Tree as Literal (Literal (..))
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Producer (..),
    Statement (..),
    Strategy (..),
    free,
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

  -- | Write a variable as what it names.
  replace :: Text -> Producer -> a -> a

instance (Simplify a) => Simplify [a] where
  simplify = map simplify
  count name = sum . map (count name)
  put name consumer = map (put name consumer)
  unfold name result body = map (unfold name result body)
  replace name value = map (replace name value)

instance (Simplify a) => Simplify (Maybe a) where
  simplify = fmap simplify
  count name = maybe 0 (count name)
  put name consumer = fmap (put name consumer)
  unfold name result body = fmap (unfold name result body)
  replace name value = fmap (replace name value)

instance Simplify Producer where
  simplify = \case
    Constructor tag fields -> Constructor tag (map settle (simplify fields))
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
  replace name value = \case
    Variable other
      | other == name -> value
      | otherwise -> Variable other
    Constructor tag fields -> Constructor tag (replace name value fields)
    Mu covariable body -> Mu covariable (replace name value body)
    Function parameters covariable body -> Function parameters covariable (replace name value body)
    Thunk covariable body -> Thunk covariable (replace name value body)
    other -> other

instance Simplify Consumer where
  simplify = \case
    Covariable name -> Covariable name
    MuTilde binder body -> MuTilde binder (simplify body)
    Case alternatives otherwise -> Case (simplify alternatives) (simplify otherwise)
    Apply arguments result -> Apply (map settle (simplify arguments)) (simplify result)
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
  replace name value = \case
    Covariable other -> Covariable other
    MuTilde binder body -> MuTilde binder (replace name value body)
    Case alternatives otherwise -> Case (replace name value alternatives) (replace name value otherwise)
    Apply arguments result -> Apply (replace name value arguments) (replace name value result)
    Force result -> Force (replace name value result)

instance Simplify Alternative where
  simplify (Alternative tag binders body) = Alternative tag binders (simplify body)
  count name (Alternative _ _ body) = count name body
  put name consumer (Alternative tag binders body) = Alternative tag binders (put name consumer body)
  unfold name result body (Alternative tag binders inner) = Alternative tag binders (unfold name result body inner)
  replace name value (Alternative tag binders body) = Alternative tag binders (replace name value body)

instance Simplify Statement where
  simplify = \case
    Cut producer consumer -> cut (simplify producer) (simplify consumer)
    Primitive primitive arguments result -> Primitive primitive (simplify arguments) (simplify result)
    -- One thing that does not refer to itself is only named.
    LetRec [(name, value)] body
      | Set.notMember name (free value) ->
          cut (simplify value) (MuTilde Binder {name, strategy = ByNeed} (simplify body))
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
  replace name value = \case
    Cut producer consumer -> Cut (replace name value producer) (replace name value consumer)
    Primitive primitive arguments result ->
      Primitive primitive (replace name value arguments) (replace name value result)
    LetRec bindings body ->
      LetRec [(local, replace name value other) | (local, other) <- bindings] (replace name value body)
    Abort -> Abort
    Halt -> Halt

-- |
-- A cut of two sides that are as simple as they can be alone.
cut :: Producer -> Consumer -> Statement
cut producer consumer = case (producer, consumer) of
  -- A consumer that is named and used once, or not at all.
  (Mu name body, _) -> case (count name body, consumer) of
    (0, _) -> body
    (1, _) -> simplify (put name consumer body)
    (_, Covariable _) -> put name consumer body
    -- One that is only given constructed values, no two of which choose
    -- the same alternative: nothing is written twice.
    (uses, Case alternatives otherwise)
      | given <- constructed name body,
        length given == uses,
        chosen <- [choice tag alternatives | tag <- given],
        nub chosen == chosen,
        length (filter (== Nothing) chosen) <= maybe 0 (const 1) otherwise ->
          simplify (put name consumer body)
    _ -> Cut producer consumer
  -- A thunk whose one use is to be forced, outside any function.
  (Thunk result body, MuTilde Binder {name} rest)
    | count name rest == 1,
      forces name rest == 1 ->
        unfold name result body rest
  -- A thunk that is forced as soon as it is named is evaluated first, and
  -- what is named is its value.
  (Thunk result body, MuTilde binder@Binder {name} (Cut (Variable other) (Force inner)))
    | other == name ->
        cut (Mu result body) (MuTilde binder (Cut (Variable name) inner))
  -- A thunk that only constructs a value is the value. What it named on
  -- the way is named first.
  (Thunk _ _, MuTilde _ _)
    | Just (named, made) <- floated producer ->
        foldr (\(binder, given) rest -> Cut given (MuTilde binder rest)) (cut made consumer) named
  -- A value that is constructed and taken apart.
  (Constructor tag fields, Case alternatives otherwise) -> case choice tag alternatives of
    Just index
      | Alternative _ binders body <- alternatives !! index,
        length binders == length fields ->
          foldr (\(binder, field) rest -> cut field (MuTilde binder rest)) body (zip binders fields)
    _ -> case otherwise of
      Just body -> body
      Nothing -> Abort
  -- A value that is forced.
  (_, Force inner) | value producer -> cut producer inner
  -- A function that is written where it is called: its body, with the
  -- parameters naming what they were given.
  (Function parameters result body, Apply arguments inner)
    | length parameters == length arguments ->
        let bind (binder, argument) rest = Cut argument (MuTilde binder rest)
         in simplify (Cut (Mu result (foldr bind body (zip parameters arguments))) inner)
  -- A name for something that is as short to write.
  (_, MuTilde Binder {name} rest)
    | short producer -> case count name rest of
        0 -> rest
        _ -> simplify (replace name producer rest)
  _ -> Cut producer consumer

-- |
-- Which of some alternatives is for a tag.
choice :: Int -> [Alternative] -> Maybe Int
choice tag alternatives = lookup tag (zip [other | Alternative other _ _ <- alternatives] [0 ..])

-- |
-- Whether a producer is a value as it stands, with nothing to evaluate.
value :: Producer -> Bool
value = \case
  Literal _ -> True
  Constructor _ _ -> True
  Function {} -> True
  _ -> False

-- |
-- A thunk that does nothing but construct a value, as the value. Anything
-- else is itself.
settle :: Producer -> Producer
settle = \case
  Thunk result (Cut made (Covariable given))
    | given == result,
      built made ->
        made
  other -> other

-- |
-- A thunk that names some things that are only made and then constructs a
-- value: what it names, and the value.
floated :: Producer -> Maybe ([(Binder, Producer)], Producer)
floated = \case
  Thunk result body -> go result body
  _ -> Nothing
  where
    go result = \case
      Cut made (Covariable given)
        | given == result,
          built made,
          count result made == 0 ->
            Just ([], made)
      Cut made (MuTilde binder@Binder {strategy = ByNeed} rest)
        | given <- settle made,
          allocated given,
          count result given == 0 -> do
            (named, final) <- go result rest
            Just ((binder, given) : named, final)
      _ -> Nothing
    allocated = \case
      Thunk {} -> True
      other -> built other

-- |
-- Whether to make a producer is only to construct it: nothing is evaluated,
-- so it may as well be made now as when it is asked for.
built :: Producer -> Bool
built = \case
  Literal _ -> True
  Constructor _ fields -> all inert fields
  Function {} -> True
  _ -> False
  where
    inert = \case
      Variable _ -> True
      Global _ -> True
      Thunk {} -> True
      other -> built other

-- |
-- Whether a producer is no more to write than a name for it would be.
short :: Producer -> Bool
short = \case
  Variable _ -> True
  Literal (Literal.Int _) -> True
  Constructor _ [] -> True
  Global _ -> True
  _ -> False

-- |
-- The tags of the constructed values a covariable is given, where it is
-- given one and nothing else is done with it.
constructed :: Text -> Statement -> [Int]
constructed name = statement
  where
    statement = \case
      Cut (Constructor tag fields) (Covariable other)
        | other == name -> tag : concatMap inProducer fields
      Cut producer consumer -> inProducer producer ++ inConsumer consumer
      Primitive _ arguments consumer -> concatMap inProducer arguments ++ inConsumer consumer
      LetRec bindings body -> concatMap (inProducer . snd) bindings ++ statement body
      Abort -> []
      Halt -> []
    inProducer = \case
      Constructor _ fields -> concatMap inProducer fields
      Mu _ body -> statement body
      Function _ _ body -> statement body
      Thunk _ body -> statement body
      _ -> []
    inConsumer = \case
      Covariable _ -> []
      MuTilde _ body -> statement body
      Case alternatives otherwise ->
        concat [statement body | Alternative _ _ body <- alternatives] ++ maybe [] statement otherwise
      Apply arguments consumer -> concatMap inProducer arguments ++ inConsumer consumer
      Force consumer -> inConsumer consumer

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
