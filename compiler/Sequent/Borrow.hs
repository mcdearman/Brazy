-- |
-- Which parameters a function only reads.
--
-- A function owns what it is given: each argument is a reference that the
-- function has to give up, and the caller that still wants the value had
-- to count one more for it. For a function that only looks at an argument,
-- that is counting for nothing, and for one that goes down a structure it
-- is counting at every step: each part is shared as it is read out, and
-- the part it was read out of is given up.
--
-- So a function that only reads a parameter can take it lent. It counts
-- nothing for it, or for what it reads out of it, and whoever lent it goes
-- on holding it. What is said here is which parameters that is worth doing
-- for. Whether a use needs to own the value is settled again where the
-- code is generated ("LLVM.FromAxCut"), which shares a lent value at each
-- use that does. So nothing is wrong if this says too much, only slower.
--
-- A parameter is only read when every use of it is to take it apart, to
-- give it to a primitive, or to pass it on, lent, to a function that only
-- reads it too, and it holds a value: a thunk has to be owned to be forced.
-- It is owned when it is kept in a constructor, a function, a thunk or a
-- consumer that is made, given back, called, or taken apart where a value
-- of as many fields is built, since that is built in its place.
module Sequent.Borrow (Lending, lending) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Generate.Global as Global (Global)
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
-- For each parameter of a function, whether the function only reads it.
type Lending = [Bool]

-- |
-- Which parameters a definition only reads, if it is a function and there
-- are any. It is given what is known of the other definitions, and its own
-- name: a call of itself is taken to read what the whole is found to.
lending :: (Global.Global -> Maybe Lending) -> Global.Global -> Producer -> Maybe Lending
lending others self = \case
  Function parameters result body ->
    let looked Binder {name, strategy} = strategy == ByValue && Set.member name (free body)
        step assumed =
          let known global
                | global == self = Just assumed
                | otherwise = others global
           in [wanted && not (owned known result name body) | (wanted, Binder {name}) <- zip assumed parameters]
        settle assumed =
          let next = step assumed
           in if next == assumed then assumed else settle next
        found = settle (map looked parameters)
     in if or found then Just found else Nothing
  _ -> Nothing

-- |
-- Whether a statement has a use of a variable that needs to own it. It is
-- given the consumer of the function's result: something lent can only be
-- passed on with a consumer that holds that one, which is what keeps the
-- value alive.
owned :: (Global.Global -> Maybe Lending) -> Text -> Text -> Statement -> Bool
owned known result name = statement
  where
    mentions producer = Set.member name (free producer)
    mentioning consumer = Set.member name (free consumer)
    -- An argument that is the variable is a use to be judged where it is.
    kept = \case
      Variable _ -> False
      other -> mentions other
    -- A consumer that is not a name is an object, and holds what it uses.
    after = \case
      Covariable _ -> False
      other -> mentioning other
    holding = \case
      Covariable other -> other == result
      other -> Set.member result (free other)
    statement = \case
      Halt -> False
      Abort -> False
      LetRec bindings body -> any (mentions . snd) bindings || statement body
      Primitive _ arguments consumer ->
        any kept arguments || case consumer of
          MuTilde _ rest -> statement rest
          other -> after other
      Cut producer consumer -> case (producer, consumer) of
        (Variable other, Case alternatives otherwise) ->
          (other == name && any rebuilt alternatives)
            || or [statement body | Alternative _ _ body <- alternatives]
            || maybe False statement otherwise
        (Global global, Apply arguments inner)
          | Just lent <- known global,
            length lent == length arguments ->
              let passed (only, argument) = case argument of
                    Variable other | other == name -> not (only && holding inner)
                    _ -> mentions argument
               in any passed (zip lent arguments) || after inner
        (Mu _ body, _) -> statement body || mentioning consumer
        (_, MuTilde _ rest) -> mentions producer || statement rest
        _ -> mentions producer || mentioning consumer

-- |
-- Whether an alternative builds a value with as many fields as it took
-- apart, which is then built in the place of what was taken apart.
rebuilt :: Alternative -> Bool
rebuilt (Alternative _ binders body) = not (null binders) && builds (length binders) body

builds :: Int -> Statement -> Bool
builds size = statement
  where
    statement = \case
      Halt -> False
      Abort -> False
      LetRec _ body -> statement body
      Primitive _ arguments result -> any producer arguments || consumer result
      Cut left right -> producer left || consumer right
    producer = \case
      Constructor _ fields -> length fields == size || any producer fields
      Mu _ body -> statement body
      _ -> False
    consumer = \case
      Covariable _ -> False
      MuTilde _ body -> statement body
      Case alternatives otherwise ->
        or [statement body | Alternative _ _ body <- alternatives] || maybe False statement otherwise
      Apply arguments result -> any producer arguments || consumer result
      Force result -> consumer result
