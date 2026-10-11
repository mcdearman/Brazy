-- |
-- Call a function that is known with all of its arguments at once.
--
-- A Haskell function of two arguments is a function of one that gives back
-- a function of one, and that is how it is applied when nothing is known of
-- it: an argument at a time, each application making a function and waiting
-- on a consumer for it. But most calls are of a definition at the top of
-- the program with every argument there.
--
-- So a definition that is written as functions inside functions is given a
-- second entry, called by the definition's name and then @direct@, that
-- takes every argument together. The definition itself stays, as the
-- functions it was, each only gathering its arguments for the second entry.
-- A call of the definition that has every argument goes to the second
-- entry, and makes nothing.
--
-- The evidence a definition takes for its constraints counts as its first
-- arguments.
module Sequent.Uncurry (Shape, shape, split, direct, whole, rewrite) where

import Data.Text (pack)
import qualified Data.Text as Text
import qualified Generate.Global as Global (Global (..))
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Producer (..),
    Statement (..),
    separator,
  )

-- |
-- How many arguments a definition takes at each application, when it is
-- functions inside functions.
type Shape = [Int]

-- |
-- The functions a definition is made of, from the outside in: the parameters
-- and the consumer of each, and the body of the last.
levels :: Producer -> Maybe ([([Binder], Text.Text)], Statement)
levels = \case
  Function parameters result body -> Just $ case body of
    -- A function that does nothing but give back another.
    Cut inner@Function {} (Covariable given)
      | given == result,
        Just (rest, last') <- levels inner ->
          ((parameters, result) : rest, last')
    _ -> ([(parameters, result)], body)
  _ -> Nothing

shape :: Producer -> Maybe Shape
shape producer = do
  (functions, _) <- levels producer
  pure [length parameters | (parameters, _) <- functions]

-- |
-- What the entry that takes every argument is called.
direct :: Global.Global -> Global.Global
direct global@Global.Global {name} = global {Global.name = Text.snoc name separator <> pack "direct"}

-- |
-- The definition that an entry that takes every argument is an entry of.
-- Anything else is itself.
whole :: Global.Global -> Global.Global
whole global@Global.Global {name} = case Text.stripSuffix (pack (separator : "direct")) name of
  Just before -> global {Global.name = before}
  Nothing -> global

-- |
-- A definition as it is left, and the entry that takes every argument, if
-- the definition is more than one function deep.
split :: Global.Global -> Producer -> (Producer, Maybe (Global.Global, Producer))
split global producer = case levels producer of
  Just (functions@(_ : _ : _), body) ->
    let everything = concat [parameters | (parameters, _) <- functions]
        (_, final) = last functions
        call = Cut (Global (direct global)) (Apply [Variable name | Binder {name} <- everything] (Covariable final))
        gather [] = error "a definition with no functions"
        gather [(parameters, result)] = Function parameters result call
        gather ((parameters, result) : rest) = Function parameters result (Cut (gather rest) (Covariable result))
     in (gather functions, Just (direct global, Function everything final body))
  _ -> (producer, Nothing)

-- |
-- Send the calls that have every argument to the entry that takes them
-- together. A definition that is known to be a function is also not forced:
-- there is nothing to evaluate.
class Rewrite a where
  rewrite :: (Global.Global -> Maybe Shape) -> a -> a

instance (Rewrite a) => Rewrite [a] where
  rewrite known = map (rewrite known)

instance (Rewrite a) => Rewrite (Maybe a) where
  rewrite known = fmap (rewrite known)

instance Rewrite Producer where
  rewrite known = \case
    Constructor tag fields -> Constructor tag (rewrite known fields)
    Mu covariable body -> Mu covariable (rewrite known body)
    Function parameters covariable body -> Function parameters covariable (rewrite known body)
    Thunk covariable body -> Thunk covariable (rewrite known body)
    other -> other

instance Rewrite Consumer where
  rewrite known = \case
    Covariable name -> Covariable name
    MuTilde binder body -> MuTilde binder (rewrite known body)
    Case alternatives otherwise -> Case (rewrite known alternatives) (rewrite known otherwise)
    Apply arguments result -> Apply (rewrite known arguments) (rewrite known result)
    Force result -> Force (rewrite known result)

instance Rewrite Alternative where
  rewrite known (Alternative tag binders body) = Alternative tag binders (rewrite known body)

instance Rewrite Statement where
  rewrite known = \case
    Cut (Global global) consumer
      | Just counts <- known global ->
          let -- Forcing a function gives the function.
              unforced = case consumer of
                Force inner -> inner
                other -> other
              (lists, rest) = applications unforced
              (used, later) = splitAt (length counts) lists
              complete = length used == length counts && and (zipWith (\count list -> length list == count) counts used)
           in if length counts >= 2 && complete
                then
                  Cut
                    (Global (direct global))
                    (Apply (rewrite known (concat used)) (rewrite known (foldr Apply rest later)))
                else Cut (Global global) (rewrite known unforced)
    Cut producer consumer -> Cut (rewrite known producer) (rewrite known consumer)
    Primitive primitive arguments result -> Primitive primitive (rewrite known arguments) (rewrite known result)
    LetRec bindings body -> LetRec [(name, rewrite known value) | (name, value) <- bindings] (rewrite known body)
    Abort -> Abort
    Halt -> Halt
    where
      -- The applications a consumer starts with, and what follows them.
      applications = \case
        Apply arguments result ->
          let (lists, rest) = applications result
           in (arguments : lists, rest)
        other -> ([], other)
