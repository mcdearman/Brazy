-- |
-- Put the body of a small function where it is called.
--
-- A call of a definition costs more than its going there and coming back.
-- The consumer of its result is an object, an argument that is not worked
-- out yet is a thunk, and neither side can be made simpler by what the
-- other is. Most of what is called is very small: a comparison of two
-- numbers is a method that calls a primitive and then @not@. With the body
-- in the place of the call, 'Sequent.Simplify' sees that the thunk is
-- forced at once and that the consumer is used once, and nothing is made.
--
-- A definition that is only a number or a constructor with no fields, as
-- @otherwise@ is, is written as that where it is forced.
--
-- Only a call that gives every argument is changed, and only when what is
-- called is a function that is small and does not call itself. The names
-- in the body are made new, so that every name in a definition is still
-- bound once.
--
-- The bodies that are put in are those of the definitions as they were
-- before this was done to them. So this can be done over again, each time
-- reaching one call further, and it always ends.
module Sequent.Inline (inline) where

import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Generate.Global as Global (Global)
import qualified Sequent.Tree as Literal (Literal (..))
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Producer (..),
    Statement (..),
    Strategy (..),
    globals,
  )

-- |
-- The most a function can be made of and still be put where it is called.
limit :: Int
limit = 30

-- |
-- A definition with the small functions it calls put in. It is given which
-- time over this is, what each definition was the time before, and the name
-- of the definition.
inline :: Int -> (Global.Global -> Maybe Producer) -> Global.Global -> Producer -> Producer
inline time find self producer = runST (inline' time find self producer)

inline' :: Int -> (Global.Global -> Maybe Producer) -> Global.Global -> Producer -> ST s Producer
inline' time find self producer = do
  sites <- newSTRef (0 :: Int)
  let producerx = \case
        Constructor tag fields -> Constructor tag <$> traverse producerx fields
        Mu covariable body -> Mu covariable <$> statement body
        Function parameters covariable body -> Function parameters covariable <$> statement body
        Thunk covariable body -> Thunk covariable <$> statement body
        other -> pure other
      consumer = \case
        MuTilde binder body -> MuTilde binder <$> statement body
        Case alternatives otherwise -> do
          alternatives <- for alternatives $ \(Alternative tag binders body) ->
            Alternative tag binders <$> statement body
          otherwise <- traverse statement otherwise
          pure $ Case alternatives otherwise
        Apply arguments result -> Apply <$> traverse producerx arguments <*> consumer result
        Force result -> Force <$> consumer result
        other -> pure other
      statement = \case
        Cut (Global global) (Apply arguments result)
          | global /= self,
            Just (Function parameters covariable body) <- find global,
            length parameters == length arguments,
            and [strategy == ByNeed | Binder {strategy} <- parameters],
            size body <= limit,
            Set.notMember global (globals body) -> do
              arguments <- traverse producerx arguments
              result <- consumer result
              site <- readSTRef sites
              modifySTRef' sites (+ 1)
              let suffix = Text.pack ('^' : show time ++ "." ++ show site)
                  mark name = name <> suffix
                  pairs = zip [name | Binder {name} <- parameters] arguments
                  -- An argument that is a name or a number is written
                  -- wherever the parameter was. Anything else is named.
                  written = Map.fromList [(name, argument) | (name, argument) <- pairs, atom argument]
                  named = [(mark name, argument) | (name, argument) <- pairs, not (atom argument)]
                  bind (name, argument) rest = Cut argument (MuTilde Binder {name, strategy = ByNeed} rest)
              pure $ Cut (Mu (mark covariable) (foldr bind (renew written mark body) named)) result
        -- A definition that is a value written down: forcing it gives it.
        Cut (Global global) (Force result)
          | Just (Thunk covariable (Cut value (Covariable given))) <- find global,
            given == covariable,
            constant value ->
              Cut value <$> consumer result
        Cut left right -> Cut <$> producerx left <*> consumer right
        Primitive primitive arguments result ->
          Primitive primitive <$> traverse producerx arguments <*> consumer result
        LetRec bindings body -> do
          bindings <- for bindings $ \(local, value) -> do
            value <- producerx value
            pure (local, value)
          LetRec bindings <$> statement body
        Abort -> pure Abort
        Halt -> pure Halt
  producerx producer

constant :: Producer -> Bool
constant = \case
  Literal (Literal.Int _) -> True
  Constructor _ [] -> True
  _ -> False

atom :: Producer -> Bool
atom = \case
  Variable _ -> True
  Global _ -> True
  Literal _ -> True
  _ -> False

-- |
-- How much something is made of.
class Size a where
  size :: a -> Int

instance (Size a) => Size [a] where
  size = sum . map size

instance (Size a) => Size (Maybe a) where
  size = maybe 0 size

instance Size Producer where
  size = \case
    Constructor _ fields -> 1 + size fields
    Mu _ body -> 1 + size body
    Function _ _ body -> 1 + size body
    Thunk _ body -> 1 + size body
    _ -> 1

instance Size Consumer where
  size = \case
    Covariable _ -> 1
    MuTilde _ body -> 1 + size body
    Case alternatives otherwise -> 1 + size alternatives + size otherwise
    Apply arguments result -> 1 + size arguments + size result
    Force result -> 1 + size result

instance Size Alternative where
  size (Alternative _ _ body) = size body

instance Size Statement where
  size = \case
    Cut left right -> size left + size right
    Primitive _ arguments result -> 1 + size arguments + size result
    LetRec bindings body -> 1 + size (map snd bindings) + size body
    Abort -> 1
    Halt -> 1

-- |
-- A function's body for one place it is put: some of its parameters
-- written as what they were given, and every other name made new.
class Renew a where
  renew :: Map Text Producer -> (Text -> Text) -> a -> a

instance (Renew a) => Renew [a] where
  renew written mark = map (renew written mark)

instance (Renew a) => Renew (Maybe a) where
  renew written mark = fmap (renew written mark)

instance Renew Binder where
  renew _ mark binder@Binder {name} = binder {name = mark name}

instance Renew Producer where
  renew written mark = \case
    Variable name -> Map.findWithDefault (Variable (mark name)) name written
    Global global -> Global global
    Literal literal -> Literal literal
    Constructor tag fields -> Constructor tag (renew written mark fields)
    Mu covariable body -> Mu (mark covariable) (renew written mark body)
    Function parameters covariable body ->
      Function (renew written mark parameters) (mark covariable) (renew written mark body)
    Thunk covariable body -> Thunk (mark covariable) (renew written mark body)

instance Renew Consumer where
  renew written mark = \case
    Covariable name -> Covariable (mark name)
    MuTilde binder body -> MuTilde (renew written mark binder) (renew written mark body)
    Case alternatives otherwise -> Case (renew written mark alternatives) (renew written mark otherwise)
    Apply arguments result -> Apply (renew written mark arguments) (renew written mark result)
    Force result -> Force (renew written mark result)

instance Renew Alternative where
  renew written mark (Alternative tag binders body) =
    Alternative tag (renew written mark binders) (renew written mark body)

instance Renew Statement where
  renew written mark = \case
    Cut left right -> Cut (renew written mark left) (renew written mark right)
    Primitive primitive arguments result ->
      Primitive primitive (renew written mark arguments) (renew written mark result)
    LetRec bindings body ->
      LetRec [(mark local, renew written mark value) | (local, value) <- bindings] (renew written mark body)
    Abort -> Abort
    Halt -> Halt
