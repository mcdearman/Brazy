-- |
-- Take the functions that are defined inside a definition, and that use
-- nothing of where they are, out to the top of the program.
--
-- A function in a @let@ or a @where@ is an object that is made each time
-- control passes it. If it mentions no variable of the definition it is in,
-- there is nothing for the object to capture, and it may as well be a
-- definition of its own: it is then made once, and what uses it knows
-- exactly which function it is. Functions that are defined together go
-- together.
--
-- What is taken out is called by the whole path to it: the name of the
-- definition it was in, and then its own, with 'separator' between. So no
-- two can be called the same, whatever they were called where they were.
--
-- A thunk is left where it is. At the top it would be worked out once and
-- then kept for as long as the program runs.
module Sequent.Hoist (hoist) where

import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Generate.Global as Global (Global (..))
import Sequent.Tree
  ( Alternative (..),
    Consumer (..),
    Producer (..),
    Statement (..),
    free,
    separator,
  )

-- |
-- A definition without the functions that could be taken out of it, and
-- those functions, each a definition.
hoist :: Global.Global -> Producer -> (Producer, Map Global.Global Producer)
hoist global producer = runST (hoist' global producer)

hoist' :: Global.Global -> Producer -> ST s (Producer, Map Global.Global Producer)
hoist' Global.Global {path, name} producer = do
  out <- newSTRef Map.empty
  let -- The path so far is the name of the definition and of every binding
      -- that has been gone into.
      producerx prefix = \case
        Constructor tag fields -> Constructor tag <$> traverse (producerx prefix) fields
        Mu covariable body -> Mu covariable <$> statement prefix body
        Function parameters covariable body -> Function parameters covariable <$> statement prefix body
        Thunk covariable body -> Thunk covariable <$> statement prefix body
        other -> pure other
      consumer prefix = \case
        MuTilde binder body -> MuTilde binder <$> statement prefix body
        Case alternatives otherwise -> do
          alternatives <- for alternatives $ \(Alternative tag binders body) ->
            Alternative tag binders <$> statement prefix body
          otherwise <- traverse (statement prefix) otherwise
          pure $ Case alternatives otherwise
        Apply arguments result -> Apply <$> traverse (producerx prefix) arguments <*> consumer prefix result
        Force result -> Force <$> consumer prefix result
        other -> pure other
      statement prefix = \case
        Cut left right -> Cut <$> producerx prefix left <*> consumer prefix right
        Primitive primitive arguments result ->
          Primitive primitive <$> traverse (producerx prefix) arguments <*> consumer prefix result
        LetRec bindings body -> do
          bindings <- for bindings $ \(local, value) -> do
            value <- producerx (Text.snoc prefix separator <> local) value
            pure (local, value)
          body <- statement prefix body
          let moved = closed bindings
              renamed = Map.fromList $ do
                local <- Set.toList moved
                pure (local, Global.Global {path, name = Text.snoc prefix separator <> local})
              (out', kept) = foldr part ([], []) bindings
              part (local, value) (out', kept) = case Map.lookup local renamed of
                Just global -> ((global, rename renamed value) : out', kept)
                Nothing -> (out', (local, rename renamed value) : kept)
          modifySTRef' out (Map.union (Map.fromList out'))
          pure $ case kept of
            [] -> rename renamed body
            _ -> LetRec kept (rename renamed body)
        Abort -> pure Abort
        Halt -> pure Halt
  producer <- producerx name producer
  found <- readSTRef out
  pure (producer, found)

-- |
-- The bindings that are functions and use nothing but each other.
closed :: [(Text, Producer)] -> Set.Set Text
closed bindings = go (Set.fromList [local | (local, Function {}) <- bindings])
  where
    go candidates
      | candidates == still = candidates
      | otherwise = go still
      where
        still = Set.fromList $ do
          (local, value) <- bindings
          if Set.member local candidates && free value `Set.isSubsetOf` candidates
            then [local]
            else []

-- |
-- Put the definitions some variables have become in their place.
class Rename a where
  rename :: Map Text Global.Global -> a -> a

instance (Rename a) => Rename [a] where
  rename renamed = map (rename renamed)

instance (Rename a) => Rename (Maybe a) where
  rename renamed = fmap (rename renamed)

instance Rename Producer where
  rename renamed = \case
    Variable local
      | Just global <- Map.lookup local renamed -> Global global
      | otherwise -> Variable local
    Global global -> Global global
    Literal literal -> Literal literal
    Constructor tag fields -> Constructor tag (rename renamed fields)
    Mu covariable body -> Mu covariable (rename renamed body)
    Function parameters covariable body -> Function parameters covariable (rename renamed body)
    Thunk covariable body -> Thunk covariable (rename renamed body)

instance Rename Consumer where
  rename renamed = \case
    Covariable local -> Covariable local
    MuTilde binder body -> MuTilde binder (rename renamed body)
    Case alternatives otherwise -> Case (rename renamed alternatives) (rename renamed otherwise)
    Apply arguments result -> Apply (rename renamed arguments) (rename renamed result)
    Force result -> Force (rename renamed result)

instance Rename Alternative where
  rename renamed (Alternative tag binders body) = Alternative tag binders (rename renamed body)

instance Rename Statement where
  rename renamed = \case
    Cut left right -> Cut (rename renamed left) (rename renamed right)
    Primitive primitive arguments result -> Primitive primitive (rename renamed arguments) (rename renamed result)
    LetRec bindings body ->
      LetRec [(local, rename renamed value) | (local, value) <- bindings] (rename renamed body)
    Abort -> Abort
    Halt -> Halt
