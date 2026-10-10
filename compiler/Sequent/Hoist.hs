-- |
-- Take the functions that are defined inside a definition out to the top of
-- the program.
--
-- A function in a @let@ or a @where@ is an object that is made each time
-- control passes it, and one that calls itself is an object that holds
-- itself, which counting references never frees. At the top of the program
-- a function is made once, what uses it knows exactly which function it is,
-- and nothing holds anything.
--
-- A function that mentions no variable of the definition it is in goes as
-- it is. One that does is given those variables as arguments, before its
-- own, and every use of it passes them: this is lambda lifting. Functions
-- that are defined together go together, and are given the same variables.
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
    Binder (..),
    Consumer (..),
    Producer (..),
    Statement (..),
    Strategy (..),
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
          let global local = Global.Global {path, name = Text.snoc prefix separator <> local}
              names = Set.fromList (map fst bindings)
              moved = closed bindings
              -- Of the rest, the functions that can be given what they use.
              lifting = Set.fromList $ do
                (local, value@Function {}) <- bindings
                if Set.notMember local moved && Set.null (continuations value)
                  then [local]
                  else []
              -- What they use of where they are: the same for all of them,
              -- since they may call each other.
              given =
                Set.toList $
                  mconcat [free value | (local, value) <- bindings, Set.member local lifting]
                    Set.\\ Set.union moved lifting
              renamed = Map.fromList $ do
                local <- Set.toList names
                if Set.member local moved
                  then [(local, Top (global local))]
                  else
                    if Set.member local lifting
                      then [(local, Lifted (global local) given)]
                      else []
              (out', kept) = foldr part ([], []) bindings
              part (local, value) (out', kept) = case Map.lookup local renamed of
                Just (Top name') -> ((name', rename renamed value) : out', kept)
                Just (Lifted name' _) ->
                  let result = local <> Text.pack "%to"
                      parameters = [Binder {name = variable, strategy = ByNeed} | variable <- given]
                      lifted = Function parameters result (Cut (rename renamed value) (Covariable result))
                   in ((name', lifted) : out', kept)
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
-- The consumers something uses and does not bind. A function that uses one
-- is left where it is: a consumer cannot be given as an argument.
class Continuations a where
  continuations :: a -> Set.Set Text

instance (Continuations a) => Continuations [a] where
  continuations = foldMap continuations

instance (Continuations a) => Continuations (Maybe a) where
  continuations = foldMap continuations

instance Continuations Producer where
  continuations = \case
    Constructor _ fields -> continuations fields
    Mu covariable body -> Set.delete covariable (continuations body)
    Function _ covariable body -> Set.delete covariable (continuations body)
    Thunk covariable body -> Set.delete covariable (continuations body)
    _ -> Set.empty

instance Continuations Consumer where
  continuations = \case
    Covariable name -> Set.singleton name
    MuTilde _ body -> continuations body
    Case alternatives otherwise -> continuations alternatives <> continuations otherwise
    Apply arguments result -> continuations arguments <> continuations result
    Force result -> continuations result

instance Continuations Alternative where
  continuations (Alternative _ _ body) = continuations body

instance Continuations Statement where
  continuations = \case
    Cut left right -> continuations left <> continuations right
    Primitive _ arguments result -> continuations arguments <> continuations result
    LetRec bindings body -> continuations (map snd bindings) <> continuations body
    Abort -> Set.empty
    Halt -> Set.empty

-- |
-- What a variable has become.
data Moved
  = -- | A definition at the top.
    Top Global.Global
  | -- | A definition at the top that is first given some variables.
    Lifted Global.Global [Text]

-- |
-- Put what some variables have become in their place.
class Rename a where
  rename :: Map Text Moved -> a -> a

instance (Rename a) => Rename [a] where
  rename renamed = map (rename renamed)

instance (Rename a) => Rename (Maybe a) where
  rename renamed = fmap (rename renamed)

instance Rename Producer where
  rename renamed = \case
    Variable local -> case Map.lookup local renamed of
      Just (Top global) -> Global global
      -- Used as a value: the function it is once it has its variables.
      Just (Lifted global given) ->
        let result = local <> Text.pack "%as"
         in Mu result (Cut (Global global) (Apply (map Variable given) (Covariable result)))
      Nothing -> Variable local
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
    -- Used where it is called: the variables are passed with the arguments.
    -- It is a function, so it is not forced.
    Cut (Variable local) consumer
      | Just (Lifted global given) <- Map.lookup local renamed ->
          let unforced = case consumer of
                Force inner -> inner
                other -> other
           in Cut (Global global) (Apply (map Variable given) (rename renamed unforced))
    Cut left right -> Cut (rename renamed left) (rename renamed right)
    Primitive primitive arguments result -> Primitive primitive (rename renamed arguments) (rename renamed result)
    LetRec bindings body ->
      LetRec [(local, rename renamed value) | (local, value) <- bindings] (rename renamed body)
    Abort -> Abort
    Halt -> Halt
