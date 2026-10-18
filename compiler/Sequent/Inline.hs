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
module Sequent.Inline (inline, specialize) where

import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Generate.Global as Global (Global)
import Sequent.Primitive (Primitive (Field, Foreign, IntToInteger, IntegerCastToInt, IntegerTruncateToInt))
import qualified Sequent.Tree as Literal (Literal (..))
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Producer (..),
    Statement (..),
    Strategy (..),
    free,
    globals,
    int63,
    intMaxBound,
    intMinBound,
  )

-- |
-- The most a function can be made of and still be put where it is called.
-- A function that is given evidence that is known can be bigger: what it
-- does with the evidence is then known too, and much of it is gone.
limit, evidenced :: Int
limit = 30
evidenced = 100

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
            -- A parameter that is a value is given one, which is only
            -- written where it was when it is a name or a number.
            and [strategy == ByNeed || atom argument | (Binder {strategy}, argument) <- zip parameters arguments],
            size body <= (if or [strategy == ByValue | Binder {strategy} <- parameters] then evidenced else limit),
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

-- |
-- What forcing an instance that needs nothing gives: its fields, each a
-- definition, or another such instance forced.
data Field
  = Forced Global.Global
  | Atom Producer

-- |
-- The fields of an instance that needs nothing, if a definition is one: a
-- thunk that forces some definitions and constructs a value of them and of
-- other definitions.
dictionary :: (Global.Global -> Maybe Producer) -> Global.Global -> Maybe [Field]
dictionary find global = case find global of
  Just (Thunk covariable body) -> go Map.empty body
    where
      go forced = \case
        Cut (Global other) (Force (MuTilde Binder {name} rest)) -> go (Map.insert name other forced) rest
        Cut (Constructor _ fields) (Covariable given)
          | given == covariable -> traverse (field forced) fields
        _ -> Nothing
      field forced = \case
        Variable name -> Forced <$> Map.lookup name forced
        value | atom value -> Just (Atom value)
        _ -> Nothing
  _ -> Nothing

-- |
-- A field of an instance that is known, read where it is read as what it is.
-- An instance that needs nothing is a definition, which is forced where it
-- is used and then taken apart: a superclass, then a method. Once the
-- instance is known each of those is known, and a method that is known can
-- be called by its name, and put where it is called. An instance that is
-- forced and then not used is not forced: it is only a value.
--
-- An @Integer@ written in the program and made an @Int@ is the @Int@, as
-- a number written for an @Int@ is once the instance of 'Num' is known.
specialize :: (Global.Global -> Maybe Producer) -> Producer -> Producer
specialize find = producer Known {instances = Map.empty, integers = Map.empty, widened = Map.empty}
  where
    producer known = \case
      Constructor tag fields -> Constructor tag (map (producer known) fields)
      Mu covariable body -> Mu covariable (statement known body)
      Function parameters covariable body -> Function parameters covariable (statement known body)
      Thunk covariable body -> Thunk covariable (statement known body)
      other -> other
    consumer known = \case
      MuTilde binder body -> MuTilde binder (statement known body)
      Case alternatives otherwise ->
        Case
          [Alternative tag binders (statement known body) | Alternative tag binders body <- alternatives]
          (statement known <$> otherwise)
      Apply arguments result -> Apply (map (producer known) arguments) (consumer known result)
      Force result -> Force (consumer known result)
      other -> other
    statement known = \case
      Cut (Global global) (Force (MuTilde binder@Binder {name} body))
        | Just fields <- dictionary find global ->
            let body' = statement known {instances = Map.insert name fields (instances known)} body
             in if Set.member name (free body')
                  then Cut (Global global) (Force (MuTilde binder body'))
                  else body'
      Primitive (Field index) [Variable name] result
        | Just fields <- Map.lookup name (instances known),
          (found : _) <- drop index fields ->
            statement known $ case found of
              Forced other -> Cut (Global other) (Force result)
              Atom value -> Cut value result
      -- Forcing an Integer that was written gives it.
      Cut (Variable given) (Force (MuTilde binder@Binder {name} body))
        | Just integer <- Map.lookup given (integers known) ->
            let body' = statement known {integers = Map.insert name integer (integers known)} body
             in if Set.member name (free body')
                  then Cut (Variable given) (Force (MuTilde binder body'))
                  else body'
      Cut (Literal (Literal.Integer integer)) (MuTilde binder@Binder {name} body) ->
        let body' = statement known {integers = Map.insert name integer (integers known)} body
         in if Set.member name (free body')
              then Cut (Literal (Literal.Integer integer)) (MuTilde binder body')
              else body'
      -- An Int made an Integer and then a floating-point number is made
      -- one at once. The Integer is not made if nothing else wants it.
      Primitive IntToInteger [argument@(Variable _)] (MuTilde binder@Binder {name} body) ->
        let body' = statement known {widened = Map.insert name argument (widened known)} body
         in if Set.member name (free body')
              then Primitive IntToInteger [argument] (MuTilde binder body')
              else body'
      Primitive (Foreign outside 2) [precision, Variable given] result
        | outside == Text.pack "float_from_integer",
          Just int <- Map.lookup given (widened known) ->
            Primitive (Foreign (Text.pack "float_from_int") 2) [precision, int] (consumer known result)
      Primitive IntegerTruncateToInt [argument] result
        | Just integer <- written argument ->
            Cut (Literal (Literal.Int (int63 integer))) (consumer known result)
      Primitive IntegerCastToInt [argument] result
        | Just integer <- written argument,
          integer >= toInteger intMinBound && integer <= toInteger intMaxBound ->
            Cut (Literal (Literal.Int (fromInteger integer))) (consumer known result)
      Cut left right -> Cut (producer known left) (consumer known right)
      Primitive primitive arguments result ->
        Primitive primitive (map (producer known) arguments) (consumer known result)
      LetRec bindings body ->
        LetRec [(local, producer known value) | (local, value) <- bindings] (statement known body)
      other -> other
      where
        written = \case
          Literal (Literal.Integer integer) -> Just integer
          Variable name -> Map.lookup name (integers known)
          _ -> Nothing

-- |
-- What is known where something is: the names that are instances that
-- are known, and the names that are @Integer@s written in the program.
data Known = Known
  { instances :: Map Text [Field],
    integers :: Map Text Integer,
    -- | The names that are Ints made Integers, and the Ints.
    widened :: Map Text Producer
  }

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
