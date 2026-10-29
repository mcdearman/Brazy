{-# LANGUAGE_HAZY UnorderedRecords #-}
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
module Sequent.Inline (inline, specialize, copy, isCopy, uses) where

import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Generate.Global as Global (Global (..))
import qualified Sequent.Runtime as Runtime (helperGlobal, showGlobal)
import Sequent.Simplify (simplify)
import qualified Sequent.Uncurry as Uncurry (direct)
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
-- The time over that what lists are made and gone through with is put where
-- it is called. Before it, a list that is made by 'build' and gone through
-- by 'listFoldr' at once can be seen as that.
lastly :: Int
lastly = 4

-- |
-- What lists are made and gone through with, in "Hazy.Helper", as fusion
-- looks for them: a list made by @build g@ or @augment g rest@, gone through
-- by @listFoldr k z@.
build, augment, listFoldr :: Global.Global
build = Runtime.helperGlobal "build"
augment = Uncurry.direct (Runtime.helperGlobal "augment")
listFoldr = Uncurry.direct (Runtime.helperGlobal "listFoldr")

-- |
-- Whether a function makes a list by 'build' or 'augment'. Such a function
-- is put where it is called though it is bigger, so that what goes through
-- the list there can be seen to.
making :: Statement -> Bool
making body = any (`Set.member` globals body) [build, augment]

-- |
-- Whether a statement goes through a list from the left by 'listFoldr': it
-- gives what going through it gives back the value to start from. Such a
-- function, as 'foldl'' and 'length' of a list are, is put where it is
-- called though it is bigger, so that a list made there and gone through
-- there can be seen to; it is bigger than most for the function of its own
-- it has for each element. Without that, a list given to 'sum' was made.
foldsLeft :: Statement -> Bool
foldsLeft = statement
  where
    folders = [listFoldr, Runtime.helperGlobal "listFoldr"]
    statement = \case
      Cut (Global global) consumer
        | global `elem` folders,
          length (applied consumer) > 3 ->
            True
      Cut left right -> producer left || consumer right
      Primitive _ arguments result -> any producer arguments || consumer result
      LetRec bindings inner -> any (producer . snd) bindings || statement inner
      Abort -> False
      Halt -> False
    applied = \case
      Apply arguments result -> arguments ++ applied result
      _ -> []
    producer = \case
      Constructor _ fields -> any producer fields
      Mu _ inner -> statement inner
      Function _ _ inner -> statement inner
      Thunk _ inner -> statement inner
      _ -> False
    consumer = \case
      MuTilde _ inner -> statement inner
      Case alternatives otherwise -> or [statement inner | Alternative _ _ inner <- alternatives] || any statement otherwise
      Apply arguments result -> any producer arguments || consumer result
      Force result -> consumer result
      Covariable _ -> False

fusing :: [Global.Global]
fusing = [build, augment, listFoldr, Runtime.helperGlobal "augment", Runtime.helperGlobal "listFoldr"]

-- |
-- A list as it is made: by a function given what puts an element before
-- the rest and what comes after the last, and what comes after if there is
-- more.
data Made
  = Built Producer
  | Augmented Producer Producer

-- |
-- The most a function can be made of and still be put where it is called.
-- A function that is given evidence that is known can be bigger: what it
-- does with the evidence is then known too, and much of it is gone. So can
-- one that goes through a list from the left ('foldsLeft'), though not as
-- much: allowed as much, the compiler's LLVM was 3% longer and nothing ran
-- faster.
limit, folding, evidenced, alone :: Int
limit = 30
folding = 50
evidenced = 100
-- | A function defined inside another that is called in one place only
-- is put there however big it is, up to this, if the call is in the
-- definition's own body, or if the function takes apart what it is given
-- with 'Field', as one given instances does to take their methods out
-- ('selects'). A function written in a definition may be copied into a
-- loop, which is done only when it is small: in @nbody@ putting what the
-- function given to @foldl'@ called into it stopped that, where @queens@'
-- @safe@, given the instances of @Int@, has to be put there to know them.
alone = 400

-- |
-- Whether a definition was defined inside another, and so is only called
-- from the module it is in.
inner :: Global.Global -> Bool
inner Global.Global {name} = Text.any (== '%') name

-- |
-- Whether a function's body takes apart one of its parameters with 'Field',
-- as a function given an instance does to take a method out of it.
selects :: [Binder] -> Statement -> Bool
selects parameters = statement
  where
    names = Set.fromList [name | Binder {name} <- parameters]
    statement = \case
      Primitive (Field _) [Variable given] _ | Set.member given names -> True
      Cut left right -> producer left || consumer right
      Primitive _ arguments result -> any producer arguments || consumer result
      LetRec bindings body -> any (producer . snd) bindings || statement body
      Abort -> False
      Halt -> False
    producer = \case
      Constructor _ fields -> any producer fields
      Mu _ body -> statement body
      Function _ _ body -> statement body
      Thunk _ body -> statement body
      _ -> False
    consumer = \case
      MuTilde _ body -> statement body
      Case alternatives otherwise -> or [statement body | Alternative _ _ body <- alternatives] || any statement otherwise
      Apply arguments result -> any producer arguments || consumer result
      Force result -> consumer result
      Covariable _ -> False

-- |
-- How many times each definition is named in some.
uses :: [Producer] -> Map Global.Global Int
uses = foldr producer Map.empty
  where
    producer value counts = case value of
      Global global -> Map.insertWith (+) global 1 counts
      Constructor _ fields -> foldr producer counts fields
      Mu _ body -> statement body counts
      Function _ _ body -> statement body counts
      Thunk _ body -> statement body counts
      _ -> counts
    consumer value counts = case value of
      MuTilde _ body -> statement body counts
      Case alternatives otherwise ->
        foldr (\(Alternative _ _ body) -> statement body) (maybe counts (`statement` counts) otherwise) alternatives
      Apply arguments result -> foldr producer (consumer result counts) arguments
      Force result -> consumer result counts
      Covariable _ -> counts
    statement value counts = case value of
      Cut left right -> producer left (consumer right counts)
      Primitive _ arguments result -> foldr producer (consumer result counts) arguments
      LetRec bindings body -> foldr (producer . snd) (statement body counts) bindings
      Abort -> counts
      Halt -> counts

-- |
-- A definition with the small functions it calls put in. It is given which
-- time over this is, what each definition was the time before, and the name
-- of the definition.
inline :: Int -> (Global.Global -> Maybe Producer) -> (Global.Global -> Bool) -> Global.Global -> Producer -> Producer
inline time find once self producer = runST (inline' time find once self producer)

inline' :: Int -> (Global.Global -> Maybe Producer) -> (Global.Global -> Bool) -> Global.Global -> Producer -> ST s Producer
inline' time find once self producer = do
  sites <- newSTRef (0 :: Int)
  let -- How many functions in a statement is: one in the definition's own
      -- body, more in a function written in it.
      producerx depth = \case
        Constructor tag fields -> Constructor tag <$> traverse (producerx depth) fields
        Mu covariable body -> Mu covariable <$> statement depth body
        Function parameters covariable body -> Function parameters covariable <$> statement (depth + 1) body
        Thunk covariable body -> Thunk covariable <$> statement depth body
        other -> pure other
      consumer depth = \case
        MuTilde binder body -> MuTilde binder <$> statement depth body
        Case alternatives otherwise -> do
          alternatives <- for alternatives $ \(Alternative tag binders body) ->
            Alternative tag binders <$> statement depth body
          otherwise <- traverse (statement depth) otherwise
          pure $ Case alternatives otherwise
        Apply arguments result -> Apply <$> traverse (producerx depth) arguments <*> consumer depth result
        Force result -> Force <$> consumer depth result
        other -> pure other
      statement depth = \case
        Cut (Global global) (Apply arguments result)
          | global /= self,
            time >= lastly || global `notElem` fusing,
            Just (Function parameters covariable body) <- find global,
            length parameters == length arguments,
            -- A parameter that is a value is given one, which is only
            -- written where it was when it is a name or a number.
            and [strategy == ByNeed || atom argument | (Binder {strategy}, argument) <- zip parameters arguments],
            size body <= (if or [strategy == ByValue | Binder {strategy} <- parameters] || making body then evidenced else if foldsLeft body then folding else limit)
              || ((depth <= 1 || selects parameters body) && inner global && once global && size body <= alone),
            Set.notMember global (globals body) -> do
              arguments <- traverse (producerx depth) arguments
              result <- consumer depth result
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
              Cut value <$> consumer depth result
        Cut left right -> Cut <$> producerx depth left <*> consumer depth right
        Primitive primitive arguments result ->
          Primitive primitive <$> traverse (producerx depth) arguments <*> consumer depth result
        LetRec bindings body -> do
          bindings <- for bindings $ \(local, value) -> do
            value <- producerx depth value
            pure (local, value)
          LetRec bindings <$> statement depth body
        Abort -> pure Abort
        Halt -> pure Halt
  producerx (0 :: Int) producer

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
specialize find = producer Known {instances = Map.empty, integers = Map.empty, widened = Map.empty, lists = Map.empty, cells = Map.empty}
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
      -- A list that is to be made by 'build' or 'augment'. If all that is
      -- done with it is go through it by 'listFoldr', it is never made;
      -- if more is done with it, it is made as it was.
      Cut made@(Thunk covariable inner) (MuTilde binder@Binder {name} body)
        | Just list <- madeIn covariable inner ->
            let fused = statement known {lists = Map.insert name list (lists known)} body
             in if Set.member name (free fused)
                  then Cut (producer known made) (MuTilde binder (statement known body))
                  else fused
      -- The same, made at once rather than when it is first wanted.
      Cut (Global maker) (Apply arguments (MuTilde binder@Binder {name} body))
        | Just list <- madeBy maker arguments ->
            let fused = statement known {lists = Map.insert name list (lists known)} body
             in if Set.member name (free fused)
                  then Cut (Global maker) (Apply (map (producer known) arguments) (MuTilde binder (statement known body)))
                  else fused
      -- Going through a list as it is made is giving what goes through it
      -- to what makes it.
      Cut (Global folder) (Apply [step, start, Variable name] result)
        | folder == listFoldr,
          Just list <- Map.lookup name (lists known) ->
            statement known $ case list of
              Built make -> Cut make (Apply [step] (Apply [start] result))
              Augmented make rest ->
                let stepName = name <> Text.pack "^step"
                    restName = name <> Text.pack "^rest"
                    restResult = name <> Text.pack "^after"
                    after =
                      Thunk restResult $
                        Cut (Global listFoldr) (Apply [Variable stepName, start, rest] (Covariable restResult))
                 in Cut step $
                      MuTilde Binder {name = stepName, strategy = ByNeed} $
                        Cut after $
                          MuTilde Binder {name = restName, strategy = ByNeed} $
                            Cut make (Apply [Variable stepName] (Apply [Variable restName] result))
      -- A list that is made by a name is gone through as it is made: a
      -- comprehension names the list of one it makes of each element.
      Cut (Global folder) (Apply [step, start, Variable list] result)
        | folder == listFoldr,
          Just cell <- Map.lookup list (cells known) ->
            statement known (Cut (Global folder) (Apply [step, start, cell] result))
      Cut cell@(Constructor tag fields) (MuTilde binder@Binder {name} body)
        | tag <= 1,
          all (\field -> atom field || constant field) fields ->
            Cut cell (MuTilde binder (statement known {cells = Map.insert name cell (cells known)} body))
      -- Going through no list at all is what comes after the last.
      Cut (Global folder) (Apply [_, start, Constructor 0 []] result)
        | folder == listFoldr ->
            Cut (producer known start) (Force (consumer known result))
      -- Going through a list whose first element is there is giving that
      -- element to what goes through, with going through the rest: a list
      -- of one, as a comprehension makes of each element, is never made.
      Cut (Global folder) (Apply [step, start, Constructor 1 [first@(Variable element), rest]] result)
        | folder == listFoldr ->
            let name = case step of
                  Variable named -> named <> Text.pack "^" <> element
                  _ -> element
                stepName = name <> Text.pack "^first"
                restName = name <> Text.pack "^more"
                restResult = name <> Text.pack "^ahead"
                after =
                  Thunk restResult $
                    Cut (Global listFoldr) (Apply [Variable stepName, start, rest] (Covariable restResult))
             in statement known $
                  Cut step $
                    MuTilde Binder {name = stepName, strategy = ByNeed} $
                      Cut after $
                        MuTilde Binder {name = restName, strategy = ByNeed} $
                          Cut (Variable stepName) (Force (Apply [first] (Apply [Variable restName] result)))
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
-- The most a function that calls itself can be made of and still be copied
-- for what it is given.
copied :: Int
copied = 200

-- |
-- The most a function that is given can be made of and still be written
-- into a copy, once for each place the copy uses it.
written' :: Int
written' = 60

-- |
-- What a function that calls itself is copied for, at one of its
-- parameters.
data Fixed
  = -- | An instance that needs nothing, forced where the copy starts.
    Instance Global.Global
  | -- | A function written where the call is, by the name it has there.
    Lambda Text Producer

-- |
-- What is known where a call is: the names that are instances that are
-- known, the names of functions written there, and every name of a value
-- there, with how it was named.
data Around = Around
  { named :: Map Text Global.Global,
    lambdas :: Map Text Producer,
    bound :: Map Text Strategy
  }

-- |
-- A copy of a function as it is made: as it was, or taking one argument
-- more, if every way through it gave back a function of one.
data Copy
  = Copied Producer
  | Expanded Producer

-- |
-- The definitions of one family, with a copy of each function that calls
-- itself for what it is given where that is known, and has to be passed on
-- as it is wherever the function calls itself. A call that gives that is
-- sent to the copy, which takes the rest.
--
-- What is known is an instance that needs nothing, or a function written
-- where the call is. In a copy for an instance, the instance is forced
-- where it starts, so that its methods are known there. In a copy for a
-- function, the function is written wherever the copy uses it. Another
-- function written there that it refers to is written in too, and anything
-- else it refers to is given to the copy as well: going through a list
-- with 'listFoldr', or making the @Int@s of a range, given what to do with
-- each, is then a loop that does it.
--
-- A copy whose every way through gives back a function of one argument, as
-- one going through a list from the left with 'listFoldr' does, is made to
-- take that argument as well, and what it would have given that function
-- is done at once: a loop with an accumulator, as GHC makes of @foldl@. A
-- call that gives the function that argument at once gives it to the copy;
-- any other is given a function that does. A copy is only ever called where
-- it was made and in itself, so nothing that the copy works out before the
-- function it gives back is worked out more often than it was. A copy for a
-- function is only made if it comes out so: one that does not still makes
-- what the function would have made, a thunk for the rest of a list at
-- each element, and those made the compiler's LLVM a fifth longer and it
-- ran no faster.
--
-- A function at the top that is given is not copied for. There are a great
-- many of those, a parser for each thing that is parsed, and with their
-- copies the compiler checked itself more slowly than without. Nor is a
-- function that only builds a value, as an instance that needs others is:
-- copied whole, those made the compiler's LLVM 15% longer and no faster.
-- Copies for instances are made all but the last time over, so that what
-- the instance's methods are can be seen to in them; copies for functions
-- are made every time over, and come out simplified.
--
-- A copy is a definition at the top of the module it is called from. One
-- for an instance is named after the function and the instance, so a call
-- of the same function with the same anywhere in the module is sent to the
-- same copy; one for a function is named after the definition the call is
-- in as well. It is given back with the family, to be put with the
-- module's definitions; it is made wherever it is used, as a definition
-- that is generated is.
copy :: Int -> (Global.Global -> Maybe Producer) -> Global.Global -> Map Global.Global Producer -> (Map Global.Global Producer, Map Global.Global Producer)
copy time find top members = runST $ do
  made <- newSTRef Map.empty
  sites <- newSTRef (0 :: Int)
  let bind binders around = around {bound = foldr (\Binder {name, strategy} -> Map.insert name strategy) (bound around) binders}
      producerx place around = \case
        Constructor tag fields -> Constructor tag <$> traverse (producerx place around) fields
        Mu covariable body -> Mu covariable <$> statement place around body
        Function parameters covariable body -> Function parameters covariable <$> statement place (bind parameters around) body
        Thunk covariable body -> Thunk covariable <$> statement place around body
        other -> pure other
      consumer place around = \case
        MuTilde binder body -> MuTilde binder <$> statement place (bind [binder] around) body
        Case alternatives otherwise -> do
          alternatives <- for alternatives $ \(Alternative tag binders body) ->
            Alternative tag binders <$> statement place (bind binders around) body
          Case alternatives <$> traverse (statement place around) otherwise
        Apply arguments result -> Apply <$> traverse (producerx place around) arguments <*> consumer place around result
        Force result -> Force <$> consumer place around result
        other -> pure other
      statement place around = \case
        Cut (Global function) (Apply arguments result)
          | Just (Function parameters covariable body) <- find function,
            length parameters == length arguments,
            not (null (callsOf function body)),
            not (builds covariable body),
            size body <= copied,
            passed <- static function parameters body,
            fixed'@(_ : _) <- [(index, given) | (index, argument) <- zip [0 ..] arguments, index `elem` passed, Just given <- [fixedOf around argument]],
            -- A copy for a function written at the call is only made if it
            -- takes the argument more. If it would not, the function is
            -- copied for the instances it is given alone, if any.
            Just (fixed, made') <-
              let attempt chosen = copyOf function (copyName top (fst place) function chosen) parameters covariable body chosen (capturedOf around chosen)
                  instances' = [(index, given) | (index, given@Instance {}) <- fixed']
               in case attempt fixed' of
                    made'@(Expanded _) -> Just (fixed', made')
                    made'@(Copied _) | length instances' == length fixed' -> Just (fixed', made')
                    _ | not (null instances') -> Just (instances', attempt instances')
                      | otherwise -> Nothing -> do
              let (self, fresh) = place
                  name = copyName top self function fixed
                  indexes = map fst fixed
                  (captured, _) = capturedOf around fixed
                  given = [argument | (index, argument) <- zip [0 :: Int ..] arguments, index `notElem` indexes] ++ [Variable local | (local, _) <- captured]
              given <- traverse (producerx place around) given
              result <- consumer place around result
              case made' of
                Copied producer -> do
                  modifySTRef' made (Map.insert name producer)
                  pure $ Cut (Global name) (Apply given result)
                Expanded producer -> do
                  let expanded = expandedName name
                  modifySTRef' made (Map.insert expanded producer)
                  site <- readSTRef sites
                  modifySTRef' sites (+ 1)
                  pure $ callExpanded (fresh <> Text.pack ('e' : show site)) expanded given result
        Cut (Global global) (Force (MuTilde binder@Binder {name} body))
          | Just (_ : _) <- dictionary find global ->
              Cut (Global global) . Force . MuTilde binder
                <$> statement place (bind [binder] around {named = Map.insert name global (named around)}) body
        Cut lambda@Function {} (MuTilde binder@Binder {name} body) -> do
          lambda <- producerx place around lambda
          Cut lambda . MuTilde binder
            <$> statement place (bind [binder] around {lambdas = Map.insert name lambda (lambdas around)}) body
        Cut left right -> Cut <$> producerx place around left <*> consumer place around right
        Primitive primitive arguments result ->
          Primitive primitive <$> traverse (producerx place around) arguments <*> consumer place around result
        LetRec bindings body -> do
          let around' = bind [Binder {name = local, strategy = ByNeed} | (local, _) <- bindings] around
          bindings <- for bindings $ \(local, value) -> (,) local <$> producerx place around' value
          LetRec bindings <$> statement place around' body
        Abort -> pure Abort
        Halt -> pure Halt
      -- What an argument is, if it is known.
      fixedOf around = \case
        Global global
          | time < 4,
            Just (_ : _) <- dictionary find global ->
              Just (Instance global)
        Variable name
          | time < 4,
            Just global <- Map.lookup name (named around) ->
              Just (Instance global)
          | Just lambda <- Map.lookup name (lambdas around),
            size lambda <= written',
            all (`Map.member` bound around) (Set.toList (free lambda)) ->
              Just (Lambda name lambda)
        _ -> Nothing
      empty' = Around {named = Map.empty, lambdas = Map.empty, bound = Map.empty}
  -- Names made in a definition end in what no name in it has.
  rewritten <- Map.traverseWithKey (\self producer -> producerx (self, freshSuffix (Cut producer (Covariable (Text.pack "")))) empty' producer) members
  copies <- readSTRef made
  pure (rewritten, copies)

-- |
-- What the functions a copy is made for refer to. A function written where
-- the call is is written into the copy too, and what it refers to in turn
-- is looked at. Anything else is a parameter of the copy of its own: the
-- name it has where the call is, and how it was named there.
capturedOf :: Around -> [(Int, Fixed)] -> ([(Text, Strategy)], [(Text, Producer)])
capturedOf Around {bound, lambdas} fixed =
  go Set.empty [] [] (Set.toList (Set.unions [free lambda | (_, Lambda _ lambda) <- fixed]))
  where
    go seen captured written = \case
      [] -> (reverse captured, reverse written)
      local : rest
        | Set.member local seen -> go seen captured written rest
        | Just lambda <- Map.lookup local lambdas,
          size lambda <= written' ->
            go (Set.insert local seen) captured ((local, lambda) : written) (Set.toList (free lambda) ++ rest)
        | otherwise ->
            go (Set.insert local seen) ((local, Map.findWithDefault ByNeed local bound) : captured) written rest

-- |
-- Whether a definition is a copy of a function made by 'copy'. No name a
-- program gives is like it.
isCopy :: Global.Global -> Bool
isCopy Global.Global {name} = Text.isPrefixOf (Text.pack "copy ") name

-- |
-- The parameters a function that calls itself passes on as they are,
-- wherever it calls itself with every argument.
static :: Global.Global -> [Binder] -> Statement -> [Int]
static function parameters body =
  [ index
    | (index, Binder {name}) <- zip [0 ..] parameters,
      and [passes index name arguments | arguments <- callsOf function body]
  ]
  where
    passes index name arguments = case drop index arguments of
      Variable given : _ -> given == name
      _ -> False

-- |
-- Whether a function does nothing but build a value, after naming what goes
-- in it, as an instance that needs others does. Its methods may call it,
-- but it is not gone round, and a copy of it would make nothing faster.
builds :: Text -> Statement -> Bool
builds covariable = \case
  Cut Constructor {} (Covariable given) -> given == covariable
  Cut _ (MuTilde _ rest) -> builds covariable rest
  Cut _ (Force (MuTilde _ rest)) -> builds covariable rest
  LetRec _ rest -> builds covariable rest
  _ -> False

-- |
-- The arguments of each call of a definition that a statement makes.
callsOf :: Global.Global -> Statement -> [[Producer]]
callsOf function = statement
  where
    producer = \case
      Constructor _ fields -> concatMap producer fields
      Mu _ body -> statement body
      Function _ _ body -> statement body
      Thunk _ body -> statement body
      _ -> []
    consumer = \case
      MuTilde _ body -> statement body
      Case alternatives otherwise -> concat [statement body | Alternative _ _ body <- alternatives] ++ foldMap statement otherwise
      Apply arguments result -> concatMap producer arguments ++ consumer result
      Force result -> consumer result
      Covariable _ -> []
    statement = \case
      Cut (Global global) (Apply arguments result)
        | global == function -> arguments : concatMap producer arguments ++ consumer result
      Cut left right -> producer left ++ consumer right
      Primitive _ arguments result -> concatMap producer arguments ++ consumer result
      LetRec bindings body -> concatMap (producer . snd) bindings ++ statement body
      Abort -> []
      Halt -> []

-- |
-- What a copy of a function is called, in the module of a definition, given
-- what it is given and, for a function written where it is called, the
-- definition the call is in.
copyName :: Global.Global -> Global.Global -> Global.Global -> [(Int, Fixed)] -> Global.Global
copyName top self function fixed = top {Global.name = Text.pack described}
  where
    described = "copy " ++ Runtime.showGlobal function ++ concatMap fixedly fixed
    fixedly = \case
      (index, Instance given) -> " " ++ show index ++ "=" ++ Runtime.showGlobal given
      (index, Lambda local _) -> " " ++ show index ++ "=" ++ Runtime.showGlobal self ++ " " ++ Text.unpack local

-- |
-- What a copy that takes one argument more is called.
expandedName :: Global.Global -> Global.Global
expandedName global@Global.Global {name} = global {Global.name = name <> Text.pack " expanded"}

-- |
-- A copy of a function that calls itself, for what some of its parameters
-- are given. An instance is forced where the copy starts; a function is
-- written wherever the copy uses it, with the functions it refers to that
-- are written in too, and what else it refers to a parameter more. Where it
-- calls itself, it calls the copy.
copyOf :: Global.Global -> Global.Global -> [Binder] -> Text -> Statement -> [(Int, Fixed)] -> ([(Text, Strategy)], [(Text, Producer)]) -> Copy
copyOf function name parameters covariable body fixed (given, also)
  | not (null lambdas'),
    Just extra <- expandable covariable name simplified =
      let parameter = Binder {name = extra <> suffix <> Text.pack "x", strategy = ByNeed}
       in Expanded $
            Function (rest ++ [parameter]) covariable $
              simplify (expand name (expandedName name) suffix covariable parameter simplified)
  | otherwise = Copied (Function rest covariable simplified)
  where
    -- Every name the copy makes has this in it, and none of the function's
    -- names, its parameters among them, has.
    suffix = freshSuffix (Cut (Function parameters covariable body) (Covariable Text.empty))
    captured = [(local, Binder {name = local <> suffix <> Text.pack "g", strategy}) | (local, strategy) <- given]
    indexes = map fst fixed
    rest = [parameter | (index, parameter) <- zip [0 ..] parameters, index `notElem` indexes] ++ map snd captured
    -- What else is written into the copy is first a name that is not
    -- anything's, and is then written as itself wherever that is.
    placeholders = [(local, local <> suffix <> Text.pack "l", lambda) | (local, lambda) <- also]
    renamed =
      Map.fromList $
        [(local, Variable given') | (local, Binder {name = given'}) <- captured]
          ++ [(local, Variable placeholder) | (local, placeholder, _) <- placeholders]
    lambdas' = [(parameterName index, lambda) | (index, Lambda _ lambda) <- fixed] ++ [(placeholder, lambda) | (_, placeholder, lambda) <- placeholders]
    parameterName index = let Binder {name = local} = parameters !! index in local
    force (index, given) inner = Cut (Global given) (Force (MuTilde (parameters !! index) inner))
    keep arguments =
      [argument | (index, argument) <- zip [0 :: Int ..] arguments, index `notElem` indexes]
        ++ [Variable given | (_, Binder {name = given}) <- captured]
    simplified =
      simplify $
        foldr force (writeLambdas suffix renamed lambdas' (redirectCalls function name (length parameters) keep body)) [(index, given) | (index, Instance given) <- fixed]

-- |
-- A statement with the calls of a function that give every argument sent to
-- another, given what to give it instead of the arguments.
redirectCalls :: Global.Global -> Global.Global -> Int -> ([Producer] -> [Producer]) -> Statement -> Statement
redirectCalls function name count keep = statement
  where
    statement = \case
      Cut (Global global) (Apply arguments result)
        | global == function,
          length arguments == count ->
            Cut (Global name) (Apply (map producer (keep arguments)) (consumer result))
      Cut left right -> Cut (producer left) (consumer right)
      Primitive primitive arguments result -> Primitive primitive (map producer arguments) (consumer result)
      LetRec bindings inner -> LetRec [(local, producer value) | (local, value) <- bindings] (statement inner)
      Abort -> Abort
      Halt -> Halt
    producer = \case
      Constructor tag fields -> Constructor tag (map producer fields)
      Mu local inner -> Mu local (statement inner)
      Function parameters result inner -> Function parameters result (statement inner)
      Thunk local inner -> Thunk local (statement inner)
      other -> other
    consumer = \case
      MuTilde binder inner -> MuTilde binder (statement inner)
      Case alternatives otherwise ->
        Case [Alternative tag binders (statement inner) | Alternative tag binders inner <- alternatives] (statement <$> otherwise)
      Apply arguments result -> Apply (map producer arguments) (consumer result)
      Force result -> Force (consumer result)
      other -> other

-- |
-- A statement with each of some names written as a function, a new copy of
-- it at each place, its names made new and what it refers to renamed.
writeLambdas :: Text -> Map Text Producer -> [(Text, Producer)] -> Statement -> Statement
writeLambdas suffix renamed lambdas' body = runST $ do
  places <- newSTRef (0 :: Int)
  let producer = \case
        Variable local
          | Just lambda <- lookup local lambdas' -> do
              place <- readSTRef places
              modifySTRef' places (+ 1)
              let mark name = name <> suffix <> Text.pack ('.' : show place)
              -- What it refers to that is written in as well is written
              -- in it.
              producer (renew renamed mark lambda)
        Constructor tag fields -> Constructor tag <$> traverse producer fields
        Mu local inner -> Mu local <$> statement inner
        Function parameters result inner -> Function parameters result <$> statement inner
        Thunk local inner -> Thunk local <$> statement inner
        other -> pure other
      consumer = \case
        MuTilde binder inner -> MuTilde binder <$> statement inner
        Case alternatives otherwise -> do
          alternatives <- for alternatives $ \(Alternative tag binders inner) -> Alternative tag binders <$> statement inner
          Case alternatives <$> traverse statement otherwise
        Apply arguments result -> Apply <$> traverse producer arguments <*> consumer result
        Force result -> Force <$> consumer result
        other -> pure other
      statement = \case
        Cut left right -> Cut <$> producer left <*> consumer right
        Primitive primitive arguments result -> Primitive primitive <$> traverse producer arguments <*> consumer result
        LetRec bindings inner -> do
          bindings <- for bindings $ \(local, value) -> (,) local <$> producer value
          LetRec bindings <$> statement inner
        Abort -> pure Abort
        Halt -> pure Halt
  statement body

-- |
-- If every way through a copy gives back a function of one argument, or is
-- the copy calling itself for what it gives back, the name of that
-- argument in one of those functions, for the copy that takes it.
expandable :: Text -> Global.Global -> Statement -> Maybe Text
expandable covariable self body
  | uses == length exits,
    (extra : _) <- [name | Just name <- exits] =
      Just extra
  | otherwise = Nothing
  where
    uses = mentions covariable body
    exits = exitsOf body
    exitsOf = \case
      Cut (Function [Binder {name}] _ _) (Covariable given) | given == covariable -> [Just name]
      Cut (Global global) (Apply _ (Covariable given)) | global == self, given == covariable -> [Nothing]
      Cut left right -> producerExits left ++ consumerExits right
      Primitive _ arguments result -> concatMap producerExits arguments ++ consumerExits result
      LetRec bindings inner -> concatMap (producerExits . snd) bindings ++ exitsOf inner
      Abort -> []
      Halt -> []
    producerExits = \case
      Constructor _ fields -> concatMap producerExits fields
      Mu _ inner -> exitsOf inner
      Function _ _ inner -> exitsOf inner
      Thunk _ inner -> exitsOf inner
      _ -> []
    consumerExits = \case
      MuTilde _ inner -> exitsOf inner
      Case alternatives otherwise -> concat [exitsOf inner | Alternative _ _ inner <- alternatives] ++ foldMap exitsOf otherwise
      Apply arguments result -> concatMap producerExits arguments ++ consumerExits result
      Force result -> consumerExits result
      Covariable _ -> []

-- |
-- How many times a covariable is mentioned.
mentions :: Text -> Statement -> Int
mentions covariable = statement
  where
    statement = \case
      Cut left right -> producer left + consumer right
      Primitive _ arguments result -> sum (map producer arguments) + consumer result
      LetRec bindings inner -> sum (map (producer . snd) bindings) + statement inner
      Abort -> 0
      Halt -> 0
    producer = \case
      Constructor _ fields -> sum (map producer fields)
      Mu _ inner -> statement inner
      Function _ _ inner -> statement inner
      Thunk _ inner -> statement inner
      _ -> 0
    consumer = \case
      Covariable given -> if given == covariable then 1 else 0
      MuTilde _ inner -> statement inner
      Case alternatives otherwise -> sum [statement inner | Alternative _ _ inner <- alternatives] + maybe 0 statement otherwise
      Apply arguments result -> sum (map producer arguments) + consumer result
      Force result -> consumer result

-- |
-- The body of a copy that takes one argument more: what it gave back is
-- given that argument, and where it calls itself it calls the copy that
-- takes it, giving it that argument where what it gave back would have
-- been given one, and otherwise giving back a function that does.
expand :: Global.Global -> Global.Global -> Text -> Text -> Binder -> Statement -> Statement
expand self expanded suffix covariable Binder {name = extra} body = runST $ do
  places <- newSTRef (0 :: Int)
  let statement = \case
        Cut (Global global) (Apply arguments result)
          | global == self -> do
              arguments <- traverse producer arguments
              result <- consumer result
              place <- readSTRef places
              modifySTRef' places (+ 1)
              pure $ callExpanded (suffix <> Text.pack ('w' : show place)) expanded arguments result
        Cut left right -> Cut <$> producer left <*> consumer right
        Primitive primitive arguments result -> Primitive primitive <$> traverse producer arguments <*> consumer result
        LetRec bindings inner -> do
          bindings <- for bindings $ \(local, value) -> (,) local <$> producer value
          LetRec bindings <$> statement inner
        Abort -> pure Abort
        Halt -> pure Halt
      producer = \case
        Constructor tag fields -> Constructor tag <$> traverse producer fields
        Mu local inner -> Mu local <$> statement inner
        Function parameters result inner -> Function parameters result <$> statement inner
        Thunk local inner -> Thunk local <$> statement inner
        other -> pure other
      consumer = \case
        Covariable given
          | given == covariable -> pure (Apply [Variable extra] (Covariable given))
        MuTilde binder inner -> MuTilde binder <$> statement inner
        Case alternatives otherwise -> do
          alternatives <- for alternatives $ \(Alternative tag binders inner) -> Alternative tag binders <$> statement inner
          Case alternatives <$> traverse statement otherwise
        Apply arguments result -> Apply <$> traverse producer arguments <*> consumer result
        Force result -> Force <$> consumer result
        other -> pure other
  statement body

-- |
-- A call of a copy that takes one argument more, given the arguments of the
-- copy it was made from and what its result went to: that argument, if it
-- went to be given one, and otherwise a function that takes it, named with
-- a suffix of its own.
callExpanded :: Text -> Global.Global -> [Producer] -> Consumer -> Statement
callExpanded suffix expanded arguments = \case
  Apply [argument] result -> Cut (Global expanded) (Apply (arguments ++ [argument]) result)
  result ->
    let parameter = Text.pack "eta" <> suffix
        given = Text.pack "etaresult" <> suffix
     in Cut
          ( Function [Binder {name = parameter, strategy = ByNeed}] given $
              Cut (Global expanded) (Apply (arguments ++ [Variable parameter]) (Covariable given))
          )
          result

-- |
-- A suffix that no name in a statement has in it, for names made new there.
freshSuffix :: Statement -> Text
freshSuffix body = head [suffix | number <- [0 :: Int ..], let suffix = Text.pack ("^c" ++ show number), not (any (suffix `Text.isInfixOf`) names)]
  where
    names = Set.toList (namesIn body)

-- |
-- Every name a statement binds.
namesIn :: Statement -> Set.Set Text
namesIn = statement
  where
    binders = Set.fromList . map (\Binder {name} -> name)
    statement = \case
      Cut left right -> producer left <> consumer right
      Primitive _ arguments result -> foldMap producer arguments <> consumer result
      LetRec bindings inner -> Set.fromList (map fst bindings) <> foldMap (producer . snd) bindings <> statement inner
      Abort -> Set.empty
      Halt -> Set.empty
    producer = \case
      Constructor _ fields -> foldMap producer fields
      Mu local inner -> Set.insert local (statement inner)
      Function parameters result inner -> binders parameters <> Set.insert result (statement inner)
      Thunk local inner -> Set.insert local (statement inner)
      _ -> Set.empty
    consumer = \case
      MuTilde Binder {name} inner -> Set.insert name (statement inner)
      Case alternatives otherwise -> mconcat [binders names <> statement inner | Alternative _ names inner <- alternatives] <> foldMap statement otherwise
      Apply arguments result -> foldMap producer arguments <> consumer result
      Force result -> consumer result
      Covariable _ -> Set.empty

-- |
-- What is known where something is: the names that are instances that
-- are known, and the names that are @Integer@s written in the program.
data Known = Known
  { instances :: Map Text [Field],
    integers :: Map Text Integer,
    -- | The names that are Ints made Integers, and the Ints.
    widened :: Map Text Producer,
    -- | The names of lists that are to be made by 'build' or 'augment'.
    lists :: Map Text Made,
    -- | The names of lists that are made there: an empty one, or a first
    -- element and the rest, each a name or a value written down.
    cells :: Map Text Producer
  }

-- |
-- How the list a thunk gives is made, if all it does is make it by 'build'
-- or 'augment'. What it is given may be named first, as putting a function
-- where it is called does: each such name is what it names, if it is used
-- once and nowhere but there.
madeIn :: Text -> Statement -> Maybe Made
madeIn covariable = go []
  where
    go bound = \case
      Cut value (MuTilde Binder {name} rest)
        | not (atom value),
          all (\(_, earlier) -> Set.notMember name (free earlier)) bound ->
            go ((name, value) : bound) rest
      Cut (Global maker) (Apply arguments (Covariable given))
        | given == covariable,
          and [length [() | Variable used <- arguments, used == name] == 1 | (name, _) <- bound],
          and [Set.null (Set.fromList (map fst bound) `Set.intersection` free value) | (_, value) <- bound] ->
            madeBy maker (map (resolve bound) arguments)
      _ -> Nothing
    resolve bound = \case
      Variable name | Just value <- lookup name bound -> value
      other -> other

-- |
-- How a list is made, if it is made by 'build' or 'augment'.
madeBy :: Global.Global -> [Producer] -> Maybe Made
madeBy maker arguments
  | maker == build, [make] <- arguments = Just (Built make)
  | maker == augment, [make, rest] <- arguments = Just (Augmented make rest)
  | otherwise = Nothing

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
