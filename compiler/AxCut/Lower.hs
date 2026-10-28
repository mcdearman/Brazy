-- |
-- The sequent calculus to AxCut.
--
-- Two things are done, in turn. Focusing gives a name to everything a
-- statement uses that does not have one, so that every cut has a variable on
-- one side: a constructor's fields and the arguments of a function or a
-- primitive become variables, and a consumer that is passed becomes a
-- covariable. Then the environment is put in order: before each statement
-- the row of values is arranged to be exactly what the statement takes,
-- after what is still wanted later. What is wanted later is worked out from
-- the names the rest of the program mentions.
--
-- A consumer that has to be passed is made into an object with the one
-- method that takes a value. That is the only place a continuation is
-- allocated.
module AxCut.Lower (lower, lowerDefinitions, lowerUnit) where

import qualified AxCut.Tree as AxCut
import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Generate.Global (Global)
import qualified Sequent.Runtime as Runtime (helperGlobal)
import qualified Sequent.Tree as Tree (Literal (Int), free)
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Producer (..),
    Program (..),
    Statement (..),
    Strategy (..),
  )

lower :: Program -> AxCut.Program
lower program = runST (lower' program)

lower' :: Program -> ST s AxCut.Program
lower' Program {definitions, entry} = do
  supply <- newSTRef 0
  definitions <- traverse (definition supply) definitions
  entry <- focus supply entry
  let known = among definitions (const Nothing)
  pure
    AxCut.Program
      { definitions = annotate known <$> definitions,
        entry = annotated known (build (linearly entry) [])
      }

-- |
-- Definitions by themselves. All that is known of a definition that is
-- not one of them is that it is one.
lowerDefinitions :: Map Global Producer -> Map Global AxCut.Object
lowerDefinitions = lowerUnit (const Nothing)

-- |
-- Definitions by themselves, as for a unit that is linked with others,
-- given how many arguments a definition of another unit takes.
lowerUnit :: (Global -> Maybe Int) -> Map Global Producer -> Map Global AxCut.Object
lowerUnit outside definitions = runST $ do
  supply <- newSTRef 0
  definitions <- traverse (definition supply) definitions
  pure $ annotate (among definitions outside) <$> definitions

-- |
-- How many arguments a definition takes at once, if it is a function:
-- going by what it is, if it is one of some definitions, and otherwise by
-- what is said of it.
among :: Map Global AxCut.Object -> (Global -> Maybe Int) -> Global -> Maybe Int
among definitions outside global = case Map.lookup global definitions of
  -- The last parameter is the consumer of the result.
  Just AxCut.Object {kind = AxCut.Closure, parameters} -> Just (length parameters - 1)
  Just _ -> Nothing
  Nothing -> outside global

-- |
-- An object with every definition it names said to take what it takes.
-- This is done last, when the row of values is in order: nothing before it
-- has a use for it.
annotate :: (Global -> Maybe Int) -> AxCut.Object -> AxCut.Object
annotate known AxCut.Object {kind, captured, method, parameters, body, whole} =
  AxCut.Object
    { kind,
      captured,
      method,
      parameters,
      body = annotated known body,
      whole = (\AxCut.Wholly {wholeParameters, wholeBody} -> AxCut.Wholly {wholeParameters, wholeBody = annotated known wholeBody}) <$> whole
    }

annotated :: (Global -> Maybe Int) -> AxCut.Statement -> AxCut.Statement
annotated known = go
  where
    go = \case
      AxCut.Substitute pairs rest -> AxCut.Substitute pairs (go rest)
      AxCut.Let name tag count rest -> AxCut.Let name tag count (go rest)
      AxCut.Switch clauses fallback ->
        AxCut.Switch [AxCut.Clause tag names (go body) | AxCut.Clause tag names body <- clauses] (go <$> fallback)
      AxCut.New name object rest -> AxCut.New name (annotate known object) (go rest)
      AxCut.Invoke method -> AxCut.Invoke method
      AxCut.Force -> AxCut.Force
      AxCut.Literal name literal rest -> AxCut.Literal name literal (go rest)
      AxCut.Global name global _ rest -> AxCut.Global name global (known global) (go rest)
      AxCut.Primitive name primitive rest -> AxCut.Primitive name primitive (go rest)
      AxCut.Rec recursives rest ->
        AxCut.Rec
          [ AxCut.Recursive {name, object = annotate known object, others}
          | AxCut.Recursive {name, object, others} <- recursives
          ]
          (go rest)
      AxCut.Abort -> AxCut.Abort
      AxCut.Halt -> AxCut.Halt

definition :: Supply s -> Producer -> ST s AxCut.Object
definition supply producer = do
  producer <- inside supply producer
  let (captured, object) = objectOf producer
  if Set.null captured
    then pure (object [])
    else error $ "a definition has free variables: " ++ unwords (unpack <$> Set.toList captured)

type Supply s = STRef s Int

-- |
-- A name that the translation from Core cannot have made.
fresh :: Supply s -> String -> ST s Text
fresh supply hint = do
  number <- readSTRef supply
  writeSTRef supply $! number + 1
  pure $ pack $ hint ++ "%" ++ show number

byNeed :: Text -> Binder
byNeed name = Binder {name, strategy = ByNeed}

-- |
-- Give names to what a statement uses, so that it is one of the forms that
-- 'linearly' takes.
focus :: Supply s -> Statement -> ST s Statement
focus supply = \case
  Halt -> pure Halt
  Abort -> pure Abort
  LetRec bindings body -> do
    bindings <- traverse (\(name, value) -> (,) name <$> inside supply value) bindings
    body <- focus supply body
    pure $ LetRec bindings body
  Primitive primitive arguments consumer ->
    naming supply arguments $ \arguments -> case consumer of
      MuTilde binder body -> do
        body <- focus supply body
        pure $ Primitive primitive arguments (MuTilde binder body)
      _ -> do
        value <- fresh supply "v"
        body <- focus supply (Cut (Variable value) consumer)
        pure $ Primitive primitive arguments (MuTilde (byNeed value) body)
  statement@(Cut producer consumer) -> case (producer, consumer) of
    (Mu name body, Covariable _) -> do
      body <- focus supply body
      pure $ Cut (Mu name body) consumer
    (Mu name body, _) -> do
      body <- focus supply body
      consumer <- standing supply consumer
      pure $ Cut (Mu name body) consumer
    (Variable _, Covariable _) -> pure statement
    (Variable _, MuTilde binder body) -> do
      body <- focus supply body
      pure $ Cut producer (MuTilde binder body)
    (Variable _, Case alternatives otherwise) -> do
      alternatives <- traverse alternative alternatives
      otherwise <- traverse (focus supply) otherwise
      pure $ Cut producer (Case alternatives otherwise)
    (Variable _, Apply [argument] result)
      | (more@(_ : _), final) <- spreading (spreadLimit - 1) result ->
          naming supply (argument : more) $ \arguments ->
            passing supply final $ \final ->
              pure $ Cut producer (foldr (\given rest -> Apply [given] rest) final arguments)
    (Variable _, Apply arguments result) ->
      naming supply arguments $ \arguments ->
        passing supply result $ \result ->
          pure $ Cut producer (Apply arguments result)
    (Variable _, Force result) ->
      passing supply result $ \result ->
        pure $ Cut producer (Force result)
    (_, MuTilde binder body) -> do
      body <- focus supply body
      bound supply producer binder body
    _ -> do
      name <- fresh supply "x"
      focus supply $ Cut producer (MuTilde (byNeed name) (Cut (Variable name) consumer))
  where
    alternative (Alternative tag binders body) = Alternative tag binders <$> focus supply body

-- |
-- The most arguments a function that is not known is given together.
spreadLimit :: Int
spreadLimit = 6

-- |
-- The arguments of a consumer that gives a function one argument after
-- another, up to some number of them, and what is given the last result.
spreading :: Int -> Consumer -> ([Producer], Consumer)
spreading limit = \case
  Apply [argument] rest
    | limit > 0 ->
        let (arguments, final) = spreading (limit - 1) rest
         in (argument : arguments, final)
  consumer -> ([], consumer)

-- |
-- Focus what is inside a function or a thunk.
inside :: Supply s -> Producer -> ST s Producer
inside supply = \case
  Function parameters result body -> Function parameters result <$> focus supply body
  Thunk result body -> Thunk result <$> focus supply body
  producer -> pure producer

-- |
-- A consumer that stays where it is, focused inside.
standing :: Supply s -> Consumer -> ST s Consumer
standing supply = \case
  consumer@(Covariable _) -> pure consumer
  MuTilde binder body -> MuTilde binder <$> focus supply body
  consumer -> do
    name <- fresh supply "x"
    MuTilde (byNeed name) <$> focus supply (Cut (Variable name) consumer)

-- |
-- Pass a consumer: if it is not a name, give it one.
passing :: Supply s -> Consumer -> (Consumer -> ST s Statement) -> ST s Statement
passing supply consumer continue = case consumer of
  Covariable _ -> continue consumer
  _ -> do
    name <- fresh supply "k"
    body <- continue (Covariable name)
    consumer <- standing supply consumer
    pure $ Cut (Mu name body) consumer

-- |
-- Name producers, in order, and go on with them as variables.
naming :: Supply s -> [Producer] -> ([Producer] -> ST s Statement) -> ST s Statement
naming supply producers continue = case producers of
  [] -> continue []
  producer@(Variable _) : rest -> naming supply rest (continue . (producer :))
  producer : rest -> do
    name <- fresh supply "x"
    body <- naming supply rest (continue . (Variable name :))
    bound supply producer (byNeed name) body

-- |
-- Something that is made, given a name, and then a statement that is
-- already focused.
bound :: Supply s -> Producer -> Binder -> Statement -> ST s Statement
bound supply producer binder rest = case producer of
  Constructor tag fields ->
    naming supply fields $ \fields ->
      pure $ Cut (Constructor tag fields) (MuTilde binder rest)
  Mu name body -> do
    body <- focus supply body
    pure $ Cut (Mu name body) (MuTilde binder rest)
  _ -> do
    producer <- inside supply producer
    pure $ Cut producer (MuTilde binder rest)

-- |
-- The names of an environment's values. The last value is last.
type Environment = [Text]

-- |
-- A statement that is ready to be put in order: the names it uses and does
-- not bind, and what it is in AxCut given the environment it runs in. The
-- names are worked out once, from the inside out, and say at each statement
-- which values are still wanted.
data Linear = Linear
  { free :: Set Text,
    build :: Environment -> AxCut.Statement
  }

-- |
-- Arrange the environment to be some values, each named with the value it is
-- now, and go on. Nothing is emitted if it is that already.
arrange :: Environment -> [(Text, Text)] -> AxCut.Statement -> AxCut.Statement
arrange environment wanted statement
  | map fst wanted == environment && all (uncurry (==)) wanted = statement
  | otherwise = AxCut.Substitute wanted statement

-- |
-- The values of an environment that are wanted, in the order they are in.
live :: Environment -> Set Text -> Environment
live environment wanted = filter (`Set.member` wanted) environment

same :: [Text] -> [(Text, Text)]
same = map (\name -> (name, name))

-- |
-- A statement that takes some values off the end of the environment and
-- does not go on.
leaving :: [Text] -> AxCut.Statement -> Linear
leaving taken statement =
  Linear
    { free = Set.fromList taken,
      build = \environment -> arrange environment (same taken) statement
    }

-- |
-- Something that takes some values off the end of the environment, puts one
-- there under a name, and goes on.
binding :: [Text] -> Text -> (AxCut.Statement -> AxCut.Statement) -> Linear -> Linear
binding taken name make Linear {free, build} =
  Linear
    { free = Set.fromList taken <> after,
      build = \environment ->
        let kept = live environment after
         in arrange environment (same (kept ++ taken)) (make (build (kept ++ [name])))
    }
  where
    after = Set.delete name free

-- |
-- The same value under another name. The old name may still be wanted.
renamed :: Text -> Text -> Linear -> Linear
renamed new old Linear {free, build} =
  Linear
    { free = Set.insert old after,
      build = \environment ->
        let kept = live environment after
         in arrange environment (same kept ++ [(new, old)]) (build (kept ++ [new]))
    }
  where
    after = Set.delete new free

-- |
-- Whether a consumer is only given values where it is made: at the end of
-- some way through the statement, which goes no further, and never inside
-- an object, which would hold it, nor to something that is called or
-- forced, which would be given it. Such a consumer need not be made. What
-- gives it a value goes to what it does, as a choice goes to one of its
-- alternatives.
joined :: Text -> Statement -> Bool
joined consumer = go
  where
    go = \case
      Halt -> True
      Abort -> True
      Cut (Variable _) (Covariable _) -> True
      Cut (Variable _) (MuTilde _ body) -> go body
      Cut (Variable _) (Case alternatives otherwise) ->
        and [go body | Alternative _ _ body <- alternatives] && all go otherwise
      Cut (Variable _) (Apply _ (Covariable result)) -> result /= consumer
      Cut (Variable _) spread@(Apply [_] (Apply [_] _))
        | (_, Covariable result) <- spreading spreadLimit spread -> result /= consumer
      Cut (Variable _) (Force (Covariable result)) -> result /= consumer
      Cut (Literal _) (MuTilde _ body) -> go body
      Cut (Global _) (MuTilde _ body) -> go body
      Cut (Constructor _ _) (MuTilde _ body) -> go body
      Cut made@Function {} (MuTilde _ body) -> Set.notMember consumer (Tree.free made) && go body
      Cut made@Thunk {} (MuTilde _ body) -> Set.notMember consumer (Tree.free made) && go body
      -- A consumer made inside is one more place to go to, if it is not
      -- made either; otherwise it would hold this one. If it goes to this
      -- one, nothing else inside goes to this one: it would be held twice.
      Cut (Mu inner first) (MuTilde _ outer) ->
        if Set.member consumer (Tree.free outer)
          then Set.notMember consumer (Tree.free first) && joined inner first && go outer
          else go first
      Cut (Mu new body) (Covariable old)
        | old == consumer -> joined new body && Set.notMember consumer (Tree.free body)
        | otherwise -> go body
      Primitive _ _ (MuTilde _ body) -> go body
      LetRec bindings body -> Set.notMember consumer (Tree.free (map snd bindings)) && go body
      _ -> False

-- |
-- Where patterns run out, what GHC throws is thrown, which can be caught.
-- This is done last of all, so that until now a failure is known for what
-- it is. The helper is a function of one argument, which it does not look
-- at, and is called straight; what is thrown never gives a value, so the
-- consumer it is given is a number, which nothing ever gives to. Neither is
-- counted. What is left of the row is given up.
patternMatchFail :: AxCut.Statement
patternMatchFail =
  AxCut.Literal argument (Tree.Int 0) $
    AxCut.Literal consumer (Tree.Int 0) $
      AxCut.Global function (Runtime.helperGlobal "patternMatchFail") Nothing $
        AxCut.Substitute [(argument, argument), (consumer, consumer), (function, function)] $
          AxCut.Invoke AxCut.Apply
  where
    argument = pack "unused%"
    consumer = pack "never%"
    function = pack "patternMatchFail%"

named :: Producer -> Text
named = \case
  Variable name -> name
  _ -> error "something used was not named"

linearly :: Statement -> Linear
linearly = \case
  Halt -> Linear {free = Set.empty, build = const AxCut.Halt}
  Abort -> Linear {free = Set.empty, build = const patternMatchFail}
  -- A value to its consumer.
  Cut (Variable value) (Covariable consumer) ->
    leaving [value, consumer] (AxCut.Invoke AxCut.Return)
  Cut (Variable old) (MuTilde Binder {name = new} body) ->
    renamed new old (linearly body)
  Cut (Variable scrutinee) (Case alternatives otherwise) ->
    let clauses = do
          Alternative tag binders body <- alternatives
          pure (tag, [name | Binder {name} <- binders], linearly body)
        fallback = linearly <$> otherwise
        wanted =
          mconcat [free Set.\\ Set.fromList names | (_, names, Linear {free}) <- clauses]
            <> foldMap (\Linear {free} -> free) fallback
     in Linear
          { free = Set.insert scrutinee wanted,
            build = \environment ->
              let kept = live environment wanted
                  clause (tag, names, Linear {build}) = AxCut.Clause tag names (build (kept ++ names))
               in arrange environment (same (kept ++ [scrutinee])) $
                    AxCut.Switch
                      (clause <$> clauses)
                      ((\Linear {build} -> build kept) <$> fallback)
          }
  Cut (Variable function) (Apply arguments (Covariable result)) ->
    leaving (map named arguments ++ [result, function]) (AxCut.Invoke AxCut.Apply)
  Cut (Variable value) (Force (Covariable result)) ->
    leaving [result, value] AxCut.Force
  Cut (Variable function) consumer@(Apply [_] (Apply [_] _)) ->
    case spreading spreadLimit consumer of
      (arguments, Covariable result) ->
        leaving (map named arguments ++ [result, function]) (AxCut.Invoke AxCut.Spread)
      _ -> error "a function given arguments in turn was not focused"
  Cut (Literal literal) (MuTilde Binder {name} body) ->
    binding [] name (AxCut.Literal name literal) (linearly body)
  Cut (Global global) (MuTilde Binder {name} body) ->
    binding [] name (AxCut.Global name global Nothing) (linearly body)
  Cut (Constructor tag fields) (MuTilde Binder {name} body) ->
    binding (map named fields) name (AxCut.Let name tag (length fields)) (linearly body)
  -- What consumes the value is the rest: an object that takes the value.
  Cut (Mu covariable inner) (MuTilde Binder {name} outer) ->
    let Linear {free = innerFree, build = innerBuild} = linearly inner
        Linear {free = outerFree, build = outerBuild} = linearly outer
        capturedSet = Set.delete name outerFree
        before = Set.delete covariable innerFree
     in Linear
          { free = before <> capturedSet,
            build = \environment ->
              let captured = live environment capturedSet
                  kept = live environment before
                  object =
                    AxCut.Object
                      { kind = if joined covariable inner then AxCut.Join else AxCut.Closure,
                        captured,
                        method = AxCut.Return,
                        parameters = [name],
                        body = outerBuild (name : captured),
                        whole = Nothing
                      }
               in arrange environment (same (kept ++ captured)) $
                    AxCut.New covariable object (innerBuild (kept ++ [covariable]))
          }
  Cut made@Function {} (MuTilde Binder {name} body) -> allocate made name (linearly body)
  Cut made@Thunk {} (MuTilde Binder {name} body) -> allocate made name (linearly body)
  -- A consumer named by another name.
  Cut (Mu new body) (Covariable old) -> renamed new old (linearly body)
  Primitive primitive arguments (MuTilde Binder {name} body) ->
    binding (map named arguments) name (AxCut.Primitive name primitive) (linearly body)
  -- Each object captures what it mentions of the environment, and then the
  -- other objects it mentions.
  LetRec bindings body ->
    let names = map fst bindings
        nameSet = Set.fromList names
        objects = [(name, objectOf producer) | (name, producer) <- bindings]
        Linear {free = bodyFree, build = bodyBuild} = linearly body
        after = bodyFree Set.\\ nameSet
        outside = mconcat [free Set.\\ nameSet | (_, (free, _)) <- objects]
     in Linear
          { free = after <> outside,
            build = \environment ->
              let kept = live environment after
                  built = do
                    (name, (free, object)) <- objects
                    let mine = live environment (free Set.\\ nameSet)
                        others = filter (`Set.member` free) names
                    pure (mine, AxCut.Recursive {name, object = object (mine ++ others), others})
               in arrange environment (same (kept ++ concatMap fst built)) $
                    AxCut.Rec (map snd built) (bodyBuild (kept ++ names))
          }
  _ -> error "a statement was not focused"
  where
    allocate made name Linear {free, build} =
      let (capturedSet, object) = objectOf made
          after = Set.delete name free
       in Linear
            { free = capturedSet <> after,
              build = \environment ->
                let captured = live environment capturedSet
                    kept = live environment after
                 in arrange environment (same (kept ++ captured)) $
                      AxCut.New name (object captured) (build (kept ++ [name]))
            }

-- |
-- The object a function or a thunk makes: the names it captures, and the
-- object given those names in the order they are captured in.
objectOf :: Producer -> (Set Text, [Text] -> AxCut.Object)
objectOf = \case
  made@(Function parameters result body) ->
    closure (flatten made) AxCut.Closure AxCut.Apply ([name | Binder {name} <- parameters] ++ [result]) body
  Thunk result body -> closure Nothing AxCut.Thunk AxCut.Forced [result] body
  _ -> error "only a function or a thunk can be made into an object"
  where
    closure flat kind method parameters body =
      let Linear {free, build} = linearly body
          wholly = do
            (names, final, innermost) <- flat
            let Linear {build = wholeBuild} = linearly innermost
            pure (names ++ [final], wholeBuild)
       in ( free Set.\\ Set.fromList parameters,
            \captured ->
              AxCut.Object
                { kind,
                  captured,
                  method,
                  parameters,
                  body = build (parameters ++ captured),
                  whole =
                    (\(wholeParameters, wholeBuild) -> AxCut.Wholly {wholeParameters, wholeBody = wholeBuild (wholeParameters ++ captured)})
                      <$> wholly
                }
          )

-- |
-- A function of one parameter that only gives back another, and so on, as
-- one function of all of their parameters: the parameters, the consumer of
-- the last result, and the last body. The function that is given back is
-- made where it is given back, under a name of its own, once focused.
--
-- The last body is written twice, once for each way in. So a function whose
-- last body has such a function inside it, anywhere, is not given the
-- entry: each would be written twice inside the other's two, and so on
-- down, which made one derived instance of 'Read' a million functions.
flatten :: Producer -> Maybe ([Text], Text, Statement)
flatten producer = case chain producer of
  Just found@(_, _, innermost) | not (nested innermost) -> Just found
  _ -> Nothing
  where
    chain = \case
      Function [Binder {name}] result (Cut inner@(Function [Binder {name = next}] innerResult innerBody) (MuTilde Binder {name = made} (Cut (Variable given) (Covariable back))))
        | made == given,
          back == result ->
            Just $ case chain inner of
              Just (names, final, innermost) -> (name : names, final, innermost)
              Nothing -> ([name, next], innerResult, innerBody)
      _ -> Nothing
    -- Whether a function inside a statement is one that would be given the
    -- entry.
    nested = statement
      where
        statement = \case
          Cut left right -> producer' left || consumer right
          Primitive _ arguments result -> any producer' arguments || consumer result
          LetRec bindings body -> any (producer' . snd) bindings || statement body
          Abort -> False
          Halt -> False
        producer' = \case
          made@(Function _ _ body) -> maybe False (const True) (chain made) || statement body
          Thunk _ body -> statement body
          Mu _ body -> statement body
          Constructor _ fields -> any producer' fields
          _ -> False
        consumer = \case
          MuTilde _ body -> statement body
          Case alternatives otherwise -> or [statement body | Alternative _ _ body <- alternatives] || any statement otherwise
          Apply arguments result -> any producer' arguments || consumer result
          Force result -> consumer result
          Covariable _ -> False
