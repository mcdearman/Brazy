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
module AxCut.Lower (lower, lowerDefinitions) where

import qualified AxCut.Tree as AxCut
import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Generate.Global (Global)
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
  pure
    AxCut.Program
      { definitions,
        entry = build (linearly entry) []
      }

-- |
-- Definitions by themselves, as for a unit that is linked with others.
lowerDefinitions :: Map Global Producer -> Map Global AxCut.Object
lowerDefinitions definitions = runST (lowerDefinitions' definitions)

lowerDefinitions' :: Map Global Producer -> ST s (Map Global AxCut.Object)
lowerDefinitions' definitions = do
  supply <- newSTRef 0
  traverse (definition supply) definitions

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
    bindings <- traverse (traverse (inside supply)) bindings
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

named :: Producer -> Text
named = \case
  Variable name -> name
  _ -> error "something used was not named"

linearly :: Statement -> Linear
linearly = \case
  Halt -> Linear {free = Set.empty, build = const AxCut.Halt}
  Abort -> Linear {free = Set.empty, build = const AxCut.Abort}
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
  Cut (Literal literal) (MuTilde Binder {name} body) ->
    binding [] name (AxCut.Literal name literal) (linearly body)
  Cut (Global global) (MuTilde Binder {name} body) ->
    binding [] name (AxCut.Global name global) (linearly body)
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
                      { kind = AxCut.Closure,
                        captured,
                        method = AxCut.Return,
                        parameters = [name],
                        body = outerBuild (name : captured)
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
  Function parameters result body ->
    closure AxCut.Closure AxCut.Apply ([name | Binder {name} <- parameters] ++ [result]) body
  Thunk result body -> closure AxCut.Thunk AxCut.Forced [result] body
  _ -> error "only a function or a thunk can be made into an object"
  where
    closure kind method parameters body =
      let Linear {free, build} = linearly body
       in ( free Set.\\ Set.fromList parameters,
            \captured ->
              AxCut.Object
                { kind,
                  captured,
                  method,
                  parameters,
                  body = build (parameters ++ captured)
                }
          )
