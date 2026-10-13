module Generate.Go.Expression where

import Control.Monad (zipWithM)
import Control.Monad.ST (ST)
import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.ConstructorInfo (ConstructorInfo (..))
import Core.Tree.EntryInfo (EntryInfo (EntryInfo))
import qualified Core.Tree.EntryInfo
import Core.Tree.Expression (Expression (..))
import Core.Tree.Instanciation (Instanciation, InstanciationF (Instanciation))
import qualified Core.Tree.Instanciation as Instanciation
import Core.Tree.MethodInfo (MethodInfo (..))
import Core.Tree.TypeLambda (TypeLambda, TypeLambdaOver (..))
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Generate.Binding.Term as Term
import Generate.Context (Context (..), fresh, singleBinding, symbol, (!-))
import qualified Generate.Context as Context
import {-# SOURCE #-} qualified Generate.Go.Declarations as Declarations
import qualified Generate.Go.Evidence as Evidence
import qualified Generate.Go.Statements as Statements
import qualified Generate.Info as Info
import qualified Generate.Mangle as Mangle
import Generate.Target (Target (..), finish)
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Field as Javascript (Field (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import Semantic.Scope (SimpleDeclaration)
import qualified Semantic.Scope as Scope

delay :: [Javascript.Statement 'True] -> Javascript.Expression
delay body =
  let flag =
        Javascript.Expression
          Javascript.Assign
            { target =
                Javascript.Member
                  { object = Javascript.This,
                    field = Mangle.lazy
                  },
              value =
                Javascript.Number
                  { number = 0
                  }
            }
      return =
        Javascript.Return
          Javascript.Member
            { object = Javascript.This,
              field = Mangle.value
            }
      fields =
        [ ( Mangle.lazy,
            Javascript.Literal
              { literal = Javascript.Number {number = 1}
              }
          ),
          ( Mangle.value,
            Javascript.Method
              { definition = mconcat [body, [flag], [return]]
              }
          )
        ]
   in Javascript.Object {fields}

strict :: Javascript.Expression -> Javascript.Expression
strict literal =
  let fields =
        [ ( Mangle.lazy,
            Javascript.Literal
              { literal = Javascript.Number {number = 0}
              }
          ),
          ( Mangle.value,
            Javascript.Literal
              { literal
              }
          )
        ]
   in Javascript.Object {fields}

done :: Javascript.Expression
done =
  Javascript.Member
    { object = Javascript.This,
      field = Mangle.value
    }

force :: Javascript.Expression -> Javascript.Expression
force object =
  let base =
        Javascript.Member
          { object,
            field = Mangle.value
          }
      value =
        Javascript.Ternary
          { condition =
              Javascript.Member
                { object,
                  field = Mangle.lazy
                },
            valid =
              Javascript.Call
                { function = base,
                  arguments = []
                },
            invalid = base
          }
   in value

implicit :: Context s scope -> Bool -> Javascript.Expression -> Instanciation scope -> ST s Javascript.Expression
implicit _ False thunk Instanciation.Mono = pure $ force thunk
implicit _ True value Instanciation.Mono = pure $ value
implicit context _ function (Instanciation arguments) = do
  arguments <- traverse (Evidence.generate context) (toList arguments)
  pure Javascript.Call {function, arguments}

generateConstructor ::
  Context s scope ->
  Constructor.Index scope ->
  Strict.Vector (Expression scope) ->
  ConstructorInfo scope ->
  ST s ([Javascript.Statement 'True], Javascript.Expression)
generateConstructor context Constructor.Index {constructorIndex} arguments ConstructorInfo {entries} = do
  let process EntryInfo {strict} argument =
        if Info.strict strict
          then do
            (extra, value) <- generatePure context argument
            pure (extra, value)
          else do
            value <- thunk context Done argument
            pure ([], value)
  (extra, arguments) <- unzip <$> zipWithM process (toList entries) (toList arguments)
  let tag = Javascript.Number constructorIndex
      elements = map Javascript.Literal (tag : arguments)
      fields = zip Mangle.fields elements
      object = Javascript.Object {fields}
  pure (concat extra, object)

-- |
-- The characters of a list of characters that is written out in full, if
-- an expression is one and is long. Such a list is one object inside
-- another as deep as it is long, and a long one is deeper than what reads
-- the program will go.
written :: Expression scope -> Maybe [Char]
written expression = case characters expression of
  Just found | length (take 33 found) > 32, all plain found -> Just found
  _ -> Nothing
  where
    characters :: Expression scope -> Maybe [Char]
    characters = \case
      Constructor {constructor, arguments}
        | constructor == Constructor.nil, null arguments -> Just []
        | constructor == Constructor.cons,
          [Character {character}, rest] <- toList arguments ->
            (character :) <$> characters rest
      _ -> Nothing
    -- Half of a pair that stands for one character cannot be kept in a
    -- string by itself.
    plain character = ord character < 0xd800 || ord character > 0xdfff

-- |
-- A list of characters, made as the program runs from a string that holds
-- them.
generateString :: Context s scope -> [Char] -> ST s Javascript.Expression
generateString context characters = do
  let Mangle.Builtin {unpackText} = Context.builtin context
  unpack <- unpackText
  pure
    Javascript.Call
      { function =
          Javascript.Member
            { object = Javascript.Variable {name = unpack},
              field = Mangle.value
            },
        arguments = [strict Javascript.String {string = Text.pack characters}]
      }

generateLambda :: Context s scope -> Expression (SimpleDeclaration 'Scope.:+ scope) -> ST s Javascript.Expression
generateLambda context body = do
  name <- fresh context
  context <- pure $ singleBinding name context
  statements <- generateInto context Return body
  pure
    Javascript.Arrow
      { parameters = [name],
        body = statements
      }

generateCharacter :: Char -> Javascript.Expression
generateCharacter character = Javascript.Number {number = ord character}

generateInteger :: Integer -> Javascript.Expression
generateInteger integer = Javascript.BigInt {bigInt = integer}

-- |
-- Is the thunk a part of a binding group or not?
data Binder
  = Done
  | Group

thunk :: Context s scope -> Binder -> Expression scope -> ST s Javascript.Expression
thunk context Done Variable {variable, instanciation = Instanciation.Mono}
  | Term.Binding {name, strict = False} <- context !- variable = do
      name <- symbol context name
      pure Javascript.Variable {name}
thunk context binder expression = case expression of
  _ | Just characters <- written expression -> strict <$> generateString context characters
  Constructor {constructor, arguments, constructorInfo} -> do
    (extra, value) <- generateConstructor context constructor arguments constructorInfo
    if not (null extra)
      then do
        let body = extra ++ [Javascript.Expression Javascript.Assign {target = done, value}]
        pure (delay body)
      else do
        pure (strict value)
  Character {character} -> pure (strict $ generateCharacter character)
  Integer {integer} -> pure (strict $ generateInteger integer)
  Lambda {body} -> do
    value <- generateLambda context body
    pure $ strict value
  Newtype {argument} -> thunk context binder argument
  _ -> do
    body <- generateInto context (Assign done) expression
    pure (delay body)

generatePure,
  generateImpure ::
    Context s scope ->
    Expression scope ->
    ST s ([Javascript.Statement 'True], Javascript.Expression)
generateImpure context expression = case expression of
  Variable {variable, instanciation} -> do
    let Term.Binding {name, strict, delayed} = context !- variable
    name <- symbol context name
    let expression = case instanciation of
          Instanciation {} | delayed -> force Javascript.Variable {name}
          _ -> Javascript.Variable {name}
    result <- implicit context strict expression instanciation
    pure ([], result)
  Method
    { method = Method.Index {methodIndex},
      evidence,
      instanciation,
      methodInfo = MethodInfo {constraintCount}
    } -> do
      evidence <- Evidence.generate context evidence
      name <- Context.fresh context
      let value =
            Javascript.Member
              { object = evidence,
                field = Mangle.fields !! (methodIndex + constraintCount)
              }
          statement = Javascript.Const name value
          expression = Javascript.Variable {name}
      result <- implicit context False expression instanciation
      pure ([statement], result)
  Selector
    { selector = Selector.Index {selectorIndex},
      argument,
      selectorInfo = EntryInfo {strict}
    } -> do
      (statements, object) <- generateImpure context argument
      let select =
            Javascript.Member
              { object,
                field = Mangle.fields !! (selectorIndex + 1)
              }
      result <- implicit context (Info.strict strict) select Instanciation.Mono
      pure (statements, result)
  -- A definition that is given every argument is called in the form that
  -- takes them at once. Whatever is given past that is given to what it
  -- gives back.
  Call {}
    | (Variable {variable, instanciation}, given) <- spine expression [],
      Term.Binding {worker = Just (worker, count)} <- context !- variable,
      length given >= count -> do
        worker <- symbol context worker
        evidence <- case instanciation of
          Instanciation.Mono -> pure []
          Instanciation evidence -> traverse (Evidence.generate context) (toList evidence)
        given <- traverse (thunk context Done) given
        let (first, rest) = splitAt count given
            call =
              Javascript.Call
                { function = Javascript.Variable {name = worker},
                  arguments = evidence ++ first
                }
            more function argument = Javascript.Call {function, arguments = [argument]}
        pure ([], foldl more call rest)
  Call {function, argument} -> do
    (statements, function) <- generateImpure context function
    argument <- thunk context Done argument
    let call =
          Javascript.Call
            { function,
              arguments = [argument]
            }
    pure (statements, call)
  Let {declarations, letBody} -> do
    (context, declarations) <- Declarations.generate context declarations
    (extra, result) <- generateImpure context letBody
    pure (declarations ++ extra, result)
  Dictionary {evidence} -> do
    value <- Evidence.generate context evidence
    pure ([], value)
  -- An action is a function of nothing, and what it gives may be yet to be
  -- evaluated.
  -- What it gives is named before it is evaluated: to evaluate something
  -- is to write it out more than once, and the action is run once.
  Run {imperative} -> do
    (statements, function) <- generateImpure context imperative
    name <- Context.fresh context
    let ran = Javascript.Const name Javascript.Call {function, arguments = []}
    pure (statements ++ [ran], force Javascript.Variable {name})
  Generalized {definition = definition@TypeLambdaOver {constraints, result}} -> case constraints of
    None -> generateImpure (Context.evidenceBindings Vector.empty context) result
    Constraints {} -> do
      (extra, value) <- declaration context Nothing definition
      pure (extra, value)
  _ -> generatePure context expression
generatePure context expression = case expression of
  _ | Just characters <- written expression -> do
    value <- generateString context characters
    pure ([], value)
  Constructor {constructor, arguments, constructorInfo} -> do
    generateConstructor context constructor arguments constructorInfo
  Character {character} -> pure ([], generateCharacter character)
  Integer {integer} -> pure ([], generateInteger integer)
  Lambda {body} -> do
    value <- generateLambda context body
    pure ([], value)
  Newtype {argument} -> generatePure context argument
  _ -> do
    name <- fresh context
    let letx = Javascript.Let name
    body <- generateInto context (Assign Javascript.Variable {name}) expression
    pure (letx : body, Javascript.Variable {name})

generateInto ::
  Context s scope ->
  Target return ->
  Expression scope ->
  ST s [Javascript.Statement 'True]
generateInto context target = \case
  Let {declarations, letBody} -> do
    (context, declarations) <- Declarations.generate context declarations
    result <- generateInto context target letBody
    pure $ declarations ++ result
  Join {statements} -> Statements.generate context target statements
  Newtype {argument} -> generateInto context target argument
  expression -> do
    (extra, value) <- generateImpure context expression
    pure $ extra ++ [finish target value]

-- |
-- A function and what it is given, from a function given one thing at a
-- time.
spine :: Expression scope -> [Expression scope] -> (Expression scope, [Expression scope])
spine expression given = case expression of
  Call {function, argument} -> spine function (argument : given)
  _ -> (expression, given)

-- |
-- How many arguments a definition takes all at once in its second form, if
-- it has one.
--
-- A function of several arguments is a function that gives back a function,
-- and to call it is to make each of those in turn. A definition that is
-- written as functions inside functions is made a second time as one
-- function of every argument, and of the evidence it requires before them,
-- which a call that gives every argument calls. The first form is left for
-- everything else, and only calls the second.
--
-- A definition of one argument has a second form only when it requires
-- evidence, which is otherwise one more function to make.
uncurried :: TypeLambda scope -> Maybe Int
uncurried TypeLambdaOver {constraints, result}
  | count >= 2 = Just count
  | count >= 1, Constraints {} <- constraints = Just count
  | otherwise = Nothing
  where
    count = depth result
    depth :: Expression scope' -> Int
    depth = \case
      Lambda {body} -> 1 + depth body
      _ -> 0

-- |
-- A definition. If it is given what its second form is called, it is made
-- in both: the second is what has to be declared first.
declaration ::
  Context s scope ->
  Maybe Text ->
  TypeLambda scope ->
  ST s ([Javascript.Statement local], Javascript.Expression)
declaration context worker definition@TypeLambdaOver {constraints, result = expression} = do
  let count = case constraints of
        Constraints constraints -> length constraints
        None -> 0
  fresh <- Vector.replicateM count (Context.fresh context)
  inner <- pure $ Context.evidenceBindings fresh context
  case (worker, uncurried definition) of
    (Just worker, Just arity) -> do
      (names, statements) <- peel inner arity expression
      let whole =
            Javascript.Arrow
              { parameters = toList fresh ++ names,
                body = statements
              }
          call =
            Javascript.Call
              { function = Javascript.Variable {name = worker},
                arguments = [Javascript.Variable {name} | name <- toList fresh ++ names]
              }
          one name rest = Javascript.Arrow {parameters = [name], body = [Javascript.Return rest]}
          curried = foldr one call names
          first = case constraints of
            None -> strict curried
            Constraints {} ->
              Javascript.Arrow
                { parameters = toList fresh,
                  body = [Javascript.Return curried]
                }
      pure ([Javascript.Const worker whole], first)
    _ -> case constraints of
      None -> do
        value <- thunk inner Group expression
        pure ([], value)
      Constraints {} -> do
        statements <- generateInto inner Return expression
        pure
          ( [],
            Javascript.Arrow
              { parameters = toList fresh,
                body = statements
              }
          )
  where
    -- The body of some functions inside each other, and what each takes.
    peel :: Context s scope' -> Int -> Expression scope' -> ST s ([Text], [Javascript.Statement 'True])
    peel context remaining = \case
      Lambda {body} | remaining > 0 -> do
        name <- fresh' context
        (names, statements) <- peel (singleBinding name context) (remaining - 1) body
        pure (name : names, statements)
      body -> do
        statements <- generateInto context Return body
        pure ([], statements)
    fresh' = Context.fresh
