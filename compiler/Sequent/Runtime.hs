-- |
-- The @Hazy@ module, written in the sequent calculus.
--
-- For Javascript these definitions are in @runtime/javascript/Hazy.mjs@.
-- Here each is written over the primitives of "Sequent.Primitive", so that
-- the runtime has only those to provide.
module Sequent.Runtime
  ( hazy,
    main,
    global,
    builtin,
    tuple,
    definition,
    fixed,
    generated,
    member,
    showGlobal,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Text (Text, pack, unpack, words)
import qualified Data.Text as Text (any)
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Text.Lazy.Builder as Builder
import Generate.Global (Global)
import qualified Generate.Global as Global (Global (..))
import qualified Semantic.Index.Type2 as Type2
import Sequent.Primitive (Primitive (..))
import qualified Sequent.Primitive as Primitive
import qualified Sequent.Printer as Printer
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Literal (..),
    Producer (..),
    Statement (..),
    Strategy (..),
    separator,
  )
import Syntax.Lexer (FullQualifiers (..), Qualifiers (..), constructorIdentifier)
import Text.Read (readMaybe)
import Prelude hiding (words)

-- |
-- The module of primitives.
hazy :: FullQualifiers
hazy = Local :.. constructorIdentifier (pack "Hazy")

-- |
-- The module the builtin instances are defined in.
helper :: FullQualifiers
helper = Local :. constructorIdentifier (pack "Hazy") :.. constructorIdentifier (pack "Helper")

-- |
-- A definition of a module whose name has one part.
global :: Text -> Text -> Global
global modulex name =
  Global.Global
    { path = Local :.. constructorIdentifier modulex,
      name
    }

-- |
-- Where a program starts.
main :: Global
main = global (pack "Main") (pack "main")

showGlobal :: Global -> String
showGlobal = Lazy.Text.unpack . Builder.toLazyText . Printer.global

-- |
-- The instance of a builtin class for a builtin type. These are ordinary
-- instances in @Hazy.Helper@.
builtin :: Type2.Index scope -> Type2.Index scope -> Global
builtin classx typex =
  Global.Global
    { path = helper,
      name = mconcat [pack "instance Hazy.", className, pack " Helper", typeName]
    }
  where
    className = case classx of
      Type2.Index {} -> error "bad evidence"
      Type2.Lifted {} -> error "bad evidence"
      _ -> pack (show classx)
    typeName = case typex of
      Type2.Index {} -> error "bad evidence"
      Type2.Lifted {} -> error "bad evidence"
      Type2.Tuple 0 -> pack "Unit"
      Type2.Tuple _ -> error "bad evidence"
      _ -> pack (show typex)

-- |
-- The instance of a builtin class for the tuples of a size, as a function
-- of the evidence for each element.
tuple :: Type2.Index scope -> Int -> Maybe Global
tuple classx number = case classx of
  Type2.Eq -> named "eqTuple"
  Type2.Ord -> named "ordTuple"
  Type2.Semigroup -> named "semigroupTuple"
  Type2.Monoid -> named "monoidTuple"
  Type2.Bounded -> named "boundedTuple"
  Type2.Show -> named "showTuple"
  Type2.Read -> named "readTuple"
  _ -> Nothing
  where
    named name =
      Just
        Global.Global
          { path = hazy,
            name = pack name <> pack " " <> pack (show number)
          }

-- |
-- A method of an instance that needs nothing, as a definition of its own.
-- Every such instance at the top of a module has its methods defined this
-- way, so that a use of a method of a known instance can go straight to it
-- and not through the instance.
member :: Global -> Int -> Global
member Global.Global {path, name} index =
  Global.Global
    { path,
      name = name <> pack " #" <> pack (show index)
    }

-- |
-- What a name of the @Hazy@ module is defined as, if it is one.
definition :: Global -> Maybe Producer
definition global@Global.Global {path, name}
  | path /= hazy = Nothing
  -- A function that was inside one of these is found through it.
  | Text.any (== separator) name = Nothing
  | Just producer <- Map.lookup name definitions = Just producer
  | Just producer <- generated global = Just producer
  | otherwise = Just (missing name)

-- |
-- A primitive that this runtime does not have. A program that uses it fails
-- when it does, and one that only could have is compiled all the same.
missing :: Text -> Producer
missing name =
  Thunk (pack "k") $
    Primitive Error [Literal (Text (pack "missing primitive: " <> name))] (covariable "k")

-- |
-- The definitions of the @Hazy@ module that are written out here. They are
-- compiled once, with the package that has the module.
fixed :: Map Global Producer
fixed = Map.mapKeys (\name -> Global.Global {path = hazy, name}) definitions

-- |
-- A definition that is made when it is asked for: the instance of a builtin
-- class for the tuples of some size. There is no end of these, so each is
-- compiled wherever it is used.
generated :: Global -> Maybe Producer
generated Global.Global {path, name}
  | path == hazy,
    [classx, number] <- words name,
    Just number <- readMaybe (unpack number),
    Just make <- Map.lookup classx tuples =
      Just (make number)
  | otherwise = Nothing

byValue, byNeed :: String -> Binder
byValue name = Binder {name = pack name, strategy = ByValue}
byNeed name = Binder {name = pack name, strategy = ByNeed}

variable :: String -> Producer
variable = Variable . pack

covariable :: String -> Consumer
covariable = Covariable . pack

-- |
-- A function of one lazy argument.
function :: String -> String -> Statement -> Producer
function parameter result = Function [byNeed parameter] (pack result)

-- |
-- Evaluate a variable and name its value.
force :: String -> String -> Statement -> Statement
force name value body = Cut (variable name) (Force (MuTilde (byValue value) body))

-- |
-- A curried function that evaluates its arguments and gives them to a
-- primitive.
wrap :: Primitive -> Producer
wrap primitive = go 0
  where
    count = Primitive.arity primitive
    argument, value, result :: Int -> String
    argument index = "x" ++ show index
    value index = "v" ++ show index
    result index = "k" ++ show index
    go index
      | index == count - 1 = function (argument index) (result index) call
      | otherwise =
          function (argument index) (result index) $
            Cut (go (index + 1)) (covariable (result index))
    call =
      foldr
        (\index -> force (argument index) (value index))
        (Primitive primitive (variable . value <$> indexes) (covariable (result (count - 1))))
        indexes
    indexes = [0 .. count - 1]

constant :: Literal -> Producer
constant literal = Thunk (pack "k") (Cut (Literal literal) (covariable "k"))

identity :: Producer
identity = function "x" "k" $ Cut (variable "x") (Force (covariable "k"))

nil, cons :: Int
nil = 0
cons = 1

definitions :: Map Text Producer
definitions =
  Map.fromList
    [ (pack name, producer)
      | (name, producer) <-
          [ ("errorText", wrap Error),
            ("pack", packx),
            ("unpack", unpackx),
            ("putStrLnText", putStrLnText),
            ("traceText", traceText),
            ("generalCategory", wrap GeneralCategory),
            ("primIntToChar", identity),
            ("primCharToInt", identity),
            ("primEqualInt", wrap IntEqual),
            ("primLessThenEqualInt", wrap IntLessThenEqual),
            ("primIntMinBound", constant (Int minBound)),
            ("primIntMaxBound", constant (Int maxBound)),
            ("primIntAdd", wrap IntAdd),
            ("primIntMinus", wrap IntMinus),
            ("primIntMultiply", wrap IntMultiply),
            ("primIntNegate", wrap IntNegate),
            ("primIntAbs", wrap IntAbs),
            ("primIntSignum", wrap IntSignum),
            ("primEqualInteger", wrap IntegerEqual),
            ("primLessThenEqualInteger", wrap IntegerLessThenEqual),
            ("primIntToInteger", wrap IntToInteger),
            ("primIntQuot", wrap IntQuot),
            ("primIntRem", wrap IntRem),
            ("primIntegerCastToInt", wrap IntegerCastToInt),
            ("primIntegerTruncateToInt", wrap IntegerTruncateToInt),
            ("primIntegerEqual", wrap IntegerEqual),
            ("primIntegerLessThenEqual", wrap IntegerLessThenEqual),
            ("primIntegerAdd", wrap IntegerAdd),
            ("primIntegerMinus", wrap IntegerMinus),
            ("primIntegerMultiply", wrap IntegerMultiply),
            ("primIntegerNegate", wrap IntegerNegate),
            ("primIntegerAbs", wrap IntegerAbs),
            ("primIntegerSignum", wrap IntegerSignum),
            ("primIntegerQuot", wrap IntegerQuot),
            ("primIntegerRem", wrap IntegerRem),
            ("primTextNull", wrap TextNull),
            ("primTextHead", wrap TextHead),
            ("primTextTail", wrap TextTail),
            ("primTextSnoc", wrap TextSnoc),
            ("primTextAppend", wrap TextAppend),
            ("primTextEqual", wrap TextEqual),
            ("primTextCompare", wrap TextCompare),
            ("primTextLength", wrap TextLength),
            ("primTextTake", wrap TextTake),
            ("primTextDrop", wrap TextDrop),
            ("primTextIsPrefixOf", wrap TextIsPrefixOf),
            ("primTextConcat", concatx),
            ("primSTPure", primSTPure),
            ("primSTBind", primSTBind),
            ("primUnsafeCoerce", identity),
            ("primNewRef", newRef),
            ("primReadRef", effect "read_ref"),
            ("primWriteRef", writeRef),
            ("primSameRef", wrap (outside "same_ref")),
            ("primArgCount", effect "arg_count"),
            ("primArg", effect "arg"),
            ("primGetEnv", effect "get_env"),
            ("primExecutablePath", effect "executable_path"),
            ("primExit", effect "exit"),
            ("primOpen", effect "open"),
            ("primClose", effect "close"),
            ("primWrite", effect "write"),
            ("primReadAll", effect "read_all"),
            ("primReadLine", effect "read_line"),
            ("primIsTerminal", effect "is_terminal"),
            ("primListDirectory", effect "list_directory"),
            ("primCreateDirectory", effect "create_directory"),
            ("primPathKind", effect "path_kind"),
            ("primCallProcess", effect "call_process"),
            ("primClock", effect "clock"),
            ("primOS", Thunk (pack "k") (Primitive (outside "os") [] (covariable "k"))),
            ("primThrow", throw),
            ("primCatch", catch)
          ]
    ]

-- |
-- An action is a function of no arguments. Running it gives its result,
-- which is not forced.
action :: String -> Statement -> Producer
action result = Function [] (pack result)

-- |
-- A primitive the runtime has under a name of its own.
outside :: String -> Primitive
outside name = case lookup name Primitive.outside of
  Just count -> Foreign (pack name) count
  Nothing -> error $ "no such primitive: " ++ name

-- |
-- A primitive that does something, as an action: a function of what it is
-- given, which are evaluated when the action is run and not before.
--
-- > effect f = \x y -> \() -> f (force x) (force y)
effect :: String -> Producer
effect name = go 0
  where
    primitive = outside name
    count = Primitive.arity primitive
    argument, value, result :: Int -> String
    argument index = "x" ++ show index
    value index = "v" ++ show index
    result index = "k" ++ show index
    go index
      | index == count = action "done" call
      | otherwise =
          function (argument index) (result index) $
            Cut (go (index + 1)) (covariable (result index))
    call =
      foldr
        (\index -> force (argument index) (value index))
        (Primitive primitive (variable . value <$> indexes) (covariable "done"))
        indexes
    indexes = [0 .. count - 1]

-- A reference holds whatever it was given, evaluated or not.
--
-- > primNewRef value = \() -> new value
newRef :: Producer
newRef =
  function "value" "k" $
    Cut
      (action "done" $ Primitive (outside "new_ref") [variable "value"] (covariable "done"))
      (covariable "k")

-- > primWriteRef reference value = \() -> write (force reference) value
writeRef :: Producer
writeRef =
  function "reference" "k" $
    Cut
      ( function "value" "k1" $
          Cut
            ( action "done" $
                force "reference" "place" $
                  Primitive (outside "write_ref") [variable "place", variable "value"] (covariable "done")
            )
            (covariable "k1")
      )
      (covariable "k")

-- To catch is to leave with the runtime what handles a thrown value and
-- where to go on from, run the action, and take them back. Where to go on
-- from is whatever was waiting for the action's result, as a function.
--
-- > primCatch action handler = \() -> push handler resume; result = force action (); pop; result
catch :: Producer
catch =
  function "action" "k" $
    Cut
      ( function "handler" "k1" $
          Cut
            ( action "done" $
                LetRec
                  [ ( pack "resume",
                      Function [byNeed "given"] (pack "unused") $
                        Cut (variable "given") (covariable "done")
                    )
                  ]
                  $ Primitive (outside "catch_push") [variable "handler", variable "resume"]
                  $ MuTilde (byValue "pushed")
                  $ Cut (variable "action")
                  $ Force
                  $ Apply []
                  $ MuTilde (byNeed "result")
                  $ Primitive (outside "catch_pop") []
                  $ MuTilde (byValue "popped")
                  $ Cut (variable "result") (covariable "done")
            )
            (covariable "k1")
      )
      (covariable "k")

-- To throw is to take back the catch begun last, run its handler on the
-- value, and go on from where that catch was begun. Whatever was waiting
-- for what is thrown never gets anything.
--
-- > primThrow value = (handler, resume) = pop; resume (force handler value ())
throw :: Producer
throw =
  function "value" "k" $
    Primitive (outside "catch_pop") [] $
      Case
        [ Alternative 0 [byNeed "handler", byNeed "resume"] $
            Cut (variable "handler") $
              Force $
                Apply [variable "value"] $
                  Apply [] $
                    MuTilde (byNeed "result") $
                      Cut (variable "resume") $
                        Force $
                          Apply [variable "result"] (covariable "k")
        ]
        Nothing

-- > putStrLnText text = \() -> print (force text)
putStrLnText :: Producer
putStrLnText =
  function "text" "k" $
    Cut
      ( action "done" $
          force "text" "value" $
            Primitive PutStrLn [variable "value"] (covariable "done")
      )
      (covariable "k")

-- > traceText text value = print (force text) >> force value
traceText :: Producer
traceText =
  function "text" "k" $
    Cut
      ( function "value" "done" $
          force "text" "message" $
            Primitive PutStrLn [variable "message"] $
              MuTilde (byValue "unit") $
                Cut (variable "value") (Force (covariable "done"))
      )
      (covariable "k")

-- > primSTPure value = \() -> value
primSTPure :: Producer
primSTPure =
  function "value" "k" $
    Cut
      (action "done" $ Cut (variable "value") (covariable "done"))
      (covariable "k")

-- > primSTBind first next = \() -> force next (force first ()) ()
primSTBind :: Producer
primSTBind =
  function "first" "k" $
    Cut
      ( function "next" "k1" $
          Cut
            ( action "done" $
                Cut (variable "first") $
                  Force $
                    Apply [] $
                      MuTilde (byNeed "result") $
                        Cut (variable "next") $
                          Force $
                            Apply [variable "result"] $
                              Apply [] (covariable "done")
            )
            (covariable "k1")
      )
      (covariable "k")

-- A text is built in a place the runtime keeps, which each character is
-- added to, so that it takes time by its length and not by the square of it.
--
-- > pack list = go new list
-- >   where
-- >     go buffer [] = done buffer
-- >     go buffer (head : tail) = push buffer (force head); go buffer tail
packx :: Producer
packx = gathered "pack_push"

-- > primTextConcat list = go new list
-- >   where
-- >     go buffer [] = done buffer
-- >     go buffer (head : tail) = append buffer (force head); go buffer tail
concatx :: Producer
concatx = gathered "pack_append"

-- |
-- A text built of a list, each element added by a primitive.
gathered :: String -> Producer
gathered add =
  function "list" "k" $
    LetRec
      [ ( pack "go",
          Function [byValue "buffer", byNeed "rest"] (pack "done") $
            Cut (variable "rest") $
              Force $
                Case
                  [ Alternative nil [] $
                      Primitive (outside "pack_done") [variable "buffer"] (covariable "done"),
                    Alternative cons [byNeed "head", byNeed "tail"] $
                      force "head" "element" $
                        Primitive (outside add) [variable "buffer", variable "element"] $
                          MuTilde (byValue "added") $
                            Cut (variable "go") $
                              Apply [variable "buffer", variable "tail"] (covariable "done")
                  ]
                  Nothing
        )
      ]
      $ Primitive (outside "pack_new") []
      $ MuTilde (byValue "buffer")
      $ Cut (variable "go")
      $ Apply [variable "buffer", variable "list"] (covariable "k")

-- > unpack text = go (force text)
-- >   where
-- >     go text
-- >       | null text = []
-- >       | otherwise = head text : go (tail text)
unpackx :: Producer
unpackx =
  function "lazy" "k" $
    LetRec
      [ ( pack "go",
          Function [byValue "text"] (pack "done") $
            Primitive TextNull [variable "text"] $
              Case
                [ Alternative 1 [] $ Cut (Constructor nil []) (covariable "done"),
                  Alternative 0 [] $
                    Primitive TextHead [variable "text"] $
                      MuTilde (byValue "head") $
                        Primitive TextTail [variable "text"] $
                          MuTilde (byValue "tail") $
                            Cut
                              ( Constructor
                                  cons
                                  [ variable "head",
                                    Thunk (pack "rest") $
                                      Cut (variable "go") $
                                        Apply [variable "tail"] (covariable "rest")
                                  ]
                              )
                              (covariable "done")
                ]
                Nothing
        )
      ]
      $ force "lazy" "text"
      $ Cut (variable "go")
      $ Apply [variable "text"] (covariable "k")

-- |
-- A small language for writing the instances for tuples, which for
-- Javascript are written by hand in @Hazy.mjs@. It is translated the way
-- "Sequent.FromCore" translates Core.
data Term
  = -- | A variable that may hold a thunk.
    Lazy String
  | -- | A variable that holds a value, such as evidence.
    Value String
  | Helper String
  | Character Char
  | Zero
  | -- | A constructed value. Its fields are not evaluated.
    Construct Int [Term]
  | Lambda String Term
  | -- | A function of evidence.
    Given [String] Term
  | Term :@ Term
  | -- | A definition applied to evidence.
    Instance Global [Term]
  | -- | A method of a class, given its place and the evidence.
    Method Int Term
  | -- | The evidence for a superclass, given its place and the evidence.
    Super Int Term
  | -- | Take a constructed value apart.
    Match Term [(Int, [String], Term)]

infixl 9 :@

type Supply s = STRef s Int

fresh :: Supply s -> String -> ST s Text
fresh supply hint = do
  number <- readSTRef supply
  writeSTRef supply $! number + 1
  pure $ pack $ hint ++ "%" ++ show number

helperGlobal :: String -> Global
helperGlobal name = Global.Global {path = helper, name = pack name}

evaluate :: Supply s -> Term -> Consumer -> ST s Statement
evaluate supply term consumer = case term of
  Lazy name -> pure $ Cut (variable name) (Force consumer)
  Value name -> pure $ Cut (variable name) consumer
  Helper name -> pure $ Cut (Global $ helperGlobal name) (Force consumer)
  Character character -> pure $ Cut (Literal $ Int $ fromIntegral $ ord character) consumer
  Zero -> pure $ Cut (Literal $ Int 0) consumer
  Construct {} -> do
    producer <- delayed supply term
    pure $ Cut producer consumer
  Lambda {} -> do
    producer <- delayed supply term
    pure $ Cut producer consumer
  Given {} -> do
    producer <- delayed supply term
    pure $ Cut producer consumer
  function :@ argument -> do
    argument <- delayed supply argument
    evaluate supply function (Apply [argument] consumer)
  Instance global arguments ->
    values supply arguments $ \arguments ->
      pure $ Cut (Global global) (Apply arguments consumer)
  Method index dictionary ->
    values supply [dictionary] $ \dictionary ->
      pure $ Primitive (Field index) dictionary (Force consumer)
  Super index dictionary ->
    values supply [dictionary] $ \dictionary ->
      pure $ Primitive (Field index) dictionary consumer
  Match scrutinee alternatives -> do
    alternatives <- sequence $ do
      (tag, names, body) <- alternatives
      pure $ Alternative tag (byNeed <$> names) <$> evaluate supply body consumer
    evaluate supply scrutinee (Case alternatives Nothing)

-- |
-- Evaluate terms and go on with their values.
values :: Supply s -> [Term] -> ([Producer] -> ST s Statement) -> ST s Statement
values supply terms continue = case terms of
  [] -> continue []
  Value name : rest -> values supply rest (continue . (variable name :))
  term : rest -> do
    name <- fresh supply "d"
    body <- values supply rest (continue . (Variable name :))
    evaluate supply term (MuTilde Binder {name, strategy = ByValue} body)

delayed :: Supply s -> Term -> ST s Producer
delayed supply = \case
  Lazy name -> pure $ variable name
  Value name -> pure $ variable name
  Helper name -> pure $ Global $ helperGlobal name
  Character character -> pure $ Literal $ Int $ fromIntegral $ ord character
  Zero -> pure $ Literal $ Int 0
  Construct tag fields -> Constructor tag <$> traverse (delayed supply) fields
  Lambda parameter body -> do
    result <- fresh supply "c"
    body <- evaluate supply body (Covariable result)
    pure $ Function [byNeed parameter] result body
  Given parameters body -> do
    result <- fresh supply "c"
    body <- evaluate supply body (Covariable result)
    pure $ Function (byValue <$> parameters) result body
  term -> do
    result <- fresh supply "c"
    body <- evaluate supply term (Covariable result)
    pure $ Thunk result body

-- |
-- An instance for the tuples of a size: a function of the evidence for each
-- element that gives the evidence for the tuple. The evidence is the
-- evidence for the superclasses and then the methods, which may use the
-- evidence itself under the name @self@.
instancex :: Int -> ([Term] -> ([Term], [Term])) -> Producer
instancex size make = runST (instancex' size make)

instancex' :: Int -> ([Term] -> ([Term], [Term])) -> ST s Producer
instancex' size make = do
  supply <- newSTRef 0
  let names = ["e" ++ show index | index <- [1 .. size]]
      (supers, methods) = make (Value <$> names)
  result <- fresh supply "c"
  self <- fresh supply "c"
  body <- values supply supers $ \supers -> do
    methods <- traverse (delayed supply) methods
    pure $ Cut (Constructor 0 (supers ++ methods)) (Covariable self)
  pure $
    Function (byValue <$> names) result $
      LetRec [(pack "self", Thunk self body)] $
        Cut (variable "self") (Force (Covariable result))

-- |
-- A default method, which is a definition in @Hazy.Helper@ that takes the
-- evidence.
defaultx :: String -> Term
defaultx name = Instance (helperGlobal name) [Lazy "self"]

fields :: String -> Int -> [String]
fields prefix size = [prefix ++ show index | index <- [1 .. size]]

-- |
-- Take two tuples apart.
both :: Int -> ([Term] -> [Term] -> Term) -> Term
both size body =
  Lambda "x" $
    Lambda "y" $
      Match
        (Lazy "x")
        [ ( 0,
            fields "x" size,
            Match
              (Lazy "y")
              [(0, fields "y" size, body (Lazy <$> fields "x" size) (Lazy <$> fields "y" size))]
          )
        ]

false, true, lessThen, equal, greaterThen :: Int
false = 0
true = 1
lessThen = 0
equal = 1
greaterThen = 2

list :: Term -> Term -> Term
list head tail = Construct cons [head, tail]

tuples :: Map Text (Int -> Producer)
tuples =
  Map.fromList
    [ (pack "eqTuple", eqTuple),
      (pack "ordTuple", ordTuple),
      (pack "semigroupTuple", semigroupTuple),
      (pack "monoidTuple", monoidTuple),
      (pack "boundedTuple", boundedTuple),
      (pack "showTuple", showTuple),
      (pack "readTuple", readTuple)
    ]

tupleGlobal :: String -> Int -> Global
tupleGlobal name size =
  Global.Global
    { path = hazy,
      name = pack name <> pack " " <> pack (show size)
    }

eqTuple :: Int -> Producer
eqTuple size = instancex size $ \evidence ->
  let go = \case
        [] -> Construct true []
        (element, left, right) : rest ->
          Match
            (Method 0 element :@ left :@ right)
            [ (false, [], Construct false []),
              (true, [], go rest)
            ]
   in ( [],
        [ both size $ \left right -> go (zip3 evidence left right),
          defaultx "defaultNotEqual"
        ]
      )

ordTuple :: Int -> Producer
ordTuple size = instancex size $ \evidence ->
  let go = \case
        [] -> Construct equal []
        (element, left, right) : rest ->
          Match
            (Method 1 element :@ left :@ right)
            [ (lessThen, [], Construct lessThen []),
              (greaterThen, [], Construct greaterThen []),
              (equal, [], go rest)
            ]
   in ( [Instance (tupleGlobal "eqTuple" size) (Super 0 <$> evidence)],
        [ both size $ \left right -> go (zip3 evidence left right),
          defaultx "defaultLessThen",
          defaultx "defaultLessThenEqual",
          defaultx "defaultGreaterThen",
          defaultx "defaultGreaterThenEqual",
          defaultx "defaultMax",
          defaultx "defaultMin"
        ]
      )

semigroupTuple :: Int -> Producer
semigroupTuple size = instancex size $ \evidence ->
  ( [],
    [ both size $ \left right ->
        Construct 0 $ do
          (element, left, right) <- zip3 evidence left right
          pure $ Method 0 element :@ left :@ right,
      defaultx "defaultSconcat",
      Given ["integral"] $
        Instance (helperGlobal "defaultStimes") [Lazy "self", Value "integral"]
    ]
  )

monoidTuple :: Int -> Producer
monoidTuple size = instancex size $ \evidence ->
  ( [Instance (tupleGlobal "semigroupTuple" size) (Super 0 <$> evidence)],
    [ Construct 0 (Method 1 <$> evidence),
      defaultx "defaultMappend",
      defaultx "defaultMconcat"
    ]
  )

boundedTuple :: Int -> Producer
boundedTuple size = instancex size $ \evidence ->
  ( [],
    [ Construct 0 (Method 0 <$> evidence),
      Construct 0 (Method 1 <$> evidence)
    ]
  )

showTuple :: Int -> Producer
showTuple size = instancex size $ \evidence ->
  let go = \case
        [] -> list (Character ')') (Lazy "string")
        [(element, field)] -> Method 0 element :@ Zero :@ field :@ go []
        (element, field) : rest ->
          Method 0 element :@ Zero :@ field :@ list (Character ',') (go rest)
   in ( [],
        [ Lambda "precedence" $
            Lambda "tuple" $
              Match
                (Lazy "tuple")
                [ ( 0,
                    fields "x" size,
                    Lambda "string" $
                      list (Character '(') (go (zip evidence (Lazy <$> fields "x" size)))
                  )
                ],
          defaultx "defaultShow",
          defaultx "defaultShowList"
        ]
      )

readTuple :: Int -> Producer
readTuple size = instancex size $ \evidence ->
  let bind parsed name rest next =
        Helper "bindRead" :@ parsed :@ Lambda name (Lambda rest next)
      lexeme character string = Helper "minilexSingle" :@ Character character :@ Lazy string
      string index = "s" ++ show (index :: Int)
      close index =
        bind (lexeme ')' (string index)) "close" "rest" $
          list
            (Construct 0 [Construct 0 (Lazy <$> fields "x" size), Lazy "rest"])
            (Construct nil [])
      go index = \case
        [] -> close index
        element : rest ->
          bind
            (Method 0 element :@ Zero :@ Lazy (string index))
            ("x" ++ show (index `div` 2 + 1))
            (string (index + 1))
            $ case rest of
              [] -> close (index + 1)
              _ ->
                bind (lexeme ',' (string (index + 1))) ("comma" ++ show index) (string (index + 2)) $
                  go (index + 2) rest
   in ( [],
        [ Lambda "precedence" $
            Lambda "string" $
              bind (lexeme '(' "string") "open" (string 0) $
                go 0 evidence,
          defaultx "defaultReadList"
        ]
      )
