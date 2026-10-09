-- |
-- AxCut to LLVM IR.
--
-- Every object's method becomes a function, and so does every definition.
-- AxCut's environment is a row of values that only the compiler sees: inside
-- a function each value is an LLVM value, and 'Substitute' costs nothing. A
-- function is entered with a tail call and never returns to its caller, so
-- the program runs in constant stack. The one function that returns is
-- whichever reaches 'Halt', and it returns to @main@.
--
-- A value is one 64-bit word: an odd word is an @Int@ or a @Char@, and an
-- even word is the address of a block whose first word is a header. The
-- layouts are written out in @runtime/native/src/lib.rs@, which has to agree
-- with what is here.
--
-- Nothing is freed yet. Counting references is a later milestone.
module LLVM.FromAxCut (Target (..), arm64, generate, generateBody, generateDefinitions) where

import AxCut.Tree
  ( Clause (..),
    Kind (..),
    Method (..),
    Object (..),
    Program (..),
    Recursive (..),
    Statement (..),
  )
import Control.Monad.ST (ST, runST)
import Data.Char (isAlphaNum, isAscii, ord)
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy.Text (unpack)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Traversable (for)
import Generate.Global (Global)
import Sequent.Primitive (Primitive (..))
import qualified Sequent.Primitive as Primitive
import qualified Sequent.Printer as Sequent (global)
import qualified Sequent.Tree as Literal (Literal (..))

-- |
-- What differs between the machines code is generated for.
data Target = Target
  { -- | How many arguments a call passes in registers. The rest go through
    -- a fixed area of memory: a call that guarantees a tail call cannot pass
    -- arguments on the stack on every machine.
    registers :: !Int
  }

arm64 :: Target
arm64 = Target {registers = 8}

-- |
-- A whole program, ready to be compiled.
generate :: Target -> Program -> Builder
generate target program = fromString prelude <> fromString "\n" <> generateBody target program

-- |
-- A program without what every program starts with, for reading.
generateBody :: Target -> Program -> Builder
generateBody target program = runST (generate' True target program)

-- |
-- The definitions of a program alone, for reading.
generateDefinitions :: Target -> Program -> Builder
generateDefinitions target program = runST (generate' False target program)

data State s = State
  { target :: !Target,
    -- | The next number for a name.
    counter :: !(STRef s Int),
    -- | The functions and constants made along the way.
    extra :: !(STRef s Builder)
  }

generate' :: Bool -> Target -> Program -> ST s Builder
generate' start target Program {definitions, entry} = do
  counter <- newSTRef 0
  extra <- newSTRef mempty
  let state = State {target, counter, extra}
  globals <- for (Map.toList definitions) $ \(global, object@Object {kind}) -> do
    function state (symbol global <> fromString ".code") (comment global) object
    pure $
      mconcat
        [ quoted (symbol global),
          fromString " = internal global { i64, ptr } { i64 ",
          number (kindOf kind),
          fromString ", ptr ",
          quoted (symbol global <> fromString ".code"),
          fromString " }, align 8\n"
        ]
  body <-
    if start
      then do
        body <- statement state [] entry
        pure $ fromString "\ndefine internal tailcc void @hazy_entry() {\n" <> body <> fromString "}\n"
      else pure mempty
  extra <- readSTRef extra
  pure $ mconcat [mconcat globals, body, extra]
  where
    comment global = fromString "; " <> Sequent.global global <> fromString "\n"

-- Block layouts -------------------------------------------------------------

dataKind, closureKind, thunkKind :: Integer
dataKind = 0
closureKind = 1
thunkKind = 2

kindOf :: Kind -> Integer
kindOf = \case
  Closure -> closureKind
  Thunk -> thunkKind

-- |
-- A header: the kind, a constructor's tag and the number of fields.
header :: Integer -> Int -> Int -> Integer
header kind tag count = kind + toInteger tag * 2 ^ (8 :: Int) + toInteger count * 2 ^ (32 :: Int)

-- |
-- An @Int@ or a @Char@ as a word.
immediate :: Integer -> Integer
immediate value = value * 2 + 1

-- Names ---------------------------------------------------------------------

number :: Integer -> Builder
number = fromString . show

-- |
-- The name of a definition, without the quotes a name needs.
symbol :: Global -> Builder
symbol global = fromString $ concatMap escape $ drop 1 $ Lazy.Text.unpack $ toLazyText $ Sequent.global global
  where
    escape character
      | isAscii character && (isAlphaNum character || character `elem` " .'_$-+*/<>=!&|^%#@~?:,()[]") =
          [character]
      | otherwise = concat ["\\" ++ hex byte | byte <- utf8 character]

hex :: Int -> String
hex byte = [digits !! (byte `div` 16), digits !! (byte `mod` 16)]
  where
    digits = "0123456789ABCDEF"

-- |
-- A character as the bytes of UTF-8.
utf8 :: Char -> [Int]
utf8 character
  | point < 0x80 = [point]
  | point < 0x800 = [0xC0 + point `div` 0x40, continuation point]
  | point < 0x10000 = [0xE0 + point `div` 0x1000, continuation (point `div` 0x40), continuation point]
  | otherwise =
      [ 0xF0 + point `div` 0x40000,
        continuation (point `div` 0x1000),
        continuation (point `div` 0x40),
        continuation point
      ]
  where
    point = ord character
    -- The low six bits, marked as a byte that carries on a character.
    continuation bits = 0x80 + bits `mod` 0x40

quoted :: Builder -> Builder
quoted name = fromString "@\"" <> name <> fromString "\""

fresh :: State s -> ST s Int
fresh State {counter} = do
  next <- readSTRef counter
  writeSTRef counter $! next + 1
  pure next

-- |
-- A new name for a value inside a function.
local :: State s -> ST s Builder
local state = do
  next <- fresh state
  pure $ fromString "%t" <> fromString (show next)

label :: State s -> ST s Builder
label state = do
  next <- fresh state
  pure $ fromString "L" <> fromString (show next)

line :: [Builder] -> Builder
line pieces = fromString "  " <> mconcat pieces <> fromString "\n"

commas :: [Builder] -> Builder
commas = mconcat . intersperse (fromString ", ")

-- Pieces of code ------------------------------------------------------------

-- |
-- The row of values: the name AxCut calls each by, and the LLVM value it is.
-- The last value is last.
type Environment = [(Text, Builder)]

splitEnd :: Int -> Environment -> (Environment, Environment)
splitEnd count environment
  | count > length environment = error "the environment is too short"
  | otherwise = splitAt (length environment - count) environment

-- |
-- A word as an address.
address :: State s -> Builder -> ST s (Builder, Builder)
address state value = do
  pointer <- local state
  pure (line [pointer, fromString " = inttoptr i64 ", value, fromString " to ptr"], pointer)

-- |
-- Where a word of a block is.
slot :: State s -> Builder -> Int -> ST s (Builder, Builder)
slot state pointer index = do
  place <- local state
  pure
    ( line
        [ place,
          fromString " = getelementptr i64, ptr ",
          pointer,
          fromString ", i64 ",
          fromString (show index)
        ],
      place
    )

load :: State s -> Builder -> Int -> ST s (Builder, Builder)
load state pointer index = do
  (code, place) <- slot state pointer index
  value <- local state
  pure (code <> line [value, fromString " = load i64, ptr ", place], value)

store :: State s -> Builder -> Int -> Builder -> ST s Builder
store state pointer index value = do
  (code, place) <- slot state pointer index
  pure $ code <> line [fromString "store i64 ", value, fromString ", ptr ", place]

-- |
-- A new block of some words, as an address.
allocate :: State s -> Int -> ST s (Builder, Builder)
allocate state words = do
  block <- local state
  pure (line [block, fromString " = call ptr @hazy_alloc(i64 ", fromString (show words), fromString ")"], block)

-- |
-- An address as a word.
word :: State s -> Builder -> ST s (Builder, Builder)
word state pointer = do
  value <- local state
  pure (line [value, fromString " = ptrtoint ptr ", pointer, fromString " to i64"], value)

-- |
-- Where an argument that is not passed in a register goes.
spill :: Int -> Builder
spill index =
  fromString "getelementptr ([64 x i64], ptr @hazy_spill, i64 0, i64 " <> fromString (show index) <> fromString ")"

-- |
-- Enter some code and do not come back. The first argument is the object the
-- code belongs to.
enter :: State s -> Builder -> [Builder] -> Builder
enter State {target = Target {registers}} code arguments
  | length spilled > 64 = error "too many arguments"
  | otherwise =
      mconcat
        [ mconcat $ do
            (index, argument) <- zip [0 ..] spilled
            pure $ line [fromString "store i64 ", argument, fromString ", ptr ", spill index],
          line
            [ fromString "musttail call tailcc void ",
              code,
              fromString "(",
              commas [fromString "i64 " <> argument | argument <- passed],
              fromString ")"
            ],
          line [fromString "ret void"]
        ]
  where
    (passed, spilled) = splitAt registers arguments

-- |
-- Enter the method of an object.
invoke :: State s -> Builder -> [Builder] -> ST s Builder
invoke state object arguments = do
  (code, pointer) <- address state object
  (code', place) <- slot state pointer 1
  method <- local state
  pure $
    mconcat
      [ code,
        code',
        line [method, fromString " = load ptr, ptr ", place],
        enter state method (object : arguments)
      ]

-- |
-- A function for an object's method. It is entered with the object and then
-- the method's parameters, and what the object captured is read out of it.
function :: State s -> Builder -> Builder -> Object -> ST s ()
function state@State {target = Target {registers}, extra} name comment Object {captured, parameters, body} = do
  self <- local state
  arguments <- for parameters $ \_ -> local state
  let (passed, spilled) = splitAt (registers - 1) arguments
      unspill = mconcat $ do
        (index, argument) <- zip [0 ..] spilled
        pure $ line [argument, fromString " = load i64, ptr ", spill index]
  (code, pointer) <- address state self
  captures <- for (zip [2 ..] captured) $ \(index, _) -> load state pointer index
  body <- statement state (zip parameters arguments ++ zip captured (map snd captures)) body
  modifySTRef' extra $ \known ->
    mconcat
      [ known,
        fromString "\n",
        comment,
        fromString "define internal tailcc void ",
        quoted name,
        fromString "(",
        commas [fromString "i64 " <> argument | argument <- self : passed],
        fromString ") {\n",
        unspill,
        if null captured then mempty else code <> foldMap fst captures,
        body,
        fromString "}\n"
      ]

-- |
-- A function for an object that is made as the program runs, and its name.
anonymous :: State s -> Object -> ST s Builder
anonymous state object@Object {kind} = do
  next <- fresh state
  let name = fromString (hint ++ "." ++ show next)
      hint = case kind of
        Closure -> "closure"
        Thunk -> "thunk"
  function state name mempty object
  pure name

-- |
-- Make an object, apart from what it captures.
object' :: State s -> Object -> ST s (Builder, Builder)
object' state object@Object {kind, captured} = do
  name <- anonymous state object
  (code, block) <- allocate state (2 + length captured)
  (code', place) <- slot state block 1
  pure
    ( mconcat
        [ code,
          line [fromString "store i64 ", number (kindOf kind), fromString ", ptr ", block],
          code',
          line [fromString "store ptr ", quoted name, fromString ", ptr ", place]
        ],
      block
    )

statement :: State s -> Environment -> Statement -> ST s Builder
statement state environment = \case
  Substitute pairs rest -> do
    let find old = case lookup old environment of
          Just value -> value
          Nothing -> error $ unpack old ++ " is not in the environment"
    statement state [(new, find old) | (new, old) <- pairs] rest
  Let name tag count rest -> do
    let (kept, taken) = splitEnd count environment
    (code, block) <- allocate state (1 + count)
    fields <- for (zip [1 ..] taken) $ \(index, (_, value)) -> store state block index value
    (code', value) <- word state block
    rest <- statement state (kept ++ [(name, value)]) rest
    pure $
      mconcat
        [ code,
          line [fromString "store i64 ", number (header dataKind tag count), fromString ", ptr ", block],
          mconcat fields,
          code',
          rest
        ]
  Switch clauses fallback -> do
    let (kept, taken) = splitEnd 1 environment
        scrutinee = snd (head taken)
    (code, pointer) <- address state scrutinee
    (code', head') <- load state pointer 0
    shifted <- local state
    tag <- local state
    otherwise' <- label state
    clauses <- for clauses $ \(Clause tag names body) -> do
      target <- label state
      fields <- for (zip [1 ..] names) $ \(index, _) -> load state pointer index
      body <- statement state (kept ++ zip names (map snd fields)) body
      pure (tag, target, foldMap fst fields <> body)
    fallback <- case fallback of
      Just body -> statement state kept body
      Nothing -> pure $ line [fromString "call void @hazy_abort()"] <> line [fromString "unreachable"]
    pure $
      mconcat
        [ code,
          code',
          line [shifted, fromString " = lshr i64 ", head', fromString ", 8"],
          line [tag, fromString " = and i64 ", shifted, fromString ", 16777215"],
          line
            [ fromString "switch i64 ",
              tag,
              fromString ", label %",
              otherwise',
              fromString " [",
              mconcat $ do
                (tag, target, _) <- clauses
                pure $ mconcat [fromString " i64 ", fromString (show tag), fromString ", label %", target],
              fromString " ]"
            ],
          mconcat $ do
            (_, target, body) <- clauses
            pure $ target <> fromString ":\n" <> body,
          otherwise' <> fromString ":\n",
          fallback
        ]
  New name object@Object {captured} rest -> do
    let (kept, taken) = splitEnd (length captured) environment
    (code, block) <- object' state object
    captures <- for (zip [2 ..] taken) $ \(index, (_, value)) -> store state block index value
    (code', value) <- word state block
    rest <- statement state (kept ++ [(name, value)]) rest
    pure $ mconcat [code, mconcat captures, code', rest]
  Invoke method -> case (method, map snd environment) of
    (Return, [value, consumer]) -> invoke state consumer [value]
    (Apply, values@(_ : _ : _)) -> invoke state (last values) (init values)
    _ -> error $ "bad invoke " ++ show method
  Force -> case map snd environment of
    [consumer, value] -> pure $ enter state (fromString "@hazy_force") [consumer, value]
    _ -> error "force with the wrong environment"
  Literal name literal rest -> do
    (code, value) <- case literal of
      Literal.Int int -> pure (mempty, number (immediate (toInteger int)))
      Literal.Integer integer
        | integer >= -(2 ^ (63 :: Int)) && integer < 2 ^ (63 :: Int) -> do
            value <- local state
            pure (line [value, fromString " = call i64 @hazy_integer(i64 ", number integer, fromString ")"], value)
        | otherwise -> do
            (constant, size) <- bytes state (show integer)
            value <- local state
            pure
              ( line
                  [ value,
                    fromString " = call i64 @hazy_integer_big(ptr ",
                    constant,
                    fromString ", i64 ",
                    fromString (show size),
                    fromString ")"
                  ],
                value
              )
      Literal.Text text -> do
        (constant, size) <- bytes state (Text.unpack text)
        value <- local state
        pure
          ( line
              [ value,
                fromString " = call i64 @hazy_text(ptr ",
                constant,
                fromString ", i64 ",
                fromString (show size),
                fromString ")"
              ],
            value
          )
    rest <- statement state (environment ++ [(name, value)]) rest
    pure $ code <> rest
  Global name global rest -> do
    let value = fromString "ptrtoint (ptr " <> quoted (symbol global) <> fromString " to i64)"
    statement state (environment ++ [(name, value)]) rest
  Primitive name primitive rest -> do
    let (kept, taken) = splitEnd (Primitive.arity primitive) environment
        arguments = map snd taken
    (code, value) <- case (primitive, arguments) of
      (Field index, [block]) -> do
        (code, pointer) <- address state block
        (code', value) <- load state pointer (1 + index)
        pure (code <> code', value)
      _ -> do
        value <- local state
        pure
          ( line
              [ value,
                fromString " = call i64 @",
                fromString (primitiveName primitive),
                fromString "(",
                commas [fromString "i64 " <> argument | argument <- arguments],
                fromString ")"
              ],
            value
          )
    rest <- statement state (kept ++ [(name, value)]) rest
    pure $ code <> rest
  Rec objects rest -> do
    let mine Recursive {object = Object {captured}, others} = length captured - length others
        (kept, taken) = splitEnd (sum (map mine objects)) environment
    made <- for objects $ \Recursive {name, object} -> do
      (code, block) <- object' state object
      (code', value) <- word state block
      pure (code <> code', (name, block, value))
    let new = [(name, value) | (_, (name, _, value)) <- made]
        fill _ [] = pure mempty
        fill taken ((recursive@Recursive {others}, (_, (_, block, _))) : rest) = do
          let (own, later) = splitAt (mine recursive) taken
              shared = do
                other <- others
                case lookup other new of
                  Just value -> [value]
                  Nothing -> error $ unpack other ++ " is not one of the objects"
          captures <- for (zip [2 ..] (map snd own ++ shared)) $ \(index, value) ->
            store state block index value
          rest <- fill later rest
          pure $ mconcat captures <> rest
    filled <- fill taken (zip objects made)
    rest <- statement state (kept ++ new) rest
    pure $ mconcat [foldMap fst made, filled, rest]
  Abort -> pure $ line [fromString "call void @hazy_abort()"] <> line [fromString "unreachable"]
  Halt -> pure $ line [fromString "ret void"]

-- |
-- Some text as a constant of the program, and how many bytes it is.
bytes :: State s -> String -> ST s (Builder, Int)
bytes state@State {extra} string = do
  next <- fresh state
  let name = fromString "@bytes." <> fromString (show next)
      encoded = concatMap utf8 string
      size = length encoded
  modifySTRef' extra $ \known ->
    mconcat
      [ known,
        name,
        fromString " = private unnamed_addr constant [",
        fromString (show size),
        fromString " x i8] c\"",
        fromString (concat ["\\" ++ hex byte | byte <- encoded]),
        fromString "\"\n"
      ]
  pure (name, size)

-- |
-- What the runtime calls a primitive.
primitiveName :: Primitive -> String
primitiveName = \case
  Field _ -> error "a field is read in place"
  Error -> "hazy_prim_error"
  PutStrLn -> "hazy_prim_put_str_ln"
  TextSnoc -> "hazy_prim_text_snoc"
  TextNull -> "hazy_prim_text_null"
  TextHead -> "hazy_prim_text_head"
  TextTail -> "hazy_prim_text_tail"
  GeneralCategory -> "hazy_prim_general_category"
  IntEqual -> "hazy_prim_int_equal"
  IntLessThenEqual -> "hazy_prim_int_less_then_equal"
  IntAdd -> "hazy_prim_int_add"
  IntMinus -> "hazy_prim_int_minus"
  IntMultiply -> "hazy_prim_int_multiply"
  IntNegate -> "hazy_prim_int_negate"
  IntAbs -> "hazy_prim_int_abs"
  IntSignum -> "hazy_prim_int_signum"
  IntQuot -> "hazy_prim_int_quot"
  IntRem -> "hazy_prim_int_rem"
  IntToInteger -> "hazy_prim_int_to_integer"
  IntegerCastToInt -> "hazy_prim_integer_cast_to_int"
  IntegerTruncateToInt -> "hazy_prim_integer_truncate_to_int"
  IntegerEqual -> "hazy_prim_integer_equal"
  IntegerLessThenEqual -> "hazy_prim_integer_less_then_equal"
  IntegerAdd -> "hazy_prim_integer_add"
  IntegerMinus -> "hazy_prim_integer_minus"
  IntegerMultiply -> "hazy_prim_integer_multiply"
  IntegerNegate -> "hazy_prim_integer_negate"
  IntegerAbs -> "hazy_prim_integer_abs"
  IntegerSignum -> "hazy_prim_integer_signum"
  IntegerQuot -> "hazy_prim_integer_quot"
  IntegerRem -> "hazy_prim_integer_rem"

-- |
-- What every program starts with: what the runtime provides, forcing, and
-- @main@.
--
-- Forcing is written here and not in the runtime because it ends in a tail
-- call into the program, which only LLVM can make.
prelude :: String
prelude =
  """
  declare ptr @hazy_alloc(i64)
  declare void @hazy_abort() noreturn
  declare void @hazy_loop() noreturn
  declare void @hazy_finish()
  declare i64 @hazy_text(ptr, i64)
  declare i64 @hazy_integer(i64)
  declare i64 @hazy_integer_big(ptr, i64)
  declare i64 @hazy_prim_error(i64)
  declare i64 @hazy_prim_put_str_ln(i64)
  declare i64 @hazy_prim_text_snoc(i64, i64)
  declare i64 @hazy_prim_text_null(i64)
  declare i64 @hazy_prim_text_head(i64)
  declare i64 @hazy_prim_text_tail(i64)
  declare i64 @hazy_prim_general_category(i64)
  declare i64 @hazy_prim_int_equal(i64, i64)
  declare i64 @hazy_prim_int_less_then_equal(i64, i64)
  declare i64 @hazy_prim_int_add(i64, i64)
  declare i64 @hazy_prim_int_minus(i64, i64)
  declare i64 @hazy_prim_int_multiply(i64, i64)
  declare i64 @hazy_prim_int_negate(i64)
  declare i64 @hazy_prim_int_abs(i64)
  declare i64 @hazy_prim_int_signum(i64)
  declare i64 @hazy_prim_int_quot(i64, i64)
  declare i64 @hazy_prim_int_rem(i64, i64)
  declare i64 @hazy_prim_int_to_integer(i64)
  declare i64 @hazy_prim_integer_cast_to_int(i64)
  declare i64 @hazy_prim_integer_truncate_to_int(i64)
  declare i64 @hazy_prim_integer_equal(i64, i64)
  declare i64 @hazy_prim_integer_less_then_equal(i64, i64)
  declare i64 @hazy_prim_integer_add(i64, i64)
  declare i64 @hazy_prim_integer_minus(i64, i64)
  declare i64 @hazy_prim_integer_multiply(i64, i64)
  declare i64 @hazy_prim_integer_negate(i64)
  declare i64 @hazy_prim_integer_abs(i64)
  declare i64 @hazy_prim_integer_signum(i64)
  declare i64 @hazy_prim_integer_quot(i64, i64)
  declare i64 @hazy_prim_integer_rem(i64, i64)

  ; Where the arguments that do not fit in registers are passed.
  @hazy_spill = internal global [64 x i64] zeroinitializer

  ; Give a value to a consumer.
  define internal tailcc void @hazy_return(i64 %consumer, i64 %value) {
    %pointer = inttoptr i64 %consumer to ptr
    %place = getelementptr i64, ptr %pointer, i64 1
    %code = load ptr, ptr %place
    musttail call tailcc void %code(i64 %consumer, i64 %value)
    ret void
  }

  ; Evaluate something that may be a thunk and give the value to a consumer.
  define internal tailcc void @hazy_force(i64 %consumer, i64 %value) {
    %odd = and i64 %value, 1
    %small = icmp ne i64 %odd, 0
    br i1 %small, label %evaluated, label %block
  block:
    %pointer = inttoptr i64 %value to ptr
    %header = load i64, ptr %pointer
    %kind = and i64 %header, 255
    switch i64 %kind, label %evaluated [ i64 2, label %delayed i64 3, label %working i64 4, label %done ]
  evaluated:
    musttail call tailcc void @hazy_return(i64 %consumer, i64 %value)
    ret void
  delayed:
    ; The thunk is being forced. Its body is given a consumer that keeps the
    ; value in the thunk and then hands it on.
    store i64 3, ptr %pointer
    %update = call ptr @hazy_alloc(i64 4)
    store i64 1, ptr %update
    %update.code = getelementptr i64, ptr %update, i64 1
    store ptr @hazy_update, ptr %update.code
    %update.thunk = getelementptr i64, ptr %update, i64 2
    store i64 %value, ptr %update.thunk
    %update.consumer = getelementptr i64, ptr %update, i64 3
    store i64 %consumer, ptr %update.consumer
    %updater = ptrtoint ptr %update to i64
    %place = getelementptr i64, ptr %pointer, i64 1
    %code = load ptr, ptr %place
    musttail call tailcc void %code(i64 %value, i64 %updater)
    ret void
  working:
    call void @hazy_loop()
    unreachable
  done:
    %kept = getelementptr i64, ptr %pointer, i64 1
    %result = load i64, ptr %kept
    musttail call tailcc void @hazy_return(i64 %consumer, i64 %result)
    ret void
  }

  ; The consumer a thunk's body is given.
  define internal tailcc void @hazy_update(i64 %self, i64 %value) {
    %pointer = inttoptr i64 %self to ptr
    %thunk.place = getelementptr i64, ptr %pointer, i64 2
    %thunk = load i64, ptr %thunk.place
    %consumer.place = getelementptr i64, ptr %pointer, i64 3
    %consumer = load i64, ptr %consumer.place
    %block = inttoptr i64 %thunk to ptr
    store i64 4, ptr %block
    %kept = getelementptr i64, ptr %block, i64 1
    store i64 %value, ptr %kept
    musttail call tailcc void @hazy_return(i64 %consumer, i64 %value)
    ret void
  }

  define i32 @main() {
    call tailcc void @hazy_entry()
    call void @hazy_finish()
    ret i32 0
  }
  """
