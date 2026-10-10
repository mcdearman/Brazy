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
module LLVM.FromAxCut
  ( Target (..),
    arm64,
    withoutReuse,
    generate,
    generateBody,
    generateDefinitions,
    Unit (..),
    generateUnit,
  )
where

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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy.Text (Text, unpack)
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
    registers :: !Int,
    -- | Whether a constructed value is built in the place of one that was
    -- just taken apart. This is here so that it can be turned off, to see
    -- what it is worth.
    reuse :: !Bool
  }

arm64 :: Target
arm64 = Target {registers = 8, reuse = True}

-- |
-- A target that never builds a value in the place of another.
withoutReuse :: Target -> Target
withoutReuse target = target {reuse = False}

-- |
-- A whole program, ready to be compiled. Nothing in it is seen from outside.
generate :: Target -> Program -> Builder
generate target program = fromString (prelude Whole) <> fromString "\n" <> generateBody target program

-- |
-- A program without what every program starts with, for reading.
generateBody :: Target -> Program -> Builder
generateBody target Program {definitions, entry} =
  runST (generate' Whole target definitions Map.empty (Just entry))

-- |
-- The definitions of a program alone, for reading.
generateDefinitions :: Target -> Program -> Builder
generateDefinitions target Program {definitions} =
  runST (generate' Whole target definitions Map.empty Nothing)

-- |
-- Some definitions compiled by themselves, to be linked with others.
data Unit = Unit
  { -- | The definitions that are this unit's and no other's.
    defined :: Map Global Object,
    -- | The definitions that other units may have made as well. The linker
    -- keeps one of each.
    shared :: Map Global Object,
    -- | Where the program starts, if this unit is the program and not a
    -- package.
    start :: Maybe Statement
  }

-- |
-- A unit, ready to be compiled. What it uses of other units is declared and
-- left to the linker.
generateUnit :: Target -> Unit -> Builder
generateUnit target Unit {defined, shared, start} =
  fromString (prelude shape) <> fromString "\n" <> runST (generate' shape target defined shared start)
  where
    shape = case start of
      Just _ -> Main
      Nothing -> Package

-- |
-- How what is generated meets the rest of a program.
data Shape
  = -- | It is the whole program.
    Whole
  | -- | It is the program's own modules, linked with packages.
    Main
  | -- | It is a package.
    Package
  deriving (Eq)

data State s = State
  { target :: !Target,
    -- | The next number for a name.
    counter :: !(STRef s Int),
    -- | The functions and constants made along the way.
    extra :: !(STRef s Builder),
    -- | The definitions that have been mentioned.
    mentioned :: !(STRef s (Set Global)),
    -- | The whole path to the code being generated: the definition it is
    -- in, and then every object inside that which has been gone into. What
    -- is generated is called by it.
    scope :: !Builder,
    -- | The names that have been given to functions.
    taken :: !(STRef s (Set Lazy.Text.Text))
  }

generate' ::
  Shape ->
  Target ->
  Map Global Object ->
  Map Global Object ->
  Maybe Statement ->
  ST s Builder
generate' shape target defined shared start = do
  counter <- newSTRef 0
  extra <- newSTRef mempty
  mentioned <- newSTRef Set.empty
  taken <- newSTRef Set.empty
  let state = State {target, counter, extra, mentioned, scope = fromString "entry", taken}
      global linkage (name, object@Object {kind}) = do
        function state {scope = symbol name} (symbol name <> fromString ".code") (comment name) Nothing object
        pure $
          mconcat
            [ quoted (symbol name),
              fromString " = ",
              fromString linkage,
              fromString "global { i64, ptr } { i64 ",
              number (kindOf kind + immortal),
              fromString ", ptr ",
              quoted (symbol name <> fromString ".code"),
              fromString " }, align 8\n"
            ]
  globals <- traverse (global (if shape == Whole then "internal " else "")) (Map.toList defined)
  common <- traverse (global "linkonce_odr ") (Map.toList shared)
  body <- case start of
    Just entry -> do
      body <- statement state Nothing [] entry
      pure $ fromString "\ndefine internal tailcc void @hazy_entry() {\n" <> body <> fromString "}\n"
    Nothing -> pure mempty
  extra <- readSTRef extra
  mentioned <- readSTRef mentioned
  -- What is used and is not here is some other unit's.
  let elsewhere = do
        name <- Set.toList mentioned
        if Map.member name defined || Map.member name shared
          then []
          else [quoted (symbol name) <> fromString " = external global { i64, ptr }\n"]
  pure $ mconcat [mconcat elsewhere, mconcat globals, mconcat common, body, extra]
  where
    comment global = fromString "; " <> Sequent.global global <> fromString "\n"

-- Block layouts -------------------------------------------------------------

dataKind, closureKind, thunkKind, huskKind :: Integer
dataKind = 0
closureKind = 1
thunkKind = 2
huskKind = 7

kindOf :: Kind -> Integer
kindOf = \case
  Closure -> closureKind
  Thunk -> thunkKind

-- |
-- A header: the kind, a constructor's tag, and how many fields or captured
-- values there are. The upper half of the word is how many references there
-- are to the block, which is none yet.
header :: Integer -> Int -> Int -> Integer
header kind tag count
  | tag >= limit = error "a constructor's tag is too big for a header"
  | count >= limit = error "a block has too many fields for a header"
  | otherwise = kind + toInteger tag * 2 ^ (6 :: Int) + toInteger count * 2 ^ (19 :: Int)

-- |
-- The most tags and the most fields there can be.
limit :: Int
limit = 8192

-- |
-- One reference, as it is added to a header.
one :: Integer
one = 2 ^ (32 :: Int)

-- |
-- The bit of a header that says a block is not counted: a definition at the
-- top. The header is a signed number to LLVM, so the bit makes it negative.
immortal :: Integer
immortal = -(2 ^ (63 :: Int))

-- |
-- Where things are in a block, after its header: the fields of a constructed
-- value, the code of an object, and what an object captured.
fieldsAt, codeAt, capturesAt :: Int
fieldsAt = 1
codeAt = 1
capturesAt = 2

-- |
-- How many values a function can hand over when a collection is due.
rootLimit :: Int
rootLimit = 1024

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
symbol global = fromString $ escaped $ drop 1 $ Lazy.Text.unpack $ toLazyText $ Sequent.global global

-- |
-- Some text as it can stand inside the quotes of a name.
escaped :: String -> String
escaped = concatMap escape
  where
    escape character
      | isAscii character && (isAlphaNum character || character `elem` " .'_$-+*/<>=!&|^%#@~?:,()[]") =
          [character]
      | otherwise = concat ["\\" ++ hex byte | byte <- utf8 character]

-- |
-- A name for the code of an object, given what AxCut calls the object: the
-- path to where it is, and then its own name. If the same object is
-- compiled twice, the second is told apart by a number.
named :: State s -> Text -> ST s Builder
named state@State {scope, taken} hint = do
  let wanted = scope <> fromString "." <> fromString (escaped (unpack hint))
  known <- readSTRef taken
  name <-
    if Set.member (toLazyText wanted) known
      then do
        next <- fresh state
        pure $ wanted <> fromString "." <> fromString (show next)
      else pure wanted
  modifySTRef' taken (Set.insert (toLazyText name))
  pure name

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

-- |
-- A new name for a value that is known to be a number that fits in a word,
-- and so is never counted.
localWord :: State s -> ST s Builder
localWord state = do
  next <- fresh state
  pure $ fromString "%w" <> fromString (show next)

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
-- The last value is last. Each is a reference the function owns.
type Environment = [(Text, Builder)]

splitEnd :: Int -> Environment -> (Environment, Environment)
splitEnd count environment
  | count > length environment = error "the environment is too short"
  | otherwise = splitAt (length environment - count) environment

-- |
-- Whether a value may be a block that is counted. A number, a constructor
-- with no fields and a definition at the top are not.
counted :: Builder -> Bool
counted value = take 2 (Lazy.Text.unpack (toLazyText value)) == "%t"

-- |
-- Say that there are more references to a value, or fewer.
share, erase :: Builder -> Builder
share value
  | counted value = line [fromString "call void @hazy_share(i64 ", value, fromString ")"]
  | otherwise = mempty
erase value
  | counted value = line [fromString "call void @hazy_erase(i64 ", value, fromString ")"]
  | otherwise = mempty

-- |
-- Give up a reference to a block whose contents have been read out and are
-- now owned.
open :: Builder -> Builder
open value
  | counted value = line [fromString "call void @hazy_open(i64 ", value, fromString ")"]
  | otherwise = mempty

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
-- A new block of some words with a header, as an address. There is one
-- reference to it.
allocate :: State s -> Int -> Integer -> ST s (Builder, Builder)
allocate state words head = do
  block <- local state
  pure
    ( line
        [ block,
          fromString " = call ptr @hazy_alloc(i64 ",
          fromString (show words),
          fromString ", i64 ",
          number head,
          fromString ")"
        ],
      block
    )

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
  (code', place) <- slot state pointer codeAt
  method <- local state
  pure $
    mconcat
      [ code,
        code',
        line [method, fromString " = load ptr, ptr ", place],
        enter state method (object : arguments)
      ]

-- |
-- If a collection is due, hand over everything the function holds. This is
-- at the start of a function, where that is what it was given: nothing else
-- in the program is alive but definitions at the top.
safepoint :: State s -> [Builder] -> ST s Builder
safepoint state roots
  | null roots = pure mempty
  | length roots > rootLimit = error "a function holds too many values"
  | otherwise = do
      due <- local state
      test <- local state
      collect <- label state
      go <- label state
      pure $
        mconcat
          [ line [due, fromString " = load i64, ptr @hazy_gc_pending"],
            line [test, fromString " = icmp ne i64 ", due, fromString ", 0"],
            line [fromString "br i1 ", test, fromString ", label %", collect, fromString ", label %", go],
            collect <> fromString ":\n",
            mconcat $ do
              (index, root) <- zip [0 :: Int ..] roots
              pure $
                line
                  [ fromString "store i64 ",
                    root,
                    fromString ", ptr getelementptr ([1024 x i64], ptr @hazy_roots, i64 0, i64 ",
                    fromString (show index),
                    fromString ")"
                  ],
            line
              [ fromString "call void @hazy_collect(ptr @hazy_roots, i64 ",
                fromString (show (length roots)),
                fromString ")"
              ],
            line [fromString "br label %", go],
            go <> fromString ":\n"
          ]

-- |
-- A function for an object's method. It is entered with the object and then
-- the method's parameters, which it owns.
--
-- What the object captured is read out of it. A closure may be entered many
-- times, so what it captured is shared, unless this was the last reference
-- to it, when the closure is freed and what it captured is simply taken. A
-- thunk is entered once, and what it captured is taken; the thunk itself
-- belongs to whoever is forcing it.
--
-- An object may also have been given the block of a value that was taken
-- apart, to build a value of as many fields in: see 'Token'. It is held
-- after what the object captured.
function :: State s -> Builder -> Builder -> Maybe Int -> Object -> ST s ()
function state@State {target = Target {registers}, extra} name comment husk object = do
  let Object {kind, captured, parameters, body} = object
  self <- local state
  arguments <- for parameters $ \_ -> local state
  let (passed, spilled) = splitAt (registers - 1) arguments
      unspill = mconcat $ do
        (index, argument) <- zip [0 ..] spilled
        pure $ line [argument, fromString " = load i64, ptr ", spill index]
  (code, pointer) <- address state self
  captures <- for (zip [capturesAt ..] captured) $ \(index, _) -> load state pointer index
  let taken = case kind of
        Closure -> open self
        Thunk -> mempty
  (carrying, carried, token) <- case husk of
    Nothing -> pure (mempty, [], Nothing)
    Just size -> do
      (loading, held) <- load state pointer (capturesAt + length captured)
      (code, block) <- unpacked state held
      pure (loading, [(held, code)], Just (block, size))
  check <- safepoint state (arguments ++ map snd captures ++ map fst carried)
  body <- statement state token (zip parameters arguments ++ zip captured (map snd captures)) body
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
        if null captured && null carried then mempty else code <> foldMap fst captures <> carrying,
        taken,
        check,
        foldMap snd carried,
        body,
        fromString "}\n"
      ]

-- |
-- A function for an object that is made as the program runs, and its name,
-- given what AxCut calls the object.
anonymous :: State s -> Text -> Maybe Int -> Object -> ST s Builder
anonymous state hint husk object = do
  name <- named state hint
  function state {scope = name} name mempty husk object
  pure name

-- |
-- Make an object, apart from what it captures.
object' :: State s -> Text -> Maybe Int -> Object -> ST s (Builder, Builder)
object' state hint husk object@Object {kind, captured} = do
  name <- anonymous state hint husk object
  let held = length captured + length husk
  (code, block) <- allocate state (capturesAt + held) (header (kindOf kind) 0 held)
  (code', place) <- slot state block codeAt
  pure
    ( mconcat
        [ code,
          code',
          line [fromString "store ptr ", quoted name, fromString ", ptr ", place]
        ],
      block
    )

-- |
-- Make the environment again. This is where values are shared and erased: a
-- value that is wanted more times than it is held is shared, and one that
-- is held more times than it is wanted is erased.
substitute :: Environment -> [(Text, Text)] -> (Builder, Environment)
substitute environment pairs = (adjust, [(new, find old) | (new, old) <- pairs])
  where
    held = Map.fromListWith (+) [(name, 1 :: Int) | (name, _) <- environment]
    wanted = Map.fromListWith (+) [(old, 1 :: Int) | (_, old) <- pairs]
    find old = case lookup old environment of
      Just value -> value
      Nothing -> error $ unpack old ++ " is not in the environment"
    adjust = mconcat $ do
      (name, have) <- Map.toList held
      let want = Map.findWithDefault 0 name wanted
          value = find name
      if want >= have
        then replicate (want - have) (share value)
        else replicate (have - want) (erase value)

-- |
-- Whether a value is known, where the program is compiled, to be a number
-- that fits in a word.
isWord :: Builder -> Bool
isWord value = case Lazy.Text.unpack (toLazyText value) of
  '%' : 'w' : _ -> True
  first : _ -> first `elem` "-0123456789"
  [] -> False

-- |
-- A consumer that is made only to be given the value of something that is
-- forced, as two functions: one that is entered with the value and what
-- the consumer captured, and one for the consumer as an object, which
-- reads what it captured and enters the first. When what is forced turns
-- out to be a value already, the first is entered at once and the consumer
-- is never made.
continuation :: State s -> Text -> Object -> ST s (Builder, Builder)
continuation outer@State {target = Target {registers}, extra} hint Object {captured, parameters, body} = do
  entry <- named outer hint
  let direct = entry <> fromString ".direct"
      names = parameters ++ captured
      state = outer {scope = entry}
  arguments <- for names $ \_ -> local state
  let (passed, spilled) = splitAt registers arguments
      unspill = mconcat $ do
        (index, argument) <- zip [0 ..] spilled
        pure $ line [argument, fromString " = load i64, ptr ", spill index]
  check <- safepoint state arguments
  body <- statement state Nothing (zip names arguments) body
  self <- local state
  given <- for parameters $ \_ -> local state
  (code, pointer) <- address state self
  captures <- for (zip [capturesAt ..] captured) $ \(index, _) -> load state pointer index
  modifySTRef' extra $ \known ->
    mconcat
      [ known,
        fromString "\ndefine internal tailcc void ",
        quoted direct,
        fromString "(",
        commas [fromString "i64 " <> argument | argument <- passed],
        fromString ") {\n",
        unspill,
        check,
        body,
        fromString "}\n",
        fromString "\ndefine internal tailcc void ",
        quoted entry,
        fromString "(",
        commas [fromString "i64 " <> argument | argument <- self : given],
        fromString ") {\n",
        if null captured then mempty else code <> foldMap fst captures,
        open self,
        enter state (quoted direct) (given ++ map snd captures),
        fromString "}\n"
      ]
  pure (entry, direct)

-- |
-- The block of a value that was just taken apart, and how many fields it
-- had, when the value was the only reference to it: a constructed value
-- with as many fields is built in its place. The block is zero when there
-- were other references.
--
-- The value may be built at once, or after control divides, or by a
-- consumer that is made here and entered later, when something that was
-- called returns: then the block goes with the consumer, as one more thing
-- it holds. So @insert@ into a tree builds each node on the way back up in
-- the block of the one it took apart on the way down. See 'wants'.
--
-- While it waits the block is a husk: a block that holds nothing, which can
-- be given up like any other if no value comes to be built in it.
type Token = Maybe (Builder, Int)

-- |
-- Whether a constructed value with some number of fields may be built by
-- what a statement goes on to do, so that a block is worth keeping back
-- for it.
wants :: Int -> Statement -> Bool
wants size = \case
  Substitute _ rest -> wants size rest
  Let _ _ count rest -> count == size || wants size rest
  New _ object rest -> wants size rest || carries size object rest
  Literal _ _ rest -> wants size rest
  Global _ _ rest -> wants size rest
  Primitive _ _ rest -> wants size rest
  Rec _ rest -> wants size rest
  Switch clauses fallback -> or [wants size body | Clause _ _ body <- clauses] || any (wants size) fallback
  Invoke {} -> False
  Force -> False
  Abort -> False
  Halt -> False

-- |
-- Whether a block can go with an object to where a value is built in it:
-- the object is a consumer, which is entered once, and it builds one. A
-- consumer of what is forced is made differently and is left out.
carries :: Int -> Object -> Statement -> Bool
carries size Object {kind = Closure, method = Return, body} rest = case rest of
  Substitute _ Force -> False
  _ -> wants size body
carries _ _ _ = False

-- |
-- A block that was kept back, as something an object can hold: itself, or
-- a number if there is none.
packed :: State s -> Builder -> ST s (Builder, Builder)
packed state block = do
  none <- local state
  held <- local state
  pure
    ( mconcat
        [ line [none, fromString " = icmp eq i64 ", block, fromString ", 0"],
          line [held, fromString " = select i1 ", none, fromString ", i64 1, i64 ", block]
        ],
      held
    )

-- |
-- What an object held as a block that was kept back, or zero. A consumer is
-- entered once, but nothing here knows that it is: if the block has come to
-- be shared, it is not used.
unpacked :: State s -> Builder -> ST s (Builder, Builder)
unpacked state held = do
  odd' <- local state
  small <- local state
  (pointing, pointer) <- address state held
  (loading, head') <- load state pointer 0
  references <- local state
  only <- local state
  boxed <- label state
  shared <- label state
  none <- label state
  joined <- label state
  block <- local state
  pure
    ( mconcat
        [ line [odd', fromString " = and i64 ", held, fromString ", 1"],
          line [small, fromString " = icmp ne i64 ", odd', fromString ", 0"],
          line [fromString "br i1 ", small, fromString ", label %", none, fromString ", label %", boxed],
          boxed <> fromString ":\n",
          pointing,
          loading,
          line [references, fromString " = lshr i64 ", head', fromString ", 32"],
          line [only, fromString " = icmp eq i64 ", references, fromString ", 1"],
          line [fromString "br i1 ", only, fromString ", label %", joined, fromString ", label %", shared],
          shared <> fromString ":\n",
          erase held,
          line [fromString "br label %", none],
          none <> fromString ":\n",
          line [fromString "br label %", joined],
          joined <> fromString ":\n",
          line
            [ block,
              fromString " = phi i64 [ ",
              held,
              fromString ", %",
              boxed,
              fromString " ], [ 0, %",
              none,
              fromString " ]"
            ]
        ],
      block
    )

-- |
-- Give up a block that was kept back and that nothing will be built in.
discard :: State s -> Builder -> ST s Builder
discard state block = do
  (code, held) <- packed state block
  pure $ code <> erase held

statement :: State s -> Token -> Environment -> Statement -> ST s Builder
statement state (Just (block, size)) environment body
  | not (wants size body) = do
      code <- discard state block
      rest <- statement state Nothing environment body
      pure $ code <> rest
statement state token environment body = wanted state token environment body

-- |
-- A statement, when the block that was kept back, if there is one, is
-- wanted by it.
wanted :: State s -> Token -> Environment -> Statement -> ST s Builder
wanted state token environment = \case
  Substitute pairs rest -> do
    let (adjust, after) = substitute environment pairs
    rest <- statement state token after rest
    pure $ adjust <> rest
  -- A constructor with no fields is not made: there is one of each already.
  Let name tag 0 rest
    | tag < limit -> do
        let value =
              mconcat
                [ fromString "ptrtoint (ptr getelementptr ([8192 x i64], ptr @hazy_nullary, i64 0, i64 ",
                  fromString (show tag),
                  fromString ") to i64)"
                ]
        statement state token (environment ++ [(name, value)]) rest
  Let name tag count rest -> do
    let (kept, taken) = splitEnd count environment
        head = header dataKind tag count
    (code, block, token) <- case token of
      -- Build in the block that was kept back, if there is one.
      Just (old, size) | size == count -> do
        (fresh', block') <- allocate state (fieldsAt + count) head
        none <- local state
        made <- label state
        kept' <- label state
        joined <- label state
        place <- local state
        reused <- local state
        more <- local state
        block <- local state
        pure
          ( mconcat
              [ line [none, fromString " = icmp eq i64 ", old, fromString ", 0"],
                line [fromString "br i1 ", none, fromString ", label %", made, fromString ", label %", kept'],
                made <> fromString ":\n",
                fresh',
                line [fromString "br label %", joined],
                kept' <> fromString ":\n",
                line [place, fromString " = inttoptr i64 ", old, fromString " to ptr"],
                line [fromString "store i64 ", number (head + one), fromString ", ptr ", place],
                line [reused, fromString " = load i64, ptr @hazy_reused"],
                line [more, fromString " = add i64 ", reused, fromString ", 1"],
                line [fromString "store i64 ", more, fromString ", ptr @hazy_reused"],
                line [fromString "br label %", joined],
                joined <> fromString ":\n",
                line
                  [ block,
                    fromString " = phi ptr [ ",
                    block',
                    fromString ", %",
                    made,
                    fromString " ], [ ",
                    place,
                    fromString ", %",
                    kept',
                    fromString " ]"
                  ]
              ],
            block,
            Nothing
          )
      _ -> do
        (code, block) <- allocate state (fieldsAt + count) head
        pure (code, block, token)
    fields <- for (zip [fieldsAt ..] taken) $ \(index, (_, value)) -> store state block index value
    (code', value) <- word state block
    rest <- statement state token (kept ++ [(name, value)]) rest
    pure $ mconcat [code, mconcat fields, code', rest]
  Switch clauses fallback -> do
    let (kept, taken) = splitEnd 1 environment
        scrutinee = snd (head taken)
    (code, pointer) <- address state scrutinee
    (code', head') <- load state pointer 0
    shifted <- local state
    tag <- local state
    otherwise' <- label state
    let reusing = reuse (target state)
    clauses <- for clauses $ \(Clause tag names body) -> do
      target <- label state
      fields <- for (zip [fieldsAt ..] names) $ \(index, _) -> load state pointer index
      let size = length names
      -- The fields are read, and then the value they were in is given up.
      -- If a value of the same size is built next, its block is kept back
      -- for that, when this was the only reference to it.
      -- A block that is kept back already goes on, if this clause wants it.
      (dropped, incoming) <- case token of
        Just (old, wanted) | not (wants wanted body) -> do
          code <- discard state old
          pure (code, Nothing)
        _ -> pure (mempty, token)
      (given, token) <- case incoming of
        Just _ -> pure (open scrutinee, incoming)
        Nothing -> if reusing && size > 0 && counted scrutinee && wants size body
          then do
            held <- local state
            only <- local state
            last' <- label state
            shared <- label state
            joined <- label state
            token <- local state
            pure
              ( mconcat
                  [ line [held, fromString " = lshr i64 ", head', fromString ", 32"],
                    line [only, fromString " = icmp eq i64 ", held, fromString ", 1"],
                    line [fromString "br i1 ", only, fromString ", label %", last', fromString ", label %", shared],
                    shared <> fromString ":\n",
                    line [fromString "call void @hazy_split(i64 ", scrutinee, fromString ")"],
                    line [fromString "br label %", joined],
                    last' <> fromString ":\n",
                    line [fromString "store i64 ", number (header huskKind 0 size + one), fromString ", ptr ", pointer],
                    line [fromString "br label %", joined],
                    joined <> fromString ":\n",
                    line
                      [ token,
                        fromString " = phi i64 [ ",
                        scrutinee,
                        fromString ", %",
                        last',
                        fromString " ], [ 0, %",
                        shared,
                        fromString " ]"
                      ]
                  ],
                Just (token, size)
              )
          else pure (open scrutinee, Nothing)
      body <- statement state token (kept ++ zip names (map snd fields)) body
      pure (tag, target, foldMap fst fields <> dropped <> given <> body)
    fallback <- case fallback of
      Just body -> do
        body <- statement state token kept body
        pure $ erase scrutinee <> body
      Nothing -> pure $ line [fromString "call void @hazy_abort()"] <> line [fromString "unreachable"]
    pure $
      mconcat
        [ code,
          code',
          line [shifted, fromString " = lshr i64 ", head', fromString ", 6"],
          line [tag, fromString " = and i64 ", shifted, fromString ", 8191"],
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
  -- A consumer made only to be given what something evaluates to. If the
  -- thing is a value already, the consumer's code is entered with it and
  -- the consumer is never made.
  New name object@Object {kind = Closure, method = Return, captured, parameters = [_]} (Substitute pairs Force)
    | (kept, taken) <- splitEnd (length captured) environment,
      (adjust, [(_, consumer), (_, value)]) <- substitute (kept ++ [(name, unmade)]) pairs,
      toLazyText consumer == toLazyText unmade -> do
        (entry, direct) <- continuation state name object
        let captures = map snd taken
            fast = enter state (quoted direct) (value : captures)
        if isWord value
          then pure $ adjust <> fast
          else do
            (code, block) <- allocate state (capturesAt + length captured) (header closureKind 0 (length captured))
            (code', place) <- slot state block codeAt
            stores <- for (zip [capturesAt ..] captures) $ \(index, capture) -> store state block index capture
            (code'', made) <- word state block
            odd' <- local state
            small <- local state
            (pointing, pointer) <- address state value
            (loading, head') <- load state pointer 0
            kind' <- local state
            offset <- local state
            thunk <- local state
            boxed <- label state
            quick <- label state
            slow <- label state
            pure $
              mconcat
                [ adjust,
                  line [odd', fromString " = and i64 ", value, fromString ", 1"],
                  line [small, fromString " = icmp ne i64 ", odd', fromString ", 0"],
                  line [fromString "br i1 ", small, fromString ", label %", quick, fromString ", label %", boxed],
                  boxed <> fromString ":\n",
                  pointing,
                  loading,
                  line [kind', fromString " = and i64 ", head', fromString ", 15"],
                  -- The three kinds of thunk are 2, 3 and 4.
                  line [offset, fromString " = sub i64 ", kind', fromString ", 2"],
                  line [thunk, fromString " = icmp ult i64 ", offset, fromString ", 3"],
                  line [fromString "br i1 ", thunk, fromString ", label %", slow, fromString ", label %", quick],
                  quick <> fromString ":\n",
                  fast,
                  slow <> fromString ":\n",
                  code,
                  code',
                  line [fromString "store ptr ", quoted entry, fromString ", ptr ", place],
                  mconcat stores,
                  code'',
                  enter state (fromString "@hazy_force") [made, value]
                ]
    where
      -- Stands for the consumer while the environment is worked out. It is
      -- never written.
      unmade = fromString "%unmade"
  -- A consumer that builds a value the block kept back can be used for: the
  -- block goes with it.
  New name object@Object {captured} rest
    | Just (husk, size) <- token,
      not (wants size rest),
      carries size object rest -> do
        let (kept, taken) = splitEnd (length captured) environment
        (code, block) <- object' state name (Just size) object
        captures <- for (zip [capturesAt ..] taken) $ \(index, (_, value)) -> store state block index value
        (packing, held) <- packed state husk
        carried <- store state block (capturesAt + length captured) held
        (code', value) <- word state block
        rest <- statement state Nothing (kept ++ [(name, value)]) rest
        pure $ mconcat [code, mconcat captures, packing, carried, code', rest]
  New name object@Object {captured} rest -> do
    let (kept, taken) = splitEnd (length captured) environment
    (code, block) <- object' state name Nothing object
    captures <- for (zip [capturesAt ..] taken) $ \(index, (_, value)) -> store state block index value
    (code', value) <- word state block
    rest <- statement state token (kept ++ [(name, value)]) rest
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
    rest <- statement state token (environment ++ [(name, value)]) rest
    pure $ code <> rest
  Global name global rest -> do
    modifySTRef' (mentioned state) (Set.insert global)
    let value = fromString "ptrtoint (ptr " <> quoted (symbol global) <> fromString " to i64)"
    statement state token (environment ++ [(name, value)]) rest
  -- A primitive borrows its arguments, which are given up afterwards.
  Primitive name primitive rest -> do
    let (kept, taken) = splitEnd (Primitive.arity primitive) environment
        arguments = map snd taken
    (code, value) <- case (primitive, arguments) of
      (Field index, [block]) -> do
        (code, pointer) <- address state block
        (code', value) <- load state pointer (fieldsAt + index)
        pure (code <> code' <> share value, value)
      _ -> do
        value <- if primitiveWord primitive then localWord state else local state
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
    rest <- statement state token (kept ++ [(name, value)]) rest
    pure $ code <> foldMap erase arguments <> rest
  Rec objects rest -> do
    let mine Recursive {object = Object {captured}, others} = length captured - length others
        (kept, taken) = splitEnd (sum (map mine objects)) environment
        -- An object is referred to once by its name, and once by each of
        -- the objects that capture it.
        references name = 1 + length [() | Recursive {others} <- objects, other <- others, other == name]
    made <- for objects $ \Recursive {name, object} -> do
      (code, block) <- object' state name Nothing object
      let Object {kind, captured} = object
          counted' = header (kindOf kind) 0 (length captured) + toInteger (references name) * one
      count <- store state block 0 (number counted')
      (code', value) <- word state block
      pure (code <> count <> code', (name, block, value))
    let new = [(name, value) | (_, (name, _, value)) <- made]
        fill _ [] = pure mempty
        fill taken ((recursive@Recursive {others}, (_, (_, block, _))) : rest) = do
          let (own, later) = splitAt (mine recursive) taken
              shared = do
                other <- others
                case lookup other new of
                  Just value -> [value]
                  Nothing -> error $ unpack other ++ " is not one of the objects"
          captures <- for (zip [capturesAt ..] (map snd own ++ shared)) $ \(index, value) ->
            store state block index value
          rest <- fill later rest
          pure $ mconcat captures <> rest
    filled <- fill taken (zip objects made)
    rest <- statement state token (kept ++ new) rest
    pure $ mconcat [foldMap fst made, filled, rest]
  Abort -> pure $ line [fromString "call void @hazy_abort()"] <> line [fromString "unreachable"]
  -- The program is over: whatever is still held is given up.
  Halt -> pure $ foldMap (erase . snd) environment <> line [fromString "ret void"]

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
-- Whether what a primitive gives back is always a number that fits in a
-- word.
primitiveWord :: Primitive -> Bool
primitiveWord = \case
  TextHead -> True
  IntAdd -> True
  IntMinus -> True
  IntMultiply -> True
  IntNegate -> True
  IntAbs -> True
  IntSignum -> True
  IntQuot -> True
  IntRem -> True
  IntegerCastToInt -> True
  IntegerTruncateToInt -> True
  _ -> False

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
-- What a unit starts with: what the runtime provides, and forcing.
--
-- Forcing is written here and not in the runtime because it ends in a tail
-- call into the program, which only LLVM can make. It is defined once, in
-- the unit that is the program, and a package only declares it.
prelude :: Shape -> String
prelude = \case
  Whole -> runtime ++ "\n" ++ areas "internal " ++ "\n" ++ forcing "internal " ++ "\n" ++ mainFunction
  Main -> runtime ++ "\n" ++ areas "" ++ "\n" ++ forcing "" ++ "\n" ++ mainFunction
  Package -> runtime ++ "\n" ++ declared

-- |
-- What the runtime provides, and counting, which every unit has for itself
-- so that it is done in place.
runtime :: String
runtime =
  """
  @hazy_gc_pending = external global i64
  @hazy_reused = external global i64
  @hazy_nullary = external global [8192 x i64]
  declare ptr @hazy_alloc(i64, i64)
  declare void @hazy_free(i64)
  declare void @hazy_give(i64)
  declare void @hazy_split(i64)
  declare void @hazy_settle(i64)
  declare void @hazy_collect(ptr, i64)
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

  ; There is one more reference to a value. A number is not counted, and
  ; neither is a block whose header is negative.
  define linkonce_odr void @hazy_share(i64 %value) alwaysinline {
    %odd = and i64 %value, 1
    %word = icmp ne i64 %odd, 0
    br i1 %word, label %out, label %block
  block:
    %pointer = inttoptr i64 %value to ptr
    %header = load i64, ptr %pointer
    %fixed = icmp slt i64 %header, 0
    br i1 %fixed, label %out, label %count
  count:
    %more = add i64 %header, 4294967296
    store i64 %more, ptr %pointer
    br label %out
  out:
    ret void
  }

  ; There is one reference fewer to a value. The runtime is told when that
  ; was the last.
  define linkonce_odr void @hazy_erase(i64 %value) alwaysinline {
    %odd = and i64 %value, 1
    %word = icmp ne i64 %odd, 0
    br i1 %word, label %out, label %block
  block:
    %pointer = inttoptr i64 %value to ptr
    %header = load i64, ptr %pointer
    %fixed = icmp slt i64 %header, 0
    br i1 %fixed, label %out, label %count
  count:
    %less = sub i64 %header, 4294967296
    store i64 %less, ptr %pointer
    %left = lshr i64 %less, 32
    %last = icmp eq i64 %left, 0
    br i1 %last, label %free, label %out
  free:
    call void @hazy_free(i64 %value)
    br label %out
  out:
    ret void
  }

  ; A reference to a block is given up by someone who has read what the
  ; block holds and now owns that. If it was the last reference only the
  ; block's words are freed. Otherwise what it holds is shared.
  define linkonce_odr void @hazy_open(i64 %value) alwaysinline {
    %odd = and i64 %value, 1
    %word = icmp ne i64 %odd, 0
    br i1 %word, label %out, label %block
  block:
    %pointer = inttoptr i64 %value to ptr
    %header = load i64, ptr %pointer
    %fixed = icmp slt i64 %header, 0
    br i1 %fixed, label %out, label %count
  count:
    %held = lshr i64 %header, 32
    %only = icmp eq i64 %held, 1
    br i1 %only, label %give, label %split
  give:
    call void @hazy_give(i64 %value)
    br label %out
  split:
    call void @hazy_split(i64 %value)
    br label %out
  out:
    ret void
  }
  """

-- |
-- The fixed areas of memory a program uses, given how they are seen from
-- other units: where the arguments that do not fit in registers are passed,
-- and where a function puts what it holds when a collection is due.
areas :: String -> String
areas linkage =
  concat
    [ "@hazy_spill = " ++ linkage ++ "global [64 x i64] zeroinitializer\n",
      "@hazy_roots = " ++ linkage ++ "global [1024 x i64] zeroinitializer\n"
    ]

-- |
-- Forcing, given how it is seen from other units.
forcing :: String -> String
forcing linkage =
  """
  ; Give a value to a consumer.
  define internal tailcc void @hazy_return(i64 %consumer, i64 %value) {
    %pointer = inttoptr i64 %consumer to ptr
    %place = getelementptr i64, ptr %pointer, i64 1
    %code = load ptr, ptr %place
    musttail call tailcc void %code(i64 %consumer, i64 %value)
    ret void
  }

  ; The consumer a thunk's body is given: the header of a closure that
  ; captures two values, its code, the thunk and the consumer that asked.
  ; It keeps the value in the thunk and then hands it on.
  define internal tailcc void @hazy_update(i64 %self, i64 %value) {
    %pointer = inttoptr i64 %self to ptr
    %thunk.place = getelementptr i64, ptr %pointer, i64 2
    %thunk = load i64, ptr %thunk.place
    %consumer.place = getelementptr i64, ptr %pointer, i64 3
    %consumer = load i64, ptr %consumer.place
    call void @hazy_open(i64 %self)
    %block = inttoptr i64 %thunk to ptr
    %header = load i64, ptr %block
    ; If this is the only reference to the thunk, nobody will ask for its
    ; value again: the thunk is freed and the value goes on unshared.
    %held = lshr i64 %header, 32
    %only = icmp eq i64 %held, 1
    br i1 %only, label %alone, label %keep
  alone:
    call void @hazy_give(i64 %thunk)
    musttail call tailcc void @hazy_return(i64 %consumer, i64 %value)
    ret void
  keep:
    ; The thunk holds the value, and so does the consumer that asked.
    call void @hazy_share(i64 %value)
    %cleared = and i64 %header, -16
    %done = or i64 %cleared, 4
    store i64 %done, ptr %block
    %kept = getelementptr i64, ptr %block, i64 1
    store i64 %value, ptr %kept
    call void @hazy_settle(i64 %thunk)
    musttail call tailcc void @hazy_return(i64 %consumer, i64 %value)
    ret void
  }

  ; Evaluate something that may be a thunk and give the value to a consumer.
  ; Both are given up.
  """
    ++ "\ndefine "
    ++ linkage
    ++ """
       tailcc void @hazy_force(i64 %consumer, i64 %value) {
         %odd = and i64 %value, 1
         %small = icmp ne i64 %odd, 0
         br i1 %small, label %evaluated, label %block
       block:
         %pointer = inttoptr i64 %value to ptr
         %header = load i64, ptr %pointer
         %kind = and i64 %header, 15
         switch i64 %kind, label %evaluated [ i64 2, label %delayed i64 3, label %working i64 4, label %done ]
       evaluated:
         musttail call tailcc void @hazy_return(i64 %consumer, i64 %value)
         ret void
       delayed:
         ; The thunk is being forced. Its body takes what it captured, and is
         ; given a consumer that holds the thunk and the consumer that asked.
         %cleared = and i64 %header, -16
         %busy = or i64 %cleared, 3
         store i64 %busy, ptr %pointer
         %update = call ptr @hazy_alloc(i64 4, i64 1048577)
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
         ; The value is read and the thunk given up. If that was the last
         ; reference to the thunk, the value is taken and not shared.
         %kept = getelementptr i64, ptr %pointer, i64 1
         %result = load i64, ptr %kept
         ; A definition at the top keeps its value for good.
         %fixed = icmp slt i64 %header, 0
         br i1 %fixed, label %lasting, label %counted
       lasting:
         call void @hazy_share(i64 %result)
         musttail call tailcc void @hazy_return(i64 %consumer, i64 %result)
         ret void
       counted:
         call void @hazy_open(i64 %value)
         musttail call tailcc void @hazy_return(i64 %consumer, i64 %result)
         ret void
       }

       """

-- |
-- What a unit that is not the program sees of the program's.
declared :: String
declared =
  """
  @hazy_spill = external global [64 x i64]
  @hazy_roots = external global [1024 x i64]
  declare tailcc void @hazy_force(i64, i64)
  """

-- |
-- Where a program starts.
mainFunction :: String
mainFunction =
  """
  define i32 @main() {
    call tailcc void @hazy_entry()
    call void @hazy_finish()
    ret i32 0
  }
  """
