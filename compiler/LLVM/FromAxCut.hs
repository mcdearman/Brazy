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
-- A function owns what it is given, and each value in the row is a
-- reference it has to give up. The exception is a value that is lent: see
-- 'lentValue'.
module LLVM.FromAxCut
  ( Target (..),
    arm64,
    windows,
    withoutReuse,
    lending,
    checking,
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
import Data.Foldable (for_, toList)
import Data.List (intercalate, intersperse, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy.Text (Text, toStrict, unpack)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Traversable (for)
import qualified Core.Foreign as Foreign
import Generate.Global (Global)
import Sequent.Primitive (Primitive (..))
import qualified Sequent.Primitive as Primitive
import qualified Sequent.Printer as Sequent (global, parts)
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
    reuse :: !Bool,
    -- | For a definition that is a function, which of its parameters it
    -- only reads, if any. Such a function has a second entry that takes
    -- those lent.
    lends :: Global -> Maybe [Bool],
    -- | Whether a function hands over what it holds when it is asked to,
    -- so that every count of references can be checked. It costs a test at
    -- the start of every function, so it is only for testing.
    checked :: !Bool,
    -- | How a definition that is made wherever it is used, such as the
    -- instance of a class for the tuples of a size, meets the same one made
    -- in another unit: what LLVM calls its linkage.
    common :: String
  }

-- |
-- What code is generated for where nothing says otherwise. It was first
-- written for arm64, and is as right for x86-64 on Linux.
arm64 :: Target
arm64 =
  Target
    { registers = 8,
      reuse = True,
      lends = const Nothing,
      checked = False,
      -- The linker keeps one of them.
      common = "linkonce_odr "
    }

-- |
-- Windows on x86-64. A call there passes four arguments in registers, and
-- LLVM cannot guarantee a tail call that passes any on the stack.
--
-- Its linker does not take two units that each have one definition by a
-- name and keep one, unless more is said than is said here. There, each
-- unit has its own.
windows :: Target
windows = arm64 {registers = 4, common = "internal "}

-- |
-- A target whose programs can have their counts of references checked.
checking :: Target -> Target
checking target = target {checked = True}

-- |
-- A target whose functions take lent the parameters they only read.
lending :: (Global -> Maybe [Bool]) -> Target -> Target
lending lends target = target {lends}

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
    -- | The next number for a name inside the function that is being
    -- generated. Each function counts from nothing, so the names are short.
    counter :: !(STRef s Int),
    -- | The next number for a name that is one of everything generated.
    unique :: !(STRef s Int),
    -- | The functions and constants made along the way.
    extra :: !(STRef s Builder),
    -- | The definitions that have been mentioned.
    mentioned :: !(STRef s (Set Global)),
    -- | The whole path to the code being generated: the definition it is
    -- in, and then every object inside that which has been gone into. What
    -- is generated is called by it.
    scope :: !Scope,
    -- | The names that have been given to functions.
    taken :: !(STRef s (Set Lazy.Text.Text)),
    -- | The values that keep alive what a function was lent: the consumer
    -- of its result, and every object made that holds one of these. This
    -- and the next are of the function that is being generated, as the
    -- names are.
    holders :: !(STRef s (Set Lazy.Text.Text)),
    -- | What each object that was made holds.
    captures :: !(STRef s (Map Lazy.Text.Text [Lazy.Text.Text])),
    -- | The definition each value that is one stands for, and how many
    -- arguments it takes at once if it is known to be a function.
    standing :: !(STRef s (Map Lazy.Text.Text (Global, Maybe Int))),
    -- | The definitions whose code is gone to by its name, and how many
    -- arguments it is given.
    direct :: !(STRef s (Map Global Int)),
    -- | The functions of C that are called, each with how it is declared.
    cFunctions :: !(STRef s (Map Text Builder)),
    -- | The definitions whose second entry is called, and how many
    -- arguments it is called with.
    called :: !(STRef s (Map Global Int)),
    -- | The consumers that are gone to and not made, by what stands for
    -- each where it would be.
    joins :: !(STRef s (Map Lazy.Text.Text JoinPoint))
  }

-- |
-- A consumer that is not made: the block it is, what it would have held,
-- which stays where it is, and each place that goes to it with a value.
data JoinPoint = JoinPoint
  { joinLabel :: Builder,
    joinHeld :: [Builder],
    joinJumps :: [(Builder, Builder)]
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
  unique <- newSTRef 0
  extra <- newSTRef mempty
  mentioned <- newSTRef Set.empty
  -- A definition's own name is not free for the code of an object inside
  -- another, which is called by the path to it and may come to the same.
  taken <- newSTRef $ Set.fromList [toLazyText (symbol name) | name <- Map.keys defined ++ Map.keys shared]
  holders <- newSTRef Set.empty
  captures <- newSTRef Map.empty
  standing <- newSTRef Map.empty
  called <- newSTRef Map.empty
  direct <- newSTRef Map.empty
  cFunctions <- newSTRef Map.empty
  joins <- newSTRef Map.empty
  let state =
        State
          { target,
            counter,
            unique,
            extra,
            mentioned,
            scope = Scope "entry" [],
            taken,
            holders,
            captures,
            standing,
            called,
            direct,
            cFunctions,
            joins
          }
      global linkage (name, object@Object {kind}) = do
        function
          state {scope = scopeOf name}
          (symbol name <> fromString ".code")
          (comment name)
          Nothing
          []
          -- The code is seen from wherever the definition is: a call of
          -- the definition from there goes to it.
          linkage
          object
        -- A package cannot tell who calls the second entry.
        if shape == Package then entry linkage (name, object) else pure ()
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
      -- The entry that takes lent what the function only reads. It is
      -- seen from wherever the definition is.
      entry linkage (name, object@Object {parameters}) = case lends target name of
        Just lent
          | or lent,
            length lent + 1 == length parameters ->
              function
                state {scope = scopeOf name}
                (symbol name <> fromString ".lent")
                (comment name)
                Nothing
                lent
                linkage
                object
        _ -> pure ()
      -- In a program the second entry of a definition is made when
      -- something calls it. It is the whole definition over again, and
      -- most are never called: the ones that were made and not called were
      -- a quarter of what was generated for the compiler. Making one may
      -- call more.
      entered made = do
        asked <- readSTRef called
        case filter (`Set.notMember` made) (Map.keys asked) of
          [] -> pure ()
          new -> do
            for_ new $ \name -> case (Map.lookup name defined, Map.lookup name shared) of
              (Just object, _) -> entry "internal " (name, object)
              (_, Just object) -> entry (common target) (name, object)
              _ -> pure ()
            entered (made <> Set.fromList new)
  -- Nothing outside a program uses what is in it.
  globals <- traverse (global (if shape == Package then "" else "internal ")) (Map.toList defined)
  common <- traverse (global (common target)) (Map.toList shared)
  body <- case start of
    Just entry -> do
      body <- statement state Nothing [] entry
      pure $ fromString "\ndefine internal tailcc void @hazy_entry() {\n" <> body <> fromString "}\n"
    Nothing -> pure mempty
  if shape == Package then pure () else entered Set.empty
  extra <- readSTRef extra
  mentioned <- readSTRef mentioned
  called <- readSTRef called
  direct <- readSTRef direct
  cFunctions <- readSTRef cFunctions
  -- What is used and is not here is some other unit's.
  let elsewhere = do
        name <- Set.toList mentioned
        if Map.member name defined || Map.member name shared
          then []
          else [quoted (symbol name) <> fromString " = external global { i64, ptr }\n"]
      entries = do
        (name, count) <- Map.toList called
        if Map.member name defined || Map.member name shared
          then []
          else
            [ mconcat
                [ fromString "declare tailcc void ",
                  quoted (symbol name <> fromString ".lent"),
                  fromString "(",
                  commas (replicate (min count (registers target)) (fromString "i64")),
                  fromString ")\n"
                ]
            ]
      codes = do
        (name, count) <- Map.toList direct
        if Map.member name defined || Map.member name shared
          then []
          else
            [ mconcat
                [ fromString "declare tailcc void ",
                  quoted (symbol name <> fromString ".code"),
                  fromString "(",
                  commas (replicate (min count (registers target)) (fromString "i64")),
                  fromString ")\n"
                ]
            ]
  pure $ mconcat [mconcat elsewhere, mconcat entries, mconcat codes, mconcat (Map.elems cFunctions), mconcat globals, mconcat common, body, extra]
  where
    comment global = fromString "; " <> Sequent.global global <> fromString "\n"

-- Block layouts -------------------------------------------------------------

dataKind, closureKind, thunkKind, integerKind, huskKind :: Integer
dataKind = 0
closureKind = 1
thunkKind = 2
integerKind = 5
huskKind = 7

-- |
-- The tag of an @Integer@ that is kept in its block, as the runtime has it.
smallIntegerTag :: Int
smallIntegerTag = 1

kindOf :: Kind -> Integer
kindOf = \case
  Closure -> closureKind
  Thunk -> thunkKind
  Join -> error "a consumer that is gone to is never made"

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
-- How many values a function can hand over when a check is due.
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
symbol = render . scopeOf

-- |
-- The whole path to some code: the definition at the top of a module that
-- it is in, and then every definition and object inside that which was
-- gone into to get to it.
data Scope = Scope String [String]

scopeOf :: Global -> Scope
scopeOf global = Scope (escaped $ drop 1 $ Lazy.Text.unpack $ toLazyText first) (map (escaped . unpack) inside)
  where
    (first, inside) = Sequent.parts global

-- |
-- What the code at the end of a path is called. A path of a few names is
-- written out. A longer one is the definition at the top, a mark that
-- stands for the whole path, and the last name: paths get to be dozens of
-- names long, and written out they were a quarter of all that was
-- generated.
--
-- The mark is worked out from the path alone, so that two units call a
-- definition the same. Two paths under one definition could come to one
-- mark, and then LLVM refuses what is generated, saying a name is defined
-- twice. A mark is one of two to the 60th.
render :: Scope -> Builder
render (Scope first inside)
  | length inside <= 2 = fromString $ intercalate "." (first : inside)
  | otherwise = fromString $ intercalate "." (first : ('~' : digest inside) : final)
  where
    final = case reverse inside of
      "direct" : before : _ -> [before, "direct"]
      end : _ -> [end]
      [] -> []

-- |
-- Twelve letters and digits for a path: three numbers of twenty bits. No
-- number on the way to them is past 31 bits, so that they are worked out
-- the same by a compiler that Hazy made, where an @Int@ is 63 bits, as by
-- one that GHC made, where it is 64.
digest :: [String] -> String
digest inside = concatMap digits [hash 31 1048573, hash 131 1048559, hash 1009 1048549]
  where
    text = intercalate "\US" inside
    hash :: Int -> Int -> Int
    hash factor modulus = foldl (\known character -> (known * factor + ord character) `mod` modulus) 5381 text
    digits value = [alphabet !! (value `div` (62 ^ place) `mod` 62) | place <- [0 .. 3 :: Int]]
    alphabet = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

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
named :: State s -> Text -> ST s (Builder, Scope)
named State {scope = Scope first inside, taken, unique} hint = do
  let path = inside ++ [escaped (unpack hint)]
  known <- readSTRef taken
  scope <-
    if Set.member (toLazyText (render (Scope first path))) known
      then do
        next <- readSTRef unique
        writeSTRef unique $! next + 1
        pure $ Scope first (path ++ [show next])
      else pure (Scope first path)
  let name = render scope
  modifySTRef' taken (Set.insert (toLazyText name))
  pure (name, scope)

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
-- A new name for a value that the function has been lent. It is not a
-- reference the function has to give up, and it is not counted when it is
-- copied or dropped: whoever lent it holds it for as long as the function
-- can use it. A use that needs a reference of its own makes one first: see
-- 'own'.
lentLocal :: State s -> ST s Builder
lentLocal state = do
  next <- fresh state
  pure $ fromString "%b" <> fromString (show next)

lentValue :: Builder -> Bool
lentValue value = take 2 (Lazy.Text.unpack (toLazyText value)) == "%b"

-- |
-- Make a reference of a value that was lent, for a use that keeps it or
-- hands it on. For any other value this is nothing: the reference the
-- function has is the one that is used.
own :: Builder -> Builder
own value
  | lentValue value = line [fromString "call void @hazy_share(i64 ", value, fromString ")"]
  | otherwise = mempty

-- |
-- The values that are only still held because something that was lent may
-- depend on them. They have no name, and are given up when the function
-- goes on to something else.
waiting :: Environment -> (Environment, Environment)
waiting = span (Text.null . fst)

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

-- |
-- Read a word out of a block that was lent. It is lent as well: the block
-- holds it.
loadLent :: State s -> Builder -> Int -> ST s (Builder, Builder)
loadLent state pointer index = do
  (code, place) <- slot state pointer index
  value <- lentLocal state
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
          fromString (if words <= smallBlock then " = call ptr @hazy_take(i64 " else " = call ptr @hazy_alloc(i64 "),
          fromString (show words),
          fromString ", i64 ",
          number head,
          fromString ")"
        ],
      block
    )

-- |
-- The most words a block can have and still be taken from a list of free
-- blocks of its size where it is made. The runtime has the same.
smallBlock :: Int
smallBlock = 32

-- |
-- An address as a word.
word :: State s -> Builder -> ST s (Builder, Builder)
word state pointer = do
  value <- local state
  pure (line [value, fromString " = ptrtoint ptr ", pointer, fromString " to i64"], value)

-- |
-- How many arguments can be passed outside the registers. A constructor of
-- very many fields is taken apart into as many arguments: the compiler's own
-- record of what is built in has hundreds. The number is also written where
-- the place for them is defined and declared, below.
spills :: Int
spills = 1024

-- |
-- Where an argument that is not passed in a register goes.
spill :: Int -> Builder
spill index =
  fromString "getelementptr ([1024 x i64], ptr @hazy_spill, i64 0, i64 " <> fromString (show index) <> fromString ")"

-- |
-- Enter some code and do not come back. The first argument is the object the
-- code belongs to.
enter :: State s -> Builder -> [Builder] -> Builder
enter State {target = Target {registers}} code arguments
  | length spilled > spills = error "too many arguments"
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
-- If a check is due, hand over everything the function holds. This is at
-- the start of a function, where that is what it was given: nothing else in
-- the program is alive but definitions at the top. It is only done by a
-- program that was compiled to be checked.
safepoint :: State s -> [Builder] -> ST s Builder
safepoint state roots
  | not (checked (target state)) = pure mempty
  | null roots = pure mempty
  | length roots > rootLimit = error "a function holds too many values"
  | otherwise = do
      due <- local state
      test <- local state
      collect <- label state
      go <- label state
      pure $
        mconcat
          [ line [due, fromString " = load i64, ptr @hazy_check_due"],
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
              [ fromString "call void @hazy_check(ptr @hazy_roots, i64 ",
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
--
-- Some of the parameters may be lent and not owned, which is said for each
-- in turn. What keeps those alive is the consumer of the function's result,
-- the last parameter: the caller saw to it that it holds them.
function :: State s -> Builder -> Builder -> Maybe Int -> [Bool] -> String -> Object -> ST s ()
function outer@State {target = Target {registers}, extra} name comment husk lent linkage object = do
  let Object {kind, captured, parameters, body} = object
  -- The names of a function are its own, and so is what is known of them.
  -- The functions made for its consumers go on with both: see
  -- 'continuation'.
  counting <- newSTRef 0
  holding <- newSTRef Set.empty
  capturing <- newSTRef Map.empty
  let state = outer {counter = counting, holders = holding, captures = capturing}
  self <- local state
  arguments <- for (zip parameters (lent ++ repeat False)) $ \(_, only) ->
    if only then lentLocal state else local state
  if or lent
    then modifySTRef' (holders state) (Set.insert (toLazyText (last arguments)))
    else pure ()
  let (passed, spilled) = splitAt (registers - 1) arguments
      unspill = mconcat $ do
        (index, argument) <- zip [0 ..] spilled
        pure $ line [argument, fromString " = load i64, ptr ", spill index]
  (code, pointer) <- address state self
  captures <- for (zip [capturesAt ..] captured) $ \(index, _) -> load state pointer index
  let taken = case kind of
        Closure -> open self
        Thunk -> mempty
        Join -> mempty
  (carrying, carried, token) <- case husk of
    Nothing -> pure (mempty, [], Nothing)
    Just size -> do
      (loading, held) <- load state pointer (capturesAt + length captured)
      (code, block) <- unpacked state held
      pure (loading, [(held, code)], Just (block, size))
  -- What was lent is not handed over: it is not the function's, and whoever
  -- lent it is reached through the consumer.
  check <- safepoint state (filter (not . lentValue) arguments ++ map snd captures ++ map fst carried)
  body <- statement state token (zip parameters arguments ++ zip captured (map snd captures)) body
  modifySTRef' extra $ \known ->
    mconcat
      [ known,
        fromString "\n",
        comment,
        fromString "define ",
        fromString linkage,
        fromString "tailcc void ",
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
  (name, scope) <- named state hint
  function state {scope} name mempty husk [] "internal " object
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
--
-- It is given the values that keep alive what the function was lent. One of
-- those that is no longer wanted is not erased here, since something lent
-- may be used after: it goes on at the front of the environment with no
-- name, and is given up when the function goes on to something else.
substitute :: Set Lazy.Text.Text -> Environment -> [(Text, Text)] -> (Builder, Environment)
substitute keeping environment pairs =
  (adjust, already ++ later ++ [(new, find old) | (new, old) <- pairs])
  where
    (already, named') = waiting environment
    held = Map.fromListWith (+) [(name, 1 :: Int) | (name, _) <- named']
    wanted = Map.fromListWith (+) [(old, 1 :: Int) | (_, old) <- pairs]
    find old = case lookup old named' of
      Just value -> value
      Nothing -> error $ unpack old ++ " is not in the environment"
    changes = do
      (name, have) <- Map.toList held
      pure (find name, Map.findWithDefault 0 name wanted - have)
    kept value = Set.member (toLazyText value) keeping
    adjust = mconcat $ do
      (value, more) <- changes
      if more >= 0
        then replicate more (share value)
        else if kept value then [] else replicate (negate more) (erase value)
    later = do
      (value, more) <- changes
      if more < 0 && kept value then replicate (negate more) (Text.empty, value) else []

-- |
-- Note what an object that was just made holds. If it holds something that
-- keeps alive what the function was lent, then so does it.
remember :: State s -> Builder -> [Builder] -> ST s ()
remember state made held = do
  let texts = map toLazyText held
  modifySTRef' (captures state) (Map.insert (toLazyText made) texts)
  keeping <- readSTRef (holders state)
  if any (`Set.member` keeping) texts
    then modifySTRef' (holders state) (Set.insert (toLazyText made))
    else pure ()

-- |
-- Whether one of some objects holds a value, itself or through an object it
-- holds, going by what each was made holding.
holds :: Map Lazy.Text.Text [Lazy.Text.Text] -> Set Lazy.Text.Text -> [Lazy.Text.Text] -> Lazy.Text.Text -> Bool
holds made seen objects value = case objects of
  [] -> False
  first : rest
    | Set.member first seen -> holds made seen rest value
    | otherwise ->
        let held = Map.findWithDefault [] first made
         in value `elem` held || holds made (Set.insert first seen) (held ++ rest) value

-- |
-- Call a function with its arguments, the last of which is the consumer of
-- its result. It is given what has to be given up first.
--
-- If the function is a definition that only reads some of its parameters,
-- and each argument for one of those can be lent, the entry that takes them
-- lent is gone to. An argument can be lent when something keeps it alive
-- until the function is done, which has to be the consumer that is passed,
-- since nothing else is left of the caller: the consumer holds the same
-- value, or, for a value the caller was lent itself, holds what keeps that
-- alive. A value that is never counted can always be lent.
call :: State s -> Builder -> Builder -> [Builder] -> ST s Builder
call state@State {target} first callee arguments = do
  standing' <- readSTRef (standing state)
  keeping <- readSTRef (holders state)
  made <- readSTRef (captures state)
  let consumer = last arguments
      given = init arguments
      found = do
        (global, _) <- Map.lookup (toLazyText callee) standing'
        lent <- lends target global
        if or lent && length lent == length given then Just (global, lent) else Nothing
      -- A definition that is known to take what it is given here. Its code
      -- is gone to by name: the definition is not asked where its code is.
      known = case Map.lookup (toLazyText callee) standing' of
        Just (global, Just count) | count == length given -> Just global
        _ -> Nothing
      plainly = case known of
        Just global -> do
          modifySTRef' (direct state) (Map.insert global (length arguments + 1))
          pure $
            mconcat
              [ foldMap own (callee : given),
                first,
                enter state (quoted (symbol global <> fromString ".code")) (callee : arguments)
              ]
        Nothing -> do
          code <- invoke state callee arguments
          pure $ foldMap own (callee : given) <> first <> code
      -- What is done to an argument to lend it, if it can be lent.
      lend argument
        | lentValue argument = if Set.member (toLazyText consumer) keeping then Just mempty else Nothing
        | not (counted argument) = Just mempty
        -- The consumer holds it too, so the caller's reference is not the
        -- last, and is given up.
        | holds made Set.empty [toLazyText consumer] (toLazyText argument) = Just (erase argument)
        | otherwise = Nothing
      prepare (only, argument) = if only then lend argument else Just (own argument)
  case found of
    Just (global, lent)
      | Just prepared <- traverse prepare (zip lent given) -> do
          modifySTRef' (called state) (Map.insert global (length arguments + 1))
          modifySTRef' (mentioned state) (Set.insert global)
          before <- local state
          more <- local state
          pure $
            mconcat
              [ mconcat prepared,
                first,
                line [before, fromString " = load i64, ptr @hazy_lent"],
                line [more, fromString " = add i64 ", before, fromString ", 1"],
                line [fromString "store i64 ", more, fromString ", ptr @hazy_lent"],
                enter state (quoted (symbol global <> fromString ".lent")) (callee : arguments)
              ]
    _ -> plainly

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
-- out to be a value already, or a thunk that has been evaluated already,
-- the first is entered at once and the consumer is never made.
--
-- A block that was kept back may go with it, as it may with any consumer:
-- the first function takes it last, and the object holds it last.
--
-- It is also given what was forced and what the consumer captures, as they
-- are where the consumer is made, so that what is known there of what holds
-- what is still known inside it. An object that holds what was forced holds
-- the value too: either they are the same, or the one is a thunk that has
-- been evaluated and now holds the other. That is what lets the value be
-- lent to a function that is called with such an object as its consumer.
--
-- Some of what the consumer would capture may have been lent to the
-- function it is made in. The consumer as an object has to own what it
-- holds. But when the first function is entered at once, nothing is held by
-- anything new, and what was lent can stay lent as long as what keeps it
-- alive is passed along too. Then there is a third function, the first over
-- again taking those lent, which is what is entered at once: its name is
-- the last thing given back.
continuation ::
  State s -> Text -> Maybe Int -> Builder -> [Builder] -> Object -> ST s (Builder, Builder, Maybe Builder)
continuation outer@State {target = Target {registers}, extra} hint husk forced outside object = do
  let Object {captured, parameters, body} = object
  (entry, scope) <- named outer hint
  keeping <- readSTRef (holders outer)
  let direct = entry <> fromString ".direct"
      names = parameters ++ captured
      state = outer {scope}
      lent = map lentValue (forced : outside)
      kept' value = Set.member (toLazyText value) keeping
      -- The function that is entered with the value and what was captured,
      -- given which of those it takes lent.
      entered name only = do
        named' <- for (zip names (only ++ repeat False)) $ \(_, flag) ->
          if flag then lentLocal state else local state
        made <- readSTRef (captures outer)
        let inside = zip outside (map toLazyText (drop (length parameters) named'))
            here text =
              [new | (old, new) <- inside, toLazyText old == text]
                ++ [toLazyText value | text == toLazyText forced, value <- take 1 named']
        for_ inside $ \(old, new) -> do
          case Map.lookup (toLazyText old) made of
            Just held -> modifySTRef' (captures outer) (Map.insert new (concatMap here held))
            Nothing -> pure ()
          if or only && kept' old
            then modifySTRef' (holders outer) (Set.insert new)
            else pure ()
          -- What was lent outside is kept alive by whatever kept it alive
          -- there, whether it comes in lent or owned.
          if kept' old
            then
              modifySTRef' (captures outer) $
                Map.insertWith (++) new $
                  [toLazyText value | (value, was) <- zip named' (forced : outside), lentValue was]
            else pure ()
        kept <- for husk $ \size -> do
          block <- local state
          pure (block, size)
        let arguments = named' ++ map fst (toList kept)
            (passed, spilled) = splitAt registers arguments
            unspill = mconcat $ do
              (index, argument) <- zip [0 ..] spilled
              pure $ line [argument, fromString " = load i64, ptr ", spill index]
        check <- safepoint state (filter (not . lentValue) arguments)
        body <- statement state kept (zip names named') body
        modifySTRef' extra $ \known ->
          mconcat
            [ known,
              fromString "\ndefine internal tailcc void ",
              quoted name,
              fromString "(",
              commas [fromString "i64 " <> argument | argument <- passed],
              fromString ") {\n",
              unspill,
              check,
              body,
              fromString "}\n"
            ]
  entered direct []
  borrowed <-
    if or lent && any kept' outside
      then do
        let name = direct <> fromString ".lent"
        entered name lent
        pure (Just name)
      else pure Nothing
  self <- local state
  given <- for parameters $ \_ -> local state
  (code, pointer) <- address state self
  captures <- for (zip [capturesAt ..] captured) $ \(index, _) -> load state pointer index
  carried <- for (toList husk) $ \_ -> do
    (loading, held) <- load state pointer (capturesAt + length captured)
    (unpacking, block) <- unpacked state held
    pure (loading, unpacking, block)
  modifySTRef' extra $ \known ->
    mconcat
      [ known,
        fromString "\ndefine internal tailcc void ",
        quoted entry,
        fromString "(",
        commas [fromString "i64 " <> argument | argument <- self : given],
        fromString ") {\n",
        if null captured && null carried then mempty else code <> foldMap fst captures,
        mconcat [loading | (loading, _, _) <- carried],
        open self,
        mconcat [unpacking | (_, unpacking, _) <- carried],
        enter state (quoted direct) (given ++ map snd captures ++ [block | (_, _, block) <- carried]),
        fromString "}\n"
      ]
  pure (entry, direct, borrowed)

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
  New _ Object {kind = Join} rest -> wants size rest
  New _ object rest -> wants size rest || carries size object rest
  Literal _ _ rest -> wants size rest
  Global _ _ _ rest -> wants size rest
  Primitive _ _ rest -> wants size rest
  Rec _ rest -> wants size rest
  Switch clauses fallback -> or [wants size body | Clause _ _ body <- clauses] || any (wants size) fallback
  Invoke {} -> False
  Force -> False
  Abort -> False
  Halt -> False

-- |
-- Whether a block can go with an object to where a value is built in it:
-- the object is a consumer, which is entered once, and it builds one.
carries :: Int -> Object -> Statement -> Bool
carries size Object {kind = Closure, method = Return, body} _ = wants size body
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
    keeping <- readSTRef (holders state)
    let (adjust, after) = substitute keeping environment pairs
    found <- readSTRef (joins state)
    let markers values = [toLazyText value | value <- values, Map.member (toLazyText value) found]
        before = markers (map snd environment)
        now = markers (map snd after)
        gone = filter (`notElem` now) before
    if length now /= length (nub now) then error "a consumer that is gone to was copied" else pure ()
    rest <- statement state token after rest
    pure $ adjust <> foldMap (\marker -> foldMap erase (joinHeld (found Map.! marker))) gone <> rest
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
    pure $ mconcat [foldMap (own . snd) taken, code, mconcat fields, code', rest]
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
      -- What is read out of a value that was lent is lent too, and the
      -- value is not given up: nothing below counts it.
      fields <- for (zip [fieldsAt ..] names) $ \(index, _) ->
        if lentValue scrutinee then loadLent state pointer index else load state pointer index
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
      (_, renamed) <- substitute Set.empty (kept ++ [(name, unmade)]) pairs,
      (_, [(_, consumer), (_, forced)]) <- waiting renamed,
      toLazyText consumer == toLazyText unmade -> do
        (entry, direct, borrowed) <- continuation state name (fmap snd token) forced (map snd taken) object
        keeping <- readSTRef (holders state)
        let (adjust, after) = substitute keeping (kept ++ [(name, unmade)]) pairs
            (late, named') = waiting after
            value = snd (last named')
            captures = map snd taken
            -- What was lent has to be owned by whoever goes on from here,
            -- unless it goes on lent. Then what was only held for its sake
            -- can go.
            owning = foldMap own (value : captures)
            husks = map fst (toList token)
            held = length captured + length husks
            -- Go on with a value, given what makes what goes with it owned
            -- if it has to be.
            onward first owned = case borrowed of
              Just lent ->
                foldMap (erase . snd) late <> enter state (quoted lent) (first : captures ++ husks)
              Nothing ->
                owned <> foldMap (erase . snd) late <> enter state (quoted direct) (first : captures ++ husks)
            fast = onward value owning
        if isWord value
          then pure $ adjust <> fast
          else do
            (code, block) <- allocate state (capturesAt + held) (header closureKind 0 held)
            (code', place) <- slot state block codeAt
            stores <- for (zip [capturesAt ..] captures) $ \(index, capture) -> store state block index capture
            carried <- for husks $ \husk -> do
              (packing, packed') <- packed state husk
              storing <- store state block (capturesAt + length captured) packed'
              pure $ packing <> storing
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
            -- A thunk that has been evaluated already has its value read
            -- out of it here, and the consumer is not made for that either.
            over <- local state
            delayed' <- label state
            done <- label state
            (reading, result) <- load state pointer 1
            fixed <- local state
            lasting <- label state
            counting <- label state
            ready <- label state
            let taking
                  -- A thunk that was lent goes on holding its value. The
                  -- value is lent as well, if what goes on can take it so.
                  | lentValue value = case borrowed of
                      Just _ -> mempty
                      Nothing -> share result
                  -- Otherwise the thunk is given up, as forcing it would:
                  -- a definition at the top keeps its value for good, and
                  -- any other gives it over if this was the last reference.
                  | otherwise =
                      mconcat
                        [ line [fixed, fromString " = icmp slt i64 ", head', fromString ", 0"],
                          line [fromString "br i1 ", fixed, fromString ", label %", lasting, fromString ", label %", counting],
                          lasting <> fromString ":\n",
                          share result,
                          line [fromString "br label %", ready],
                          counting <> fromString ":\n",
                          open value,
                          line [fromString "br label %", ready],
                          ready <> fromString ":\n"
                        ]
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
                  line [fromString "br i1 ", thunk, fromString ", label %", delayed', fromString ", label %", quick],
                  quick <> fromString ":\n",
                  fast,
                  delayed' <> fromString ":\n",
                  line [over, fromString " = icmp eq i64 ", kind', fromString ", 4"],
                  line [fromString "br i1 ", over, fromString ", label %", done, fromString ", label %", slow],
                  done <> fromString ":\n",
                  reading,
                  taking,
                  onward result (foldMap own captures),
                  slow <> fromString ":\n",
                  owning,
                  code,
                  code',
                  line [fromString "store ptr ", quoted entry, fromString ", ptr ", place],
                  mconcat stores,
                  mconcat carried,
                  code'',
                  foldMap (erase . snd) late,
                  enter state (fromString "@hazy_force") [made, value]
                ]
    where
      -- Stands for the consumer while the environment is worked out. It is
      -- never written.
      unmade = fromString "%unmade"
  -- A consumer that is only given values here is not made. What it would
  -- hold stays where it is, and what gives it a value goes to its code,
  -- which comes after, where those values come together.
  New name Object {kind = Join, captured, parameters = [parameter], body} rest -> do
    let (kept, taken) = splitEnd (length captured) environment
    next <- fresh state
    point <- label state
    let marker = fromString "%join" <> fromString (show next)
        key = toLazyText marker
    modifySTRef' (joins state) (Map.insert key JoinPoint {joinLabel = point, joinHeld = map snd taken, joinJumps = []})
    rest <- statement state token (kept ++ [(name, marker)]) rest
    found <- readSTRef (joins state)
    case Map.lookup key found of
      Just JoinPoint {joinJumps = jumps@(_ : _)} -> do
        value <- local state
        body <- statement state Nothing ((parameter, value) : zip captured (map snd taken)) body
        pure $
          mconcat
            [ rest,
              point <> fromString ":\n",
              line
                [ value,
                  fromString " = phi i64 ",
                  commas [fromString "[ " <> given <> fromString ", %" <> from <> fromString " ]" | (given, from) <- reverse jumps]
                ],
              body
            ]
      _ -> pure rest
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
        remember state value (map snd taken)
        rest <- statement state Nothing (kept ++ [(name, value)]) rest
        pure $ mconcat [foldMap (own . snd) taken, code, mconcat captures, packing, carried, code', rest]
  New name object@Object {captured} rest -> do
    let (kept, taken) = splitEnd (length captured) environment
    (code, block) <- object' state name Nothing object
    captures <- for (zip [capturesAt ..] taken) $ \(index, (_, value)) -> store state block index value
    (code', value) <- word state block
    remember state value (map snd taken)
    rest <- statement state token (kept ++ [(name, value)]) rest
    pure $ mconcat [foldMap (own . snd) taken, code, mconcat captures, code', rest]
  -- The function goes on to something else. What it gives away it has to
  -- own, and what was only held for the sake of something lent is given up.
  Invoke method -> do
    let (late, named') = waiting environment
        first = foldMap (erase . snd) late
    case (method, map snd named') of
      (Return, [value, consumer]) -> do
        found <- readSTRef (joins state)
        case Map.lookup (toLazyText consumer) found of
          Just JoinPoint {joinLabel} -> do
            from <- label state
            modifySTRef' (joins state) (Map.adjust (\point -> point {joinJumps = (value, from) : joinJumps point}) (toLazyText consumer))
            pure $
              mconcat
                [ own value,
                  first,
                  line [fromString "br label %", from],
                  from <> fromString ":\n",
                  line [fromString "br label %", joinLabel]
                ]
          Nothing -> do
            code <- invoke state consumer [value]
            pure $ own value <> first <> code
      (Apply, values@(_ : _ : _)) -> call state first (last values) (init values)
      _ -> error $ "bad invoke " ++ show method
  Force -> do
    let (late, named') = waiting environment
    case map snd named' of
      [consumer, value] ->
        pure $ own value <> foldMap (erase . snd) late <> enter state (fromString "@hazy_force") [consumer, value]
      _ -> error "force with the wrong environment"
  Literal name literal rest -> do
    (code, value) <- case literal of
      Literal.Int int -> pure (mempty, number (immediate (toInteger int)))
      Literal.Integer integer
        | integer >= -(2 ^ (63 :: Int)) && integer < 2 ^ (63 :: Int) -> do
            value <- smallInteger state integer
            pure (mempty, value)
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
  Global name global arity rest -> do
    modifySTRef' (mentioned state) (Set.insert global)
    let value = fromString "ptrtoint (ptr " <> quoted (symbol global) <> fromString " to i64)"
    modifySTRef' (standing state) (Map.insert (toLazyText value) (global, arity))
    statement state token (environment ++ [(name, value)]) rest
  -- A primitive borrows its arguments, which are given up afterwards.
  Primitive name primitive rest -> do
    let (kept, taken) = splitEnd (Primitive.arity primitive) environment
        arguments = map snd taken
    (code, value) <- case (primitive, arguments) of
      (Call call, _) -> foreignCall state call arguments
      _ | Just made <- intInline state primitive arguments -> made
      (Export call exported, _) -> do
        exportStub state call exported
        pure (mempty, fromString "ptrtoint (ptr @hazy_nullary to i64)")
      (Wrap call, [entry, function]) -> wrapperNew state call entry function
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
    pure $ mconcat [foldMap (own . snd) taken, foldMap fst made, filled, rest]
  Abort -> pure $ line [fromString "call void @hazy_abort()"] <> line [fromString "unreachable"]
  -- The program is over: whatever is still held is given up.
  Halt -> do
    found <- readSTRef (joins state)
    let given value = case Map.lookup (toLazyText value) found of
          Just JoinPoint {joinHeld} -> foldMap erase joinHeld
          Nothing -> erase value
    pure $ foldMap (given . snd) environment <> line [fromString "ret void"]

-- |
-- An @Integer@ written in the program that fits in 64 bits, as a constant
-- of the program: a block that is never counted, which holds the number, as
-- the runtime keeps a small one.
smallInteger :: State s -> Integer -> ST s Builder
smallInteger State {extra, unique} integer = do
  next <- readSTRef unique
  writeSTRef unique $! next + 1
  let name = fromString "@integer." <> fromString (show next)
      head = header integerKind smallIntegerTag 0 + immortal
  modifySTRef' extra $ \known ->
    mconcat
      [ known,
        name,
        fromString " = private unnamed_addr constant [2 x i64] [i64 ",
        number head,
        fromString ", i64 ",
        number integer,
        fromString "], align 8\n"
      ]
  pure $ fromString "ptrtoint (ptr " <> name <> fromString " to i64)"

-- |
-- Some text as a constant of the program, and how many bytes it is.
bytes :: State s -> String -> ST s (Builder, Int)
bytes State {extra, unique} string = do
  next <- readSTRef unique
  writeSTRef unique $! next + 1
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
  TextCompare -> True
  TextLength -> True
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
-- A primitive of @Int@s done where it is used, on the words they are: a
-- number @n@ is the word @2n + 1@. A sum, a difference, a product and the
-- rest are a few instructions on that, which come round at the bounds as
-- the runtime's do. A comparison gives one of the two constructors of
-- @Bool@, which are never counted. A division by zero is left to the
-- runtime, which stops the program.
intInline :: State s -> Primitive -> [Builder] -> Maybe (ST s (Builder, Builder))
intInline state primitive arguments = case (primitive, arguments) of
  (IntAdd, [left, right]) -> Just $ do
    sum' <- temporary state
    value <- localWord state
    pure (line [sum', fromString " = add i64 ", left, fromString ", ", right] <> line [value, fromString " = sub i64 ", sum', fromString ", 1"], value)
  (IntMinus, [left, right]) -> Just $ do
    difference <- temporary state
    value <- localWord state
    pure (line [difference, fromString " = sub i64 ", left, fromString ", ", right] <> line [value, fromString " = add i64 ", difference, fromString ", 1"], value)
  (IntMultiply, [left, right]) -> Just $ do
    number <- temporary state
    twice <- temporary state
    product' <- temporary state
    value <- localWord state
    pure
      ( line [number, fromString " = ashr i64 ", left, fromString ", 1"]
          <> line [twice, fromString " = sub i64 ", right, fromString ", 1"]
          <> line [product', fromString " = mul i64 ", number, fromString ", ", twice]
          <> line [value, fromString " = add i64 ", product', fromString ", 1"],
        value
      )
  (IntNegate, [given]) -> Just $ do
    value <- localWord state
    pure (line [value, fromString " = sub i64 2, ", given], value)
  (IntAbs, [given]) -> Just $ do
    negative <- temporary state
    negated <- temporary state
    value <- localWord state
    pure
      ( line [negative, fromString " = icmp slt i64 ", given, fromString ", 0"]
          <> line [negated, fromString " = sub i64 2, ", given]
          <> line [value, fromString " = select i1 ", negative, fromString ", i64 ", negated, fromString ", i64 ", given],
        value
      )
  (IntSignum, [given]) -> Just $ do
    positive <- temporary state
    negative <- temporary state
    either' <- temporary state
    value <- localWord state
    pure
      ( line [positive, fromString " = icmp sgt i64 ", given, fromString ", 1"]
          <> line [negative, fromString " = icmp slt i64 ", given, fromString ", 0"]
          <> line [either', fromString " = select i1 ", negative, fromString ", i64 -1, i64 1"]
          <> line [value, fromString " = select i1 ", positive, fromString ", i64 3, i64 ", either'],
        value
      )
  (IntEqual, [left, right]) -> Just $ compared "eq" left right
  (IntLessThenEqual, [left, right]) -> Just $ compared "sle" left right
  (IntQuot, [left, right]) -> Just $ divided "sdiv" "hazy_prim_int_quot" left right
  (IntRem, [left, right]) -> Just $ divided "srem" "hazy_prim_int_rem" left right
  _ -> Nothing
  where
    compared condition left right = do
      test <- temporary state
      value <- localWord state
      pure
        ( line [test, fromString " = icmp ", fromString condition, fromString " i64 ", left, fromString ", ", right]
            <> line [value, fromString " = select i1 ", test, fromString ", i64 ", nullaryOf 1, fromString ", i64 ", nullaryOf 0],
          value
        )
    divided operation runtime left right = do
      zero <- temporary state
      stop <- label state
      go <- label state
      dividend <- temporary state
      divisor <- temporary state
      result <- temporary state
      shifted <- temporary state
      value <- localWord state
      pure
        ( mconcat
            [ line [zero, fromString " = icmp eq i64 ", right, fromString ", 1"],
              line [fromString "br i1 ", zero, fromString ", label %", stop, fromString ", label %", go],
              stop <> fromString ":\n",
              line [fromString "call i64 @", fromString runtime, fromString "(i64 ", left, fromString ", i64 ", right, fromString ")"],
              line [fromString "unreachable"],
              go <> fromString ":\n",
              line [dividend, fromString " = ashr i64 ", left, fromString ", 1"],
              line [divisor, fromString " = ashr i64 ", right, fromString ", 1"],
              line [result, fromString " = ", fromString operation, fromString " i64 ", dividend, fromString ", ", divisor],
              line [shifted, fromString " = shl i64 ", result, fromString ", 1"],
              line [value, fromString " = or i64 ", shifted, fromString ", 1"]
            ],
          value
        )
    nullaryOf :: Int -> Builder
    nullaryOf tag =
      fromString "ptrtoint (ptr getelementptr ([8192 x i64], ptr @hazy_nullary, i64 0, i64 "
        <> fromString (show tag)
        <> fromString ") to i64)"

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
  TextAppend -> "hazy_prim_text_append"
  TextEqual -> "hazy_prim_text_equal"
  TextCompare -> "hazy_prim_text_compare"
  TextLength -> "hazy_prim_text_length"
  TextTake -> "hazy_prim_text_take"
  TextDrop -> "hazy_prim_text_drop"
  TextIsPrefixOf -> "hazy_prim_text_is_prefix_of"
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
  Foreign name _ -> "hazy_prim_" ++ unpack name
  Call _ -> error "a function of C is called by its own name"
  Export _ _ -> error "a foreign export is nothing to run"
  Wrap _ -> error "a wrapper is made in place"

-- |
-- A call of a function of C: what it is given made what C has, and what it
-- gives made a value again. It is declared once in each unit that calls it.
foreignCall :: State s -> Foreign.Call -> [Builder] -> ST s (Builder, Builder)
foreignCall state Foreign.Call {target = Foreign.Address, symbol} _ = do
  -- Something of C is declared as a function, whatever it is: only its
  -- address is wanted, and a name is declared once in a unit.
  modifySTRef' (cFunctions state) $
    Map.insertWith (\_ old -> old) symbol $
      mconcat [fromString "declare void @", fromString (unpack symbol), fromString "()\n"]
  number <- temporary state
  shifted <- temporary state
  value <- localWord state
  pure
    ( line [number, fromString " = ptrtoint ptr @", fromString (unpack symbol), fromString " to i64"]
        <> line [shifted, fromString " = shl i64 ", number, fromString ", 1"]
        <> line [value, fromString " = or i64 ", shifted, fromString ", 1"],
      value
    )
foreignCall state Foreign.Call {target = Foreign.Dynamic, arguments, result} (function : values) = do
  -- The function is called where its address is, which is given first.
  number <- temporary state
  address <- temporary state
  given <- sequence (zipWith (marshalIn state) (drop 1 arguments) values)
  let invocation =
        mconcat
          [ fromString "call ",
            cResult result,
            fromString " ",
            address,
            fromString "(",
            commas (zipWith typed (drop 1 arguments) (map snd given)),
            fromString ")"
          ]
  (code, value) <- marshalOut state result invocation
  pure
    ( line [number, fromString " = ashr i64 ", function, fromString ", 1"]
        <> line [address, fromString " = inttoptr i64 ", number, fromString " to ptr"]
        <> foldMap fst given
        <> code,
      value
    )
foreignCall state Foreign.Call {symbol, arguments, result} values = do
  modifySTRef' (cFunctions state) $
    Map.insertWith (\_ old -> old) symbol $
      mconcat
        [ fromString "declare ",
          cResult result,
          fromString " @",
          fromString (unpack symbol),
          fromString "(",
          commas (map cParameter arguments),
          fromString ")\n"
        ]
  given <- sequence (zipWith (marshalIn state) arguments values)
  let invocation =
        mconcat
          [ fromString "call ",
            cResult result,
            fromString " @",
            fromString (unpack symbol),
            fromString "(",
            commas (zipWith typed arguments (map snd given)),
            fromString ")"
          ]
  (code, value) <- marshalOut state result invocation
  pure (foldMap fst given <> code, value)

-- |
-- A function of C that calls a definition, as a foreign export says: what C
-- gives is made values, the definition's code is run on them until it
-- stops, and what it left is made what C gets. What is thrown there and
-- not caught there ends the program; a catch begun before C was called is
-- out of its reach. Whatever else the unit says of the name, it is this.
exportStub :: State s -> Foreign.Call -> Global -> ST s ()
exportStub state call@Foreign.Call {symbol = name, arguments, result} exported = do
  parameters <- traverse (const (temporary state)) arguments
  let self = fromString "ptrtoint (ptr " <> quoted (symbol exported) <> fromString " to i64)"
  body <- entering state call parameters (quoted (symbol exported <> fromString ".code")) [self]
  let stub =
        mconcat
          [ fromString "\ndefine ",
            cResult result,
            fromString " @",
            fromString (unpack name),
            fromString "(",
            commas (zipWith typed arguments parameters),
            fromString ") {\n",
            body,
            fromString "}\n"
          ]
  modifySTRef' (cFunctions state) (Map.insert name stub)

-- |
-- What a function of C does to call into the program: what C gave, in the
-- parameters, is made values; some code is run on what comes first and
-- them until it stops, with what is thrown there and not caught there out
-- of reach of a catch begun before; and what was left is made what C gets.
entering :: State s -> Foreign.Call -> [Builder] -> Builder -> [Builder] -> ST s Builder
entering state@State {target = Target {registers}} Foreign.Call {arguments, result} parameters code first = do
  given <- for (zip arguments parameters) $ \(marshal, parameter) ->
    marshalOut state marshal (fromString "freeze " <> cType marshal <> fromString " " <> parameter)
  taken <- local state
  (made, returned) <- case result of
    Foreign.Unit -> pure (mempty, fromString "void")
    _ -> do
      (made, value) <- marshalIn state result taken
      pure (made, cType result <> fromString " " <> value)
  let -- The code never gives its result to its consumer.
      consumer = fromString "ptrtoint (ptr @hazy_nullary to i64)"
      (passed, spilled) = splitAt registers (first ++ map snd given ++ [consumer])
  pure $
    mconcat
      [ foldMap fst given,
        line [fromString "call void @hazy_catch_barrier()"],
        mconcat
          [ line [fromString "store i64 ", argument, fromString ", ptr ", spill index]
            | (index, argument) <- zip [0 ..] spilled
          ],
        line
          [ fromString "call tailcc void ",
            code,
            fromString "(",
            commas [fromString "i64 " <> argument | argument <- passed],
            fromString ")"
          ],
        line [fromString "call void @hazy_catch_unbarrier()"],
        line [taken, fromString " = call i64 @hazy_callback_take()"],
        made,
        erase taken,
        line [fromString "ret ", returned]
      ]

-- |
-- How many functions of C a unit has for wrappers of a type: how many
-- there can be at once, until some are freed.
wrappers :: Int
wrappers = 128

-- |
-- A function of C made of a function of the program, as a wrapper does. A
-- unit has a number of functions of C for each type, each of which calls
-- what is kept in a place of its own: the code that applies the function to
-- what C gives, as an object, and the function. The runtime gives out the
-- places, and takes them back when the function of C is freed.
wrapperNew :: State s -> Foreign.Call -> Builder -> Builder -> ST s (Builder, Builder)
wrapperNew state call entry function = do
  let name = wrapperName call
      key = Lazy.Text.toStrict (toLazyText name)
  known <- readSTRef (cFunctions state)
  if Map.member key known
    then pure ()
    else do
      made <- wrapperFunctions state call name
      modifySTRef' (cFunctions state) (Map.insert key made)
  marshalOut state Foreign.Pointer $
    mconcat
      [ fromString "call ptr @hazy_wrapper_new(ptr ",
        quoted (name <> fromString ".slots"),
        fromString ", ptr ",
        quoted (name <> fromString ".stubs"),
        fromString ", i64 ",
        fromString (show wrappers),
        fromString ", i64 ",
        entry,
        fromString ", i64 ",
        function,
        fromString ")"
      ]

-- |
-- The name of the functions of C a unit has for wrappers of a type.
wrapperName :: Foreign.Call -> Builder
wrapperName Foreign.Call {arguments, result, action} =
  fromString $
    "hazy.wrapper."
      ++ concatMap ((++ ".") . show) arguments
      ++ "to."
      ++ show result
      ++ (if action then ".io" else "")

-- |
-- The functions of C for wrappers of a type, the places they call what is
-- kept in, and where each function is.
wrapperFunctions :: State s -> Foreign.Call -> Builder -> ST s Builder
wrapperFunctions state call@Foreign.Call {arguments, result} name = do
  parameters <- traverse (const (temporary state)) arguments
  body <- entering state call parameters (fromString "%code") [fromString "%entry", fromString "%function"]
  let slots = quoted (name <> fromString ".slots")
      place index = fromString "getelementptr ([" <> number (toInteger (2 * wrappers)) <> fromString " x i64], ptr " <> slots <> fromString ", i64 0, i64 " <> number index <> fromString ")"
      header = commas (zipWith typed arguments parameters)
      stub index =
        mconcat
          [ fromString "\ndefine internal ",
            cResult result,
            fromString " ",
            quoted (name <> fromString "." <> fromString (show index)),
            fromString "(",
            header,
            fromString ") {\n",
            line [fromString "%entry = load i64, ptr ", place (2 * index)],
            line [fromString "%function = load i64, ptr ", place (2 * index + 1)],
            line
              [ case result of
                  Foreign.Unit -> mempty
                  _ -> fromString "%result = ",
                fromString "tail call ",
                cResult result,
                fromString " ",
                quoted name,
                fromString "(",
                commas ([fromString "i64 %entry", fromString "i64 %function"] ++ zipWith typed arguments parameters),
                fromString ")"
              ],
            line
              [ case result of
                  Foreign.Unit -> fromString "ret void"
                  _ -> fromString "ret " <> cType result <> fromString " %result"
              ],
            fromString "}\n"
          ]
  pure $
    mconcat
      [ fromString "\n",
        slots,
        fromString " = internal global [",
        number (toInteger (2 * wrappers)),
        fromString " x i64] zeroinitializer\n",
        quoted (name <> fromString ".stubs"),
        fromString " = internal constant [",
        number (toInteger wrappers),
        fromString " x ptr] [",
        commas [fromString "ptr " <> quoted (name <> fromString "." <> fromString (show index)) | index <- [0 .. wrappers - 1]],
        fromString "]\n",
        foldMap stub [0 .. toInteger wrappers - 1],
        -- What is called: the code of the object kept first, given it, the
        -- function kept second, and what C gave. The code takes a reference
        -- to each, and the place keeps its own.
        fromString "\ndefine internal ",
        cResult result,
        fromString " ",
        quoted name,
        fromString "(",
        commas ([fromString "i64 %entry", fromString "i64 %function"] ++ zipWith typed arguments parameters),
        fromString ") {\n",
        line [fromString "call void @hazy_share(i64 %entry)"],
        line [fromString "call void @hazy_share(i64 %function)"],
        line [fromString "%object = inttoptr i64 %entry to ptr"],
        line [fromString "%place = getelementptr i64, ptr %object, i64 ", fromString (show codeAt)],
        line [fromString "%code = load ptr, ptr %place"],
        body,
        fromString "}\n"
      ]

-- |
-- The type C has for a value.
cType :: Foreign.Marshal -> Builder
cType = fromString . \case
  Foreign.Int -> "i64"
  Foreign.Int8 -> "i8"
  Foreign.Int16 -> "i16"
  Foreign.Int32 -> "i32"
  Foreign.Int64 -> "i64"
  Foreign.Word -> "i64"
  Foreign.Word8 -> "i8"
  Foreign.Word16 -> "i16"
  Foreign.Word32 -> "i32"
  Foreign.Word64 -> "i64"
  Foreign.Char -> "i32"
  Foreign.Bool -> "i32"
  Foreign.Pointer -> "ptr"
  Foreign.Double -> "double"
  Foreign.Float -> "float"
  Foreign.Unit -> "void"

-- |
-- The type C has for a value, with what C does to make a small number a
-- whole register, which it is passed and given back in.
cParameter :: Foreign.Marshal -> Builder
cParameter marshal = cType marshal <> foldMap (fromString . (' ' :)) (extension marshal)

-- |
-- The type C gives back, with the same, which is written before it.
cResult :: Foreign.Marshal -> Builder
cResult marshal = foldMap (fromString . (++ " ")) (extension marshal) <> cType marshal

extension :: Foreign.Marshal -> Maybe String
extension = \case
  Foreign.Int8 -> Just "signext"
  Foreign.Int16 -> Just "signext"
  Foreign.Word8 -> Just "zeroext"
  Foreign.Word16 -> Just "zeroext"
  _ -> Nothing

-- |
-- A name for a step of passing a value to C or back, which is never
-- counted.
temporary :: State s -> ST s Builder
temporary state = do
  next <- fresh state
  pure $ fromString "%f" <> fromString (show next)

-- |
-- A value as C has it, written with its type.
typed :: Foreign.Marshal -> Builder -> Builder
typed marshal value = cParameter marshal <> fromString " " <> value

-- |
-- A value as C is given it: the code, and what C is given, of the type
-- 'cParameter' says.
marshalIn :: State s -> Foreign.Marshal -> Builder -> ST s (Builder, Builder)
marshalIn state marshal value = case marshal of
  Foreign.Int -> do
    number <- temporary state
    pure (line [number, fromString " = ashr i64 ", value, fromString ", 1"], number)
  Foreign.Char -> do
    number <- temporary state
    code <- temporary state
    pure
      ( line [number, fromString " = ashr i64 ", value, fromString ", 1"]
          <> line [code, fromString " = trunc i64 ", number, fromString " to i32"],
        code
      )
  Foreign.Double -> do
    number <- temporary state
    pure (line [number, fromString " = call double @hazy_ffi_double(i64 ", value, fromString ")"], number)
  Foreign.Float -> do
    double <- temporary state
    single <- temporary state
    pure
      ( line [double, fromString " = call double @hazy_ffi_double(i64 ", value, fromString ")"]
          <> line [single, fromString " = fptrunc double ", double, fromString " to float"],
        single
      )
  Foreign.Bool -> do
    truth <- temporary state
    pure (line [truth, fromString " = call i32 @hazy_ffi_bool(i64 ", value, fromString ")"], truth)
  Foreign.Pointer -> do
    number <- temporary state
    address <- temporary state
    pure
      ( line [number, fromString " = ashr i64 ", value, fromString ", 1"]
          <> line [address, fromString " = inttoptr i64 ", number, fromString " to ptr"],
        address
      )
  -- Kept as an Int.
  Foreign.Int8 -> untagged
  Foreign.Int16 -> untagged
  Foreign.Int32 -> untagged
  -- Kept as an Integer.
  Foreign.Int64 -> whole
  Foreign.Word -> whole
  Foreign.Word64 -> whole
  Foreign.Word8 -> integral
  Foreign.Word16 -> integral
  Foreign.Word32 -> integral
  Foreign.Unit -> error "a function of C is not given ()"
  where
    untagged = do
      number <- temporary state
      narrow <- temporary state
      pure
        ( line [number, fromString " = ashr i64 ", value, fromString ", 1"]
            <> line [narrow, fromString " = trunc i64 ", number, fromString " to ", cType marshal],
          narrow
        )
    whole = do
      number <- temporary state
      pure (line [number, fromString " = call i64 @hazy_ffi_integer(i64 ", value, fromString ")"], number)
    integral = do
      number <- temporary state
      narrow <- temporary state
      pure
        ( line [number, fromString " = call i64 @hazy_ffi_integer(i64 ", value, fromString ")"]
            <> line [narrow, fromString " = trunc i64 ", number, fromString " to ", cType marshal],
          narrow
        )

-- |
-- What C gives back, as a value: the code of the call and after, and the
-- value.
marshalOut :: State s -> Foreign.Marshal -> Builder -> ST s (Builder, Builder)
marshalOut state marshal invocation = case marshal of
  Foreign.Unit -> do
    value <- local state
    pure (line [invocation] <> line [value, fromString " = call i64 @hazy_ffi_unit()"], value)
  Foreign.Int -> do
    number <- temporary state
    tagged number (line [number, fromString " = ", invocation])
  Foreign.Char -> do
    code <- temporary state
    number <- temporary state
    tagged number $
      line [code, fromString " = ", invocation]
        <> line [number, fromString " = zext i32 ", code, fromString " to i64"]
  Foreign.Double -> do
    number <- temporary state
    value <- local state
    pure
      ( line [number, fromString " = ", invocation]
          <> line [value, fromString " = call i64 @hazy_ffi_from_double(double ", number, fromString ")"],
        value
      )
  Foreign.Float -> do
    single <- temporary state
    double <- temporary state
    value <- local state
    pure
      ( line [single, fromString " = ", invocation]
          <> line [double, fromString " = fpext float ", single, fromString " to double"]
          <> line [value, fromString " = call i64 @hazy_ffi_from_double(double ", double, fromString ")"],
        value
      )
  Foreign.Bool -> do
    truth <- temporary state
    value <- local state
    pure
      ( line [truth, fromString " = ", invocation]
          <> line [value, fromString " = call i64 @hazy_ffi_from_bool(i32 ", truth, fromString ")"],
        value
      )
  Foreign.Pointer -> do
    address <- temporary state
    number <- temporary state
    tagged number $
      line [address, fromString " = ", invocation]
        <> line [number, fromString " = ptrtoint ptr ", address, fromString " to i64"]
  -- Kept as an Int: widened with its sign.
  Foreign.Int8 -> widened "sext"
  Foreign.Int16 -> widened "sext"
  Foreign.Int32 -> widened "sext"
  -- Kept as an Integer.
  Foreign.Int64 -> boxed "hazy_integer" Nothing
  Foreign.Word8 -> boxed "hazy_integer" (Just "zext")
  Foreign.Word16 -> boxed "hazy_integer" (Just "zext")
  Foreign.Word32 -> boxed "hazy_integer" (Just "zext")
  Foreign.Word -> boxed "hazy_ffi_from_unsigned" Nothing
  Foreign.Word64 -> boxed "hazy_ffi_from_unsigned" Nothing
  where
    widened extend = do
      narrow <- temporary state
      number <- temporary state
      tagged number $
        line [narrow, fromString " = ", invocation]
          <> line [number, fromString " = ", fromString extend, fromString " ", cType marshal, fromString " ", narrow, fromString " to i64"]
    boxed make extend = do
      given <- temporary state
      number <- case extend of
        Nothing -> pure given
        Just _ -> temporary state
      value <- local state
      pure
        ( line [given, fromString " = ", invocation]
            <> foldMap (\extend -> line [number, fromString " = ", fromString extend, fromString " ", cType marshal, fromString " ", given, fromString " to i64"]) extend
            <> line [value, fromString " = call i64 @", fromString make, fromString "(i64 ", number, fromString ")"],
          value
        )
    -- A number as a value: shifted left by one, and odd.
    tagged number code = do
      shifted <- temporary state
      value <- localWord state
      pure
        ( code
            <> line [shifted, fromString " = shl i64 ", number, fromString ", 1"]
            <> line [value, fromString " = or i64 ", shifted, fromString ", 1"],
          value
        )

-- |
-- What a unit starts with: what the runtime provides, and forcing.
--
-- Forcing is written here and not in the runtime because it ends in a tail
-- call into the program, which only LLVM can make. It is defined once, in
-- the unit that is the program, and a package only declares it.
prelude :: Shape -> String
prelude = \case
  Whole -> runtime ++ foreigns ++ "\n" ++ areas "internal " ++ "\n" ++ forcing "internal " ++ "\n" ++ mainFunction
  Main -> runtime ++ foreigns ++ "\n" ++ areas "" ++ "\n" ++ forcing "" ++ "\n" ++ mainFunction
  Package -> runtime ++ foreigns ++ "\n" ++ declared

-- |
-- The primitives the runtime has under names of their own.
foreigns :: String
foreigns =
  concat
    [ "declare i64 @hazy_prim_" ++ name ++ "(" ++ concat (intersperse ", " (replicate count "i64")) ++ ")\n"
      | (name, count) <- Primitive.outside
    ]

-- |
-- What the runtime provides, and counting, which every unit has for itself
-- so that it is done in place.
runtime :: String
runtime =
  """
  @hazy_check_due = external global i64
  @hazy_reused = external global i64
  @hazy_lent = external global i64
  @hazy_free_lists = external global [33 x ptr]
  @hazy_alloc_slow = external global i64
  @hazy_allocated = external global i64
  @hazy_freed = external global i64
  @hazy_nullary = external global [8192 x i64]
  declare ptr @hazy_alloc(i64, i64)
  declare void @hazy_free(i64)
  declare void @hazy_give(i64)
  declare void @hazy_split(i64)
  declare void @hazy_settle(i64)
  declare void @hazy_check(ptr, i64)
  declare void @hazy_abort() noreturn
  declare void @hazy_loop() noreturn
  declare void @hazy_finish()
  declare i64 @hazy_text(ptr, i64)
  declare i64 @hazy_integer(i64)
  declare i64 @hazy_integer_big(ptr, i64)
  declare double @hazy_ffi_double(i64)
  declare i64 @hazy_ffi_from_double(double)
  declare i64 @hazy_ffi_unit()
  declare i32 @hazy_ffi_bool(i64)
  declare i64 @hazy_ffi_from_bool(i32)
  declare i64 @hazy_ffi_integer(i64)
  declare i64 @hazy_ffi_from_unsigned(i64)
  declare void @hazy_catch_barrier()
  declare void @hazy_catch_unbarrier()
  declare i64 @hazy_callback_take()
  declare ptr @hazy_wrapper_new(ptr, ptr, i64, i64, i64)
  declare i64 @hazy_prim_error(i64)
  declare i64 @hazy_prim_put_str_ln(i64)
  declare i64 @hazy_prim_text_snoc(i64, i64)
  declare i64 @hazy_prim_text_null(i64)
  declare i64 @hazy_prim_text_head(i64)
  declare i64 @hazy_prim_text_tail(i64)
  declare i64 @hazy_prim_text_append(i64, i64)
  declare i64 @hazy_prim_text_equal(i64, i64)
  declare i64 @hazy_prim_text_compare(i64, i64)
  declare i64 @hazy_prim_text_length(i64)
  declare i64 @hazy_prim_text_take(i64, i64)
  declare i64 @hazy_prim_text_drop(i64, i64)
  declare i64 @hazy_prim_text_is_prefix_of(i64, i64)
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

  ; A new block of a small size, with one reference: the first of the free
  ; blocks of its size, unless there are none or the runtime has more to do.
  define linkonce_odr ptr @hazy_take(i64 %size, i64 %head) alwaysinline {
    %slow = load i64, ptr @hazy_alloc_slow
    %quick = icmp eq i64 %slow, 0
    br i1 %quick, label %look, label %call
  look:
    %slot = getelementptr [33 x ptr], ptr @hazy_free_lists, i64 0, i64 %size
    %block = load ptr, ptr %slot
    %empty = icmp eq ptr %block, null
    br i1 %empty, label %call, label %pop
  pop:
    %link = getelementptr i64, ptr %block, i64 1
    %next = load ptr, ptr %link
    store ptr %next, ptr %slot
    %counted = or i64 %head, 4294967296
    store i64 %counted, ptr %block
    %before = load i64, ptr @hazy_allocated
    %after = add i64 %before, 1
    store i64 %after, ptr @hazy_allocated
    ret ptr %block
  call:
    %made = call ptr @hazy_alloc(i64 %size, i64 %head)
    ret ptr %made
  }

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
  ; A constructed value, a function or a thunk of a small size goes back on
  ; the list of free blocks of its size here. Anything else goes back
  ; through the runtime.
  give:
    %kind = and i64 %header, 15
    %plain = icmp ule i64 %kind, 2
    br i1 %plain, label %sized, label %runtime
  sized:
    %fields = lshr i64 %header, 19
    %entries = and i64 %fields, 8191
    %constructed = icmp eq i64 %kind, 0
    %before = select i1 %constructed, i64 1, i64 2
    %words = add i64 %entries, %before
    %small = icmp ule i64 %words, 32
    br i1 %small, label %push, label %runtime
  push:
    %slot = getelementptr [33 x ptr], ptr @hazy_free_lists, i64 0, i64 %words
    %first = load ptr, ptr %slot
    store i64 15, ptr %pointer
    %link = getelementptr i64, ptr %pointer, i64 1
    store ptr %first, ptr %link
    store ptr %pointer, ptr %slot
    %given = load i64, ptr @hazy_freed
    %more = add i64 %given, 1
    store i64 %more, ptr @hazy_freed
    br label %out
  runtime:
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
-- and where a function puts what it holds when a check is due.
areas :: String -> String
areas linkage =
  concat
    [ "@hazy_spill = " ++ linkage ++ "global [1024 x i64] zeroinitializer\n",
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
  @hazy_spill = external global [1024 x i64]
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
