{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- An abstract machine for AxCut.
--
-- This is the reference for what an AxCut program means, and it is what a
-- code generator is tested against: a program made into native code must
-- print what it prints run here. It is not a runtime. Nothing here is
-- tuned, and values are left to Haskell's garbage collector.
--
-- The environment is kept with its names, which a real machine would not
-- have. 'Substitute' looks values up by name, so an environment that the
-- lowering got out of order is an error here and not a wrong answer.
module AxCut.Machine (run) where

import AxCut.Tree
  ( Clause (..),
    Method (..),
    Object (..),
    Program (..),
    Recursive (..),
    Statement (..),
  )
import qualified AxCut.Tree as Kind (Kind (..))
import Data.Bits (popCount, shift, xor, (.&.), (.|.))
import qualified Core.Foreign as Foreign
import Data.Char (chr, generalCategory, isAlpha, isDigit, isSpace, ord, toLower, toUpper)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Foldable (for_)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr)
import qualified System.Directory as Directory
import Data.Word (Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Data.List (sort)
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Traversable (for)
import Generate.Global (Global)
import Sequent.Primitive (Primitive (..))
import qualified Sequent.Primitive as Primitive
import qualified Sequent.Runtime as Runtime
import qualified Sequent.Tree as Literal (Literal (..))
import qualified Sequent.Tree as Tree (int63, intMaxBound, intMinBound)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, removeFile)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import System.IO
  ( Handle,
    IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode),
    SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd),
    hClose,
    hFileSize,
    hFlush,
    hGetBuf,
    hGetBufSome,
    hGetChar,
    hIsEOF,
    hIsReadable,
    hIsTerminalDevice,
    hIsWritable,
    hLookAhead,
    hPutBuf,
    hPutStrLn,
    hSeek,
    hSetEncoding,
    hSetFileSize,
    hSetNewlineMode,
    hTell,
    noNewlineTranslation,
    openFile,
    stderr,
    stdin,
    stdout,
    utf8
  )
import System.IO.Error (alreadyExistsErrorType, ioeSetErrorString, isAlreadyExistsError, isDoesNotExistError, isPermissionError, mkIOError, tryIOError)
import System.Info (os)

data Value
  = Int !Int
  | Integer !Integer
  | Text !Text
  | Data !Int [Value]
  | Reference !(IORef Cell)
  | -- | A place that holds a value, which can be changed for another.
    Box !(IORef Value)
  | -- | Places one after another, each of which can be changed.
    Array !(IORef (Vector Value))
  | -- | A floating-point number. A Float is kept as the Double it is.
    Floating !Double

-- |
-- What a program has of the world outside while it runs: what catches a
-- thrown value, the catch begun last first, the files that are open and
-- what each was opened as, and what was said of the last that could not be.
data World = World
  { -- | Each catch's handler, where it goes on, and how many thunks were
    -- being worked out when it began.
    catching :: !(IORef [(Value, Value, Int)]),
    -- | The thunks being worked out, the last begun first.
    working :: !(IORef [IORef Cell]),
    files :: !(IORef (Map Int (Text, Handle))),
    failure :: !(IORef Text),
    -- | The arguments and the name of the program, which it can change for
    -- a while.
    arguments :: !(IORef [Text]),
    programName :: !(IORef Text),
    -- | The texts that are being built, each as its pieces, the last first.
    building :: !(IORef (Map Int [Text])),
    -- | The memory that C's library gives out: the bytes that have been
    -- written, each at its address, and the blocks given out, each at its
    -- address with its size. A byte that was never written is 0.
    memory :: !(IORef (Map Int Word8)),
    blocks :: !(IORef (Map Int Int)),
    -- | The addresses that the things of C whose addresses were asked for
    -- were given, each by its name. A function of C called by its address
    -- is called by that name.
    symbols :: !(IORef (Map Text Int)),
    -- | What C says went wrong last, which the machine says for what it
    -- does in C's place.
    errno :: !(IORef Int),
    -- | The definitions that C calls by a name, as foreign exports say;
    -- and what the last call into the program left for C.
    exported :: Map Text (IORef Cell),
    returned :: !(IORef Value),
    -- | The functions of C that wrappers made, each by its address, with
    -- the code that applies the function to what C gives, and the function.
    wrappers :: !(IORef (Map Int (Value, Value))),
    -- | Run a function of the program, as C calls it, on what C gave, and
    -- give back what it left.
    enter :: IORef Cell -> [Value] -> IO Value
  }

-- |
-- What an object is at the moment.
data Cell
  = -- | A function or a consumer: its method, the method's parameters and
    -- body, and what was captured.
    Closure !Method [Text] Statement Environment
  | -- | A thunk that has not been forced.
    Delayed [Text] Statement Environment
  | -- | A thunk that is being forced.
    Working
  | -- | A thunk that has been forced.
    Done Value
  | -- | A thunk that threw, the throw having been caught: a thunk that
    -- throws the same again.
    Again Value
  | -- | The consumer a thunk's body is given: it keeps the value in the
    -- thunk and then hands it to the consumer that asked.
    Update !(IORef Cell) Value

-- |
-- The row of values. The last value is last. What a closure or a thunk
-- captured is one.
type Environment = [(Text, Value)]

type Globals = Map Global (IORef Cell)

run :: Program -> IO ()
run Program {definitions, entry} = do
  globals <- traverse (\object -> allocate object []) definitions
  catching <- newIORef []
  working <- newIORef []
  files <- newIORef Map.empty
  failure <- newIORef Text.empty
  arguments <- newIORef []
  programName <- newIORef (Text.pack "<interactive>")
  building <- newIORef Map.empty
  memory <- newIORef Map.empty
  blocks <- newIORef Map.empty
  symbols <- newIORef Map.empty
  errno <- newIORef 0
  returned <- newIORef unit
  wrappers <- newIORef Map.empty
  let world = World {catching, working, files, failure, arguments, programName, building, memory, blocks, symbols, errno, exported, returned, wrappers, enter}
      exported =
        Map.fromList
          [ (symbol, cell)
            | Object {body} <- Map.elems definitions,
              (Foreign.Call {symbol}, global) <- exportsIn body,
              Just cell <- [Map.lookup global globals]
          ]
      enter = callback world globals
  execute world globals entry []

-- |
-- Where C calls definitions by a name, as foreign exports say.
exportsIn :: Statement -> [(Foreign.Call, Global)]
exportsIn = \case
  Substitute _ rest -> exportsIn rest
  Let _ _ _ rest -> exportsIn rest
  Switch clauses fallback -> concat [exportsIn body | Clause _ _ body <- clauses] ++ foldMap exportsIn fallback
  New _ Object {body} rest -> exportsIn body ++ exportsIn rest
  Literal _ _ rest -> exportsIn rest
  Global _ _ _ rest -> exportsIn rest
  Primitive _ (Export call global) rest -> (call, global) : exportsIn rest
  Primitive _ _ rest -> exportsIn rest
  Rec objects rest -> concat [exportsIn body | Recursive {object = Object {body}} <- objects] ++ exportsIn rest
  Invoke _ -> []
  Force -> []
  Abort -> []
  Halt -> []

-- |
-- Run a function of the program that C calls, on what C gave, until it
-- stops, and give back what it left. A catch begun before C was called is
-- out of its reach.
callback :: World -> Globals -> IORef Cell -> [Value] -> IO Value
callback world@World {catching, returned} globals cell given =
  readIORef cell >>= \case
    Closure Apply parameters body environment
      | length parameters == length given + 1 -> do
          outer <- readIORef catching
          writeIORef catching []
          execute world globals body (zip parameters (given ++ [unit]) ++ environment)
          writeIORef catching outer
          readIORef returned
    _ -> bug "what C calls is not a function of what C gives"

allocate :: Object -> Environment -> IO (IORef Cell)
allocate Object {kind, method, parameters, body} environment = newIORef $ case kind of
  Kind.Closure -> Closure method parameters body environment
  Kind.Join -> Closure method parameters body environment
  Kind.Thunk -> Delayed parameters body environment

bug :: String -> IO a
bug message = do
  hPutStrLn stderr $ "AxCut machine: " ++ message
  exitFailure

-- |
-- Split the last values off an environment.
splitEnd :: Int -> Environment -> IO (Environment, Environment)
splitEnd count environment
  | count > length environment = bug "the environment is too short"
  | otherwise = pure $ splitAt (length environment - count) environment

unit, false, true :: Value
unit = Data 0 []
false = Data 0 []
true = Data 1 []

bool :: Bool -> Value
bool = \case
  False -> false
  True -> true

execute :: World -> Globals -> Statement -> Environment -> IO ()
execute world globals = go
  where
    go :: Statement -> Environment -> IO ()
    go statement environment = case statement of
      Substitute pairs rest -> do
        environment <- for pairs $ \(new, old) -> case lookup old environment of
          Just value -> pure (new, value)
          Nothing -> bug $ unpack old ++ " is not in the environment"
        go rest environment
      Let name tag count rest -> do
        (kept, taken) <- splitEnd count environment
        go rest (kept ++ [(name, Data tag (map snd taken))])
      Switch clauses fallback -> do
        (kept, taken) <- splitEnd 1 environment
        case taken of
          [(_, Data tag fields)]
            | (names, body) : _ <- [(names, body) | Clause tag' names body <- clauses, tag == tag'] ->
                if length names == length fields
                  then go body (kept ++ zip names fields)
                  else bug "a clause has the wrong number of fields"
            | Just body <- fallback -> go body kept
            | otherwise -> bug $ "no clause for tag " ++ show tag
          _ -> bug "switch on something that is not constructed"
      New name object rest -> do
        (kept, taken) <- splitEnd (length (captured object)) environment
        cell <- allocate object (zip (captured object) (map snd taken))
        go rest (kept ++ [(name, Reference cell)])
      Invoke method -> do
        (arguments, taken) <- splitEnd 1 environment
        case (method, map snd arguments, taken) of
          (Return, [value], [(_, consumer)]) -> deliver value consumer
          (Apply, values, [(_, Reference cell)]) ->
            readIORef cell >>= \case
              Closure Apply parameters body environment
                | length parameters == length values ->
                    go body (zip parameters values ++ environment)
                | otherwise -> bug "a function was given the wrong number of arguments"
              _ -> bug "apply of something that is not a function"
          -- A function given its arguments in turn: given the first, and
          -- a consumer that gives the rest to what it gives.
          (Spread, first : rest@(_ : _), [(_, function)]) -> do
            let names = [Text.pack ("spread" ++ show index) | index <- [1 .. length rest]]
                given = Text.pack "given"
                onward = if length rest == 2 then Apply else Spread
                body = Substitute [(name, name) | name <- names ++ [given]] (Invoke onward)
            cell <- newIORef (Closure Return [given] body (zip names rest))
            go (Invoke Apply) [(Text.pack "first", first), (Text.pack "consumer", Reference cell), (Text.pack "function", function)]
          _ -> bug $ "bad invoke " ++ show method
      Force -> case environment of
        [(_, consumer), (_, value)] -> force value consumer
        _ -> bug "force with the wrong environment"
      Literal name literal rest ->
        let value = case literal of
              Literal.Int int -> Int int
              Literal.Integer integer -> Integer integer
              Literal.Text text -> Text text
              Literal.Double double -> Floating double
         in go rest (environment ++ [(name, value)])
      Global name global _ rest -> case Map.lookup global globals of
        Just cell -> go rest (environment ++ [(name, Reference cell)])
        Nothing -> bug $ "undefined global: " ++ Runtime.showGlobal global
      Primitive name primitive rest -> do
        (kept, taken) <- splitEnd (Primitive.arity primitive) environment
        value <- apply world primitive (map snd taken)
        go rest (kept ++ [(name, value)])
      Rec objects rest -> do
        let mine Recursive {object, others} = length (captured object) - length others
        (kept, taken) <- splitEnd (sum (map mine objects)) environment
        cells <- for objects $ \_ -> newIORef Working
        let new = zip [name | Recursive {name} <- objects] (map Reference cells)
            fill _ [] [] = pure ()
            fill taken (recursive@Recursive {object, others} : objects) (cell : cells) = do
              let (own, later) = splitAt (mine recursive) taken
              shared <- for others $ \other -> case lookup other new of
                Just value -> pure value
                Nothing -> bug $ unpack other ++ " is not one of the objects"
              filled <- allocate object (zip (captured object) (map snd own ++ shared))
              readIORef filled >>= writeIORef cell
              fill later objects cells
            fill _ _ _ = bug "objects and cells do not line up"
        fill taken objects cells
        go rest (kept ++ new)
      Abort -> do
        hPutStrLn stderr "bottom"
        exitFailure
      Halt -> pure ()

    -- Evaluate something that may be a thunk and give the value to a
    -- consumer.
    force :: Value -> Value -> IO ()
    force value consumer = case value of
      Reference cell ->
        readIORef cell >>= \case
          Delayed parameters body environment -> do
            writeIORef cell Working
            modifyIORef (working world) (cell :)
            update <- newIORef (Update cell consumer)
            go body (zip parameters [Reference update] ++ environment)
          Done value -> deliver value consumer
          Again thrower -> force thrower consumer
          Working -> do
            hPutStrLn stderr "<<loop>>"
            exitFailure
          Closure {} -> deliver value consumer
          Update {} -> bug "force of a consumer"
      _ -> deliver value consumer

    -- Give a value to a consumer.
    deliver :: Value -> Value -> IO ()
    deliver value = \case
      Reference cell ->
        readIORef cell >>= \case
          Closure Return [parameter] body environment ->
            go body ((parameter, value) : environment)
          Update thunk consumer -> do
            writeIORef thunk (Done value)
            modifyIORef (working world) (drop 1)
            deliver value consumer
          _ -> bug "a value was given to something that is not a consumer"
      _ -> bug "a value was given to something that is not an object"

apply :: World -> Primitive -> [Value] -> IO Value
apply world primitive arguments = case (primitive, arguments) of
  (Foreign name _, _) -> outside world (unpack name) arguments
  (Call call, _) -> callC world call arguments
  (Export _ _, _) -> pure unit
  (Wrap _, [entry, function]) -> do
    made <- readIORef (wrappers world)
    let address = maybe 0x40000000 ((+ 16) . fst) (Map.lookupMax made)
    modifyIORef (wrappers world) (Map.insert address (entry, function))
    pure (Int (fromIntegral address))
  (Field index, [Data _ fields])
    | index < length fields -> pure $ fields !! index
  (Error, [Text message]) -> do
    hPutStrLn stderr (unpack message)
    exitFailure
  (PutStrLn, [Text text]) -> do
    Text.IO.putStrLn text
    pure unit
  (TextSnoc, [Text text, Int character]) ->
    pure $ Text $ Text.snoc text $ chr $ fromIntegral character
  (TextNull, [Text text]) -> pure $ bool $ Text.null text
  (TextHead, [Text text]) -> pure $ Int $ fromIntegral $ ord $ Text.head text
  (TextTail, [Text text]) -> pure $ Text $ Text.tail text
  (TextAppend, [Text left, Text right]) -> pure $ Text $ left <> right
  (TextEqual, [Text left, Text right]) -> pure $ bool $ left == right
  (TextCompare, [Text left, Text right]) -> pure $ Int $ case compare left right of
    LT -> -1
    EQ -> 0
    GT -> 1
  (TextLength, [Text text]) -> pure $ Int $ fromIntegral $ Text.length text
  (TextTake, [Int count, Text text]) -> pure $ Text $ Text.take (fromIntegral count) text
  (TextDrop, [Int count, Text text]) -> pure $ Text $ Text.drop (fromIntegral count) text
  (TextIsPrefixOf, [Text prefix, Text text]) -> pure $ bool $ Text.isPrefixOf prefix text
  (GeneralCategory, [Int character]) ->
    pure $ Data (fromEnum $ generalCategory $ chr $ fromIntegral character) []
  (IntEqual, [Int left, Int right]) -> pure $ bool $ left == right
  (IntLessThenEqual, [Int left, Int right]) -> pure $ bool $ left <= right
  (IntAdd, [Int left, Int right]) -> pure $ Int $ wrapped $ left + right
  (IntMinus, [Int left, Int right]) -> pure $ Int $ wrapped $ left - right
  (IntMultiply, [Int left, Int right]) -> pure $ Int $ wrapped $ left * right
  (IntNegate, [Int value]) -> pure $ Int $ wrapped $ negate value
  (IntAbs, [Int value]) -> pure $ Int $ wrapped $ abs value
  (IntSignum, [Int value]) -> pure $ Int $ signum value
  (IntQuot, [Int left, Int right])
    | right == 0 -> divideByZero
    -- The one quotient that does not fit wraps around.
    | right == -1 -> pure $ Int $ wrapped $ negate left
    | otherwise -> pure $ Int $ left `quot` right
  (IntRem, [Int left, Int right])
    | right == 0 -> divideByZero
    | right == -1 -> pure $ Int 0
    | otherwise -> pure $ Int $ left `rem` right
  (IntDiv, [Int left, Int right])
    | right == 0 -> divideByZero
    | right == -1 -> pure $ Int $ wrapped $ negate left
    | otherwise -> pure $ Int $ left `div` right
  (IntMod, [Int left, Int right])
    | right == 0 -> divideByZero
    | right == -1 -> pure $ Int 0
    | otherwise -> pure $ Int $ left `mod` right
  (IntToInteger, [Int value]) -> pure $ Integer $ fromIntegral value
  (IntegerCastToInt, [Integer value])
    | value > toInteger Tree.intMaxBound || value < toInteger Tree.intMinBound -> do
        hPutStrLn stderr "Enum Integer Overflow"
        exitFailure
    | otherwise -> pure $ Int $ fromInteger value
  (IntegerTruncateToInt, [Integer value]) -> pure $ Int $ Tree.int63 value
  (IntegerEqual, [Integer left, Integer right]) -> pure $ bool $ left == right
  (IntegerLessThenEqual, [Integer left, Integer right]) -> pure $ bool $ left <= right
  (IntegerAdd, [Integer left, Integer right]) -> pure $ Integer $ left + right
  (IntegerMinus, [Integer left, Integer right]) -> pure $ Integer $ left - right
  (IntegerMultiply, [Integer left, Integer right]) -> pure $ Integer $ left * right
  (IntegerNegate, [Integer value]) -> pure $ Integer $ negate value
  (IntegerAbs, [Integer value]) -> pure $ Integer $ abs value
  (IntegerSignum, [Integer value]) -> pure $ Integer $ signum value
  (IntegerQuot, [Integer left, Integer right])
    | right == 0 -> divideByZero
    | otherwise -> pure $ Integer $ left `quot` right
  (IntegerRem, [Integer left, Integer right])
    | right == 0 -> divideByZero
    | otherwise -> pure $ Integer $ left `rem` right
  _ -> bug $ "bad arguments to " ++ show primitive
  where
    divideByZero = do
      hPutStrLn stderr "divide by zero"
      exitFailure
    -- An Int is 63 bits, as in native code, and comes round at its bounds.
    wrapped = Tree.int63 . toInteger

-- |
-- A floating-point number of a precision: 32 for a Float, worked out as a
-- Float, and 64 for a Double.
precise :: Int -> Double -> Float -> Value
precise precision double float
  | precision == 32 = Floating (realToFrac float)
  | otherwise = Floating double

single :: Double -> Float
single = realToFrac

-- | The functions of floating point, by number, as the runtime has them.
function :: (Floating a) => Int -> a -> a
function = \case
  3 -> exp
  4 -> log
  5 -> sqrt
  6 -> sin
  7 -> cos
  8 -> tan
  9 -> asin
  10 -> acos
  11 -> atan
  12 -> sinh
  13 -> cosh
  14 -> tanh
  15 -> asinh
  16 -> acosh
  17 -> atanh
  operation -> error $ "no such function of floating point: " ++ show operation

operator :: (Floating a) => Int -> a -> a -> a
operator = \case
  0 -> (+)
  1 -> (-)
  2 -> (*)
  3 -> (/)
  4 -> (**)
  operation -> error $ "no such operator of floating point: " ++ show operation

-- |
-- What a place of an array holds.
placed :: IORef (Vector Value) -> Int -> IO Value
placed array index = do
  places <- readIORef array
  case places Vector.!? fromIntegral index of
    Just value -> pure value
    Nothing -> bug $ "index " ++ show index ++ " is out of the bounds of an array of " ++ show (Vector.length places)

-- |
-- A function of C, as the machine does it. The machine cannot call C, and
-- knows only some of the functions of its library: those of numbers and of
-- characters, which give the same in either.
callC :: World -> Foreign.Call -> [Value] -> IO Value
callC World {symbols} Foreign.Call {target = Foreign.Address, symbol} _ = do
  known <- readIORef symbols
  case Map.lookup symbol known of
    Just address -> pure (Int (fromIntegral address))
    Nothing -> do
      let address = 0x100000 + 16 * Map.size known
      modifyIORef symbols (Map.insert symbol address)
      pure (Int (fromIntegral address))
callC world@World {symbols, wrappers, enter} call@Foreign.Call {target = Foreign.Dynamic} (Int address : given) = do
  known <- readIORef symbols
  made <- readIORef wrappers
  case [name | (name, at) <- Map.toList known, at == fromIntegral address] of
    _ | Just (Reference entry, function) <- Map.lookup (fromIntegral address) made -> enter entry (function : given)
    name : _ -> callC world call {Foreign.target = Foreign.Static, Foreign.symbol = name, Foreign.arguments = drop 1 (Foreign.arguments call)} given
    [] -> do
      hPutStrLn stderr "the machine cannot call a function of C at an address it did not give out"
      exitFailure
callC World {exported, enter} Foreign.Call {target = Foreign.Static, symbol} given
  | Just cell <- Map.lookup symbol exported = enter cell given
callC World {wrappers} Foreign.Call {target = Foreign.Static, symbol} [Int address]
  | symbol == Text.pack "hazy_wrapper_free" = unit <$ modifyIORef wrappers (Map.delete (fromIntegral address))
callC world@World {memory} Foreign.Call {target = Foreign.Static, symbol} [Int base, count, size, Int compare]
  | symbol == Text.pack "qsort" = do
      let whole = \case
            Int number -> fromIntegral number
            Integer number -> fromInteger number
            _ -> 0
          width = whole size :: Int
          at index = fromIntegral base + index * width
          element index = do
            bytes <- readIORef memory
            pure [Map.findWithDefault 0 (at index + offset) bytes | offset <- [0 .. width - 1]]
          put index element =
            modifyIORef memory $ \bytes ->
              foldr (\(offset, byte) -> Map.insert (at index + offset) byte) bytes (zip [0 ..] element)
          comparing =
            Foreign.Call
              { target = Foreign.Dynamic,
                symbol = Text.empty,
                arguments = [Foreign.Pointer, Foreign.Pointer, Foreign.Pointer],
                result = Foreign.Int32,
                action = True
              }
          insert index
            | index <= 0 = pure ()
            | otherwise = do
                order <- callC world comparing [Int compare, Int (fromIntegral (at (index - 1))), Int (fromIntegral (at index))]
                case order of
                  Int order | order > 0 -> do
                    before <- element (index - 1)
                    after <- element index
                    put (index - 1) after
                    put index before
                    insert (index - 1)
                  _ -> pure ()
      for_ [1 .. whole count - 1] insert
      pure unit
callC World {memory, blocks, errno, files} Foreign.Call {symbol, result} given = case (unpack symbol, given) of
  -- Memory, as C's library gives it out. A block is put after the last,
  -- on a boundary of 16 bytes, and none is used again.
  ("malloc", [Int size]) -> Int <$> allocate size
  ("calloc", [Int count, Int size]) -> Int <$> allocate (count * size)
  ("realloc", [Int old, Int size]) -> do
    new <- allocate size
    known <- readIORef blocks
    copy new old (min size (Map.findWithDefault 0 (fromIntegral old) known))
    pure $ Int new
  ("free", [Int address]) -> unit <$ modifyIORef blocks (Map.delete (fromIntegral address))
  ("memcpy", [Int target, Int source, Int count]) -> unit <$ copy target source count
  ("memmove", [Int target, Int source, Int count]) -> unit <$ copy target source count
  ("hazy_cast_u32_f32", [Integer bits]) -> pure $ Floating $ realToFrac $ castWord32ToFloat (fromInteger bits)
  ("hazy_cast_f32_u32", [Floating x]) -> pure $ Integer $ toInteger $ castFloatToWord32 (realToFrac x)
  ("hazy_cast_u64_f64", [Integer bits]) -> pure $ Floating $ castWord64ToDouble (fromInteger bits)
  ("hazy_cast_f64_u64", [Floating x]) -> pure $ Integer $ toInteger $ castDoubleToWord64 x
  ("strlen", [Int address]) -> Integer . toInteger . length <$> string address
  -- Bytes written from memory to a file, and read from one into memory,
  -- through the handles the machine has.
  ("hazy_put_buf", [Int descriptor, Int address, Int count]) -> do
    handle <- handleOf descriptor
    bytes <- for [0 .. count - 1] (\offset -> readBytes (address + offset) 1)
    allocaBytes (fromIntegral count) $ \buffer -> do
      pokeArray buffer (map fromInteger bytes :: [Word8])
      hPutBuf handle buffer (fromIntegral count)
    pure unit
  ("hazy_get_buf", [Int descriptor, Int address, Int count, Int some]) -> do
    handle <- handleOf descriptor
    bytes <- allocaBytes (fromIntegral count) $ \buffer -> do
      read' <- (if some /= 0 then hGetBufSome else hGetBuf) handle buffer (fromIntegral count)
      peekArray read' (buffer :: Ptr Word8)
    for_ (zip [0 ..] bytes) (\(offset, byte) -> write (address + offset) 1 (toInteger byte))
    pure (Int (fromIntegral (length bytes)))
  -- C's words for a number are not known.
  ("hazy_errno", []) -> Int . fromIntegral <$> readIORef errno
  ("hazy_set_errno", [Int number]) -> unit <$ writeIORef errno (fromIntegral number)
  -- What the runtime asks of the system for System.Directory, done with
  -- the System.Directory the machine is made with.
  ("hazy_remove_directory", [Int path]) -> status (pathOf path >>= Directory.removeDirectory)
  ("hazy_remove_directory_recursive", [Int path]) -> status (pathOf path >>= Directory.removeDirectoryRecursive)
  ("hazy_rename", [Int from, Int to]) -> status (Directory.renamePath <$> pathOf from <*> pathOf to >>= id)
  ("hazy_current_directory", []) -> made Directory.getCurrentDirectory
  ("hazy_set_current_directory", [Int path]) -> status (pathOf path >>= Directory.setCurrentDirectory)
  ("hazy_home_directory", []) -> made Directory.getHomeDirectory
  ("hazy_temporary_directory", []) -> made Directory.getTemporaryDirectory
  ("hazy_copy_file", [Int from, Int to]) -> status (Directory.copyFile <$> pathOf from <*> pathOf to >>= id)
  ("hazy_file_size", [Int path]) -> sizeOf path
  ("hazy_canonicalize", [Int path]) -> made (pathOf path >>= Directory.canonicalizePath)
  ("hazy_create_link", [Int target, Int link, Int directory]) ->
    status $ do
      target' <- pathOf target
      link' <- pathOf link
      (if directory /= 0 then Directory.createDirectoryLink else Directory.createFileLink) target' link'
  ("hazy_remove_link", [Int path]) -> status (pathOf path >>= Directory.removeDirectoryLink)
  ("hazy_read_link", [Int path]) -> made (pathOf path >>= Directory.getSymbolicLinkTarget)
  ("hazy_is_link", [Int path]) -> answer (fromEnum <$> (pathOf path >>= Directory.pathIsSymbolicLink))
  ("hazy_permissions", [Int path]) -> answer $ do
    permissions <- pathOf path >>= Directory.getPermissions
    pure $ sum [bit | (bit, True) <- zip [1, 2, 4, 8] (map ($ permissions) [Directory.readable, Directory.writable, Directory.executable, Directory.searchable])]
  ("hazy_set_permissions", [Int path, Int bits]) -> status $ do
    path' <- pathOf path
    let has bit = bits `div` bit `mod` 2 == 1
    Directory.setPermissions path' $
      Directory.setOwnerReadable (has 1) $
        Directory.setOwnerWritable (has 2) $
          Directory.setOwnerExecutable (has 4) $
            Directory.setOwnerSearchable (has 8) Directory.emptyPermissions
  ("strerror", [Int number]) -> do
    let text = map (fromIntegral . fromEnum) ("errno " ++ show number) ++ [0 :: Integer]
    address <- allocate (length text)
    for_ (zip [0 ..] text) (\(offset, byte) -> write (address + offset) 1 byte)
    pure (Int address)
  ("strcmp", [Int left, Int right]) -> do
    left <- string left
    right <- string right
    pure $ Int $ case compare left right of
      LT -> -1
      EQ -> 0
      GT -> 1
  ("memchr", [Int address, Int byte, Int count]) -> do
    bytes <- for [0 .. count - 1] (\offset -> readBytes (address + offset) 1)
    pure $ Int $ case lookup (toInteger byte `mod` 256) (zip bytes [0 ..]) of
      Just offset -> address + offset
      Nothing -> 0
  ("memset", [Int target, Int byte, Int count]) ->
    unit <$ for_ [0 .. count - 1] (\offset -> write (target + offset) 1 (toInteger byte))
  -- What the runtime reads and writes memory with.
  ('h' : 'a' : 'z' : 'y' : '_' : 'p' : 'e' : 'e' : 'k' : '_' : what, [Int address]) -> case what of
    "f32" -> Floating . realToFrac . castWord32ToFloat . fromInteger <$> readBytes address 4
    "f64" -> Floating . castWord64ToDouble . fromInteger <$> readBytes address 8
    "ptr" -> Int . fromInteger <$> readBytes address 8
    'i' : bits -> signed (read bits) <$> readBytes address (read bits `div` 8)
    _ -> Integer <$> readBytes address (read (drop 1 what) `div` 8)
  ('h' : 'a' : 'z' : 'y' : '_' : 'p' : 'o' : 'k' : 'e' : '_' : what, [Int address, value]) ->
    unit <$ case (what, value) of
      ("f32", Floating x) -> write address 4 (toInteger (castFloatToWord32 (realToFrac x)))
      ("f64", Floating x) -> write address 8 (toInteger (castDoubleToWord64 x))
      (_, Int n) -> write address (width what) (toInteger n)
      (_, Integer n) -> write address (width what) n
      _ -> bug "a value of the wrong kind was written to memory"
  (name, [Floating x])
    | Just f <- lookup name doubles -> pure $ floating (f x)
    | Just f <- single name -> pure $ floating (f x)
  (name, [Floating x, Floating y])
    | Just f <- lookup name doubles2 -> pure $ floating (f x y)
    | Just f <- lookup name singles2 -> pure $ floating (f x y)
  ("labs", [Int n]) -> pure $ Int (abs n)
  ("llabs", [Int n]) -> pure $ Int (abs n)
  ("abs", [Int n]) -> pure $ Int (abs n)
  ("llabs", [Integer n]) -> pure $ Integer (abs n)
  ("toupper", [Integer c]) -> pure $ Integer (fromIntegral (ord (toUpper (chr (fromIntegral c)))))
  ("isdigit", [Int c]) -> pure $ bool (isDigit (chr (fromIntegral c)))
  ("isalpha", [Int c]) -> pure $ bool (isAlpha (chr (fromIntegral c)))
  ("isspace", [Int c]) -> pure $ bool (isSpace (chr (fromIntegral c)))
  ("toupper", [Int c]) -> pure $ Int (fromIntegral (ord (toUpper (chr (fromIntegral c)))))
  ("tolower", [Int c]) -> pure $ Int (fromIntegral (ord (toLower (chr (fromIntegral c)))))
  ("srand", [_]) -> pure unit
  (name, _) -> do
    hPutStrLn stderr ("the machine cannot call the C function " ++ name ++ "; compile the program to native code to call it")
    exitFailure
  where
    -- Kept as an Int if it is narrower than an Int, and as an Integer if
    -- it is not.
    signed :: Int -> Integer -> Value
    signed bits n
      | bits < 64 = Int (fromInteger value)
      | otherwise = Integer value
      where
        value = if n >= 2 ^ (bits - 1) then n - 2 ^ bits else n
    width what = case what of
      "ptr" -> 8
      _ -> read (drop 1 what) `div` 8
    allocate size = do
      known <- readIORef blocks
      let next = case Map.lookupMax known of
            Nothing -> 65536
            Just (address, taken) -> (address + max taken 1 + 15) `div` 16 * 16
      modifyIORef blocks (Map.insert next (fromIntegral size))
      pure (fromIntegral next)
    copy target source count = do
      bytes <- for [0 .. count - 1] (\offset -> readBytes (source + offset) 1)
      for_ (zip [0 ..] bytes) (\(offset, byte) -> write (target + offset) 1 byte)
    -- What the system says went wrong, as C would say it.
    failed :: IOException -> IO ()
    failed problem = writeIORef errno (number problem)
    number problem
      | isDoesNotExistError problem = 2
      | isAlreadyExistsError problem = 17
      | isPermissionError problem = 13
      | otherwise = 5
    -- 0 if the action was done, and -1 if it went wrong.
    status action = answer (0 <$ action)
    answer :: IO Int -> IO Value
    answer action = do
      done <- tryIOError action
      case done of
        Right number -> pure (Int (fromIntegral number))
        Left problem -> Int (-1) <$ failed problem
    sizeOf path = do
      done <- tryIOError (pathOf path >>= Directory.getFileSize)
      case done of
        Right size -> pure (Integer size)
        Left problem -> Integer (-1) <$ failed problem
    -- A string made in the machine's memory, as C's malloc would make it,
    -- or a null address if the action went wrong.
    made action = do
      done <- tryIOError action
      case done of
        Right text -> do
          let bytes = map toInteger (encodeUtf8' text) ++ [0]
          address <- allocate (length bytes)
          for_ (zip [0 ..] bytes) (\(offset, byte) -> write (address + offset) 1 byte)
          pure (Int address)
        Left problem -> Int 0 <$ failed problem
    -- A path given as a string of C in UTF-8.
    pathOf address = decodeUtf8' . map fromInteger <$> string address
    -- The handle of a file the program has open.
    handleOf descriptor = case descriptor of
      0 -> pure stdin
      1 -> pure stdout
      2 -> pure stderr
      _ -> do
        open <- readIORef files
        case Map.lookup (fromIntegral descriptor) open of
          Just (_, handle) -> pure handle
          Nothing -> bug "a file that is not open"
    -- The bytes from an address up to the first 0.
    string address = do
      byte <- readBytes address 1
      if byte == 0 then pure [] else (byte :) <$> string (address + 1)
    -- What is at an address, little-endian.
    readBytes address count = do
      bytes <- readIORef memory
      pure $ foldr (\offset n -> n * 256 + toInteger (Map.findWithDefault 0 (fromIntegral address + offset) bytes)) 0 [0 .. fromIntegral count - 1]
    write address count n =
      modifyIORef memory $ \bytes ->
        foldr (\offset -> Map.insert (fromIntegral address + offset) (fromInteger ((n `div` 2 ^ (8 * offset)) `mod` 256))) bytes [0 .. fromIntegral count - 1]
    floating x = Floating $ case result of
      Foreign.Float -> realToFrac (realToFrac x :: Float)
      _ -> x
    -- A function of a Float is the one of a Double with an f at the end,
    -- worked out in single precision.
    single name = case reverse name of
      'f' : rest | Just f <- lookup (reverse rest) doubles -> Just (inSingle f)
      _ -> Nothing
    inSingle :: (Double -> Double) -> Double -> Double
    inSingle f x = realToFrac (realToFrac (f (realToFrac (realToFrac x :: Float))) :: Float)
    singles2 = [(name ++ "f", \x y -> realToFrac (realToFrac (f x y) :: Float)) | (name, f) <- doubles2]
    doubles =
      [ ("sin", sin),
        ("cos", cos),
        ("tan", tan),
        ("asin", asin),
        ("acos", acos),
        ("atan", atan),
        ("sinh", sinh),
        ("cosh", cosh),
        ("tanh", tanh),
        ("exp", exp),
        ("log", log),
        ("log10", logBase 10),
        ("sqrt", sqrt),
        ("fabs", abs),
        ("floor", fromInteger . floor),
        ("ceil", fromInteger . ceiling),
        ("trunc", fromInteger . truncate),
        ("round", \x -> fromInteger (if x < 0 then negate (floor (0.5 - x)) else floor (x + 0.5)))
      ]
    doubles2 =
      [ ("pow", (**)),
        ("atan2", atan2),
        ("hypot", \x y -> sqrt (x * x + y * y)),
        ("fmin", min),
        ("fmax", max)
      ]

-- |
-- What the runtime does under a name of its own, as the machine does it.
outside :: World -> String -> [Value] -> IO Value
outside World {catching, working, files, failure, building, arguments, programName, returned} name given = case (name, given) of
  ("callback_result", [value]) -> unit <$ writeIORef returned value
  ("new_ref", [value]) -> Box <$> newIORef value
  ("read_ref", [Box box]) -> readIORef box
  ("write_ref", [Box box, value]) -> unit <$ writeIORef box value
  ("same_ref", [Box left, Box right]) -> pure $ bool $ left == right
  ("array_new", [Int count, fill]) -> Array <$> newIORef (Vector.replicate (max 0 (fromIntegral count)) fill)
  ("array_read", [Array array, Int index]) -> placed array index
  ("array_index", [Array array, Int index]) -> placed array index
  ("array_write", [Array array, Int index, value]) -> do
    _ <- placed array index
    unit <$ modifyIORef array (Vector.// [(fromIntegral index, value)])
  ("array_length", [Array array]) -> Int . fromIntegral . Vector.length <$> readIORef array
  ("float_from_integer", [Int precision, Integer number]) -> pure $ precise precision (fromInteger number) (fromInteger number)
  ("float_from_int", [Int precision, Int number]) -> pure $ precise precision (fromIntegral number) (fromIntegral number)
  ("float_unary", [Int precision, Int operation, Floating x]) -> pure $ case operation of
    0 -> Floating (negate x)
    1 -> Floating (abs x)
    2 -> Floating (if x > 0 then 1 else if x < 0 then -1 else x)
    _ -> precise precision (function operation x) (function operation (single x))
  ("float_binary", [Int precision, Int operation, Floating x, Floating y]) ->
    pure $ precise precision (operator operation x y) (operator operation (single x) (single y))
  ("float_compare", [Floating x, Floating y]) ->
    pure $ Int $ if x < y then 0 else if x == y then 1 else if x > y then 2 else 3
  ("float_test", [Int precision, Int test, Floating x]) -> pure $ bool $ case test of
    0 -> isNaN x
    1 -> isInfinite x
    2 -> if precision == 32 then isDenormalized (single x) else isDenormalized x
    _ -> isNegativeZero x
  ("float_mantissa", [Int precision, Floating x]) ->
    pure $ Integer $ fst $ if precision == 32 then decodeFloat (single x) else decodeFloat x
  ("float_exponent", [Int precision, Floating x]) ->
    pure $ Int $ fromIntegral $ snd $ if precision == 32 then decodeFloat (single x) else decodeFloat x
  ("float_encode", [Int precision, Integer mantissa, Int exponent]) ->
    pure $ precise precision (encodeFloat mantissa (fromIntegral exponent)) (encodeFloat mantissa (fromIntegral exponent))
  ("float_truncate", [Floating x]) -> pure $ Integer $ truncate x
  ("integer_bits", [Int operation, Integer left, Integer right]) ->
    pure $ Integer $ case operation of
      0 -> left .&. right
      1 -> left .|. right
      _ -> left `xor` right
  ("integer_shift", [Integer number, Int places]) -> pure $ Integer $ shift number (fromIntegral places)
  ("integer_pop_count", [Integer number]) -> pure $ Int $ fromIntegral $ popCount number
  ("integer_show", [Integer number]) -> pure $ Text $ Text.pack $ show number
  -- A program on the machine is given no arguments, until it sets some.
  ("arg_count", []) -> Int . fromIntegral . length <$> readIORef arguments
  ("arg", [Int index]) -> Text . (!! fromIntegral index) <$> readIORef arguments
  ("set_args", [Text list]) -> unit <$ writeIORef arguments (nulled list)
  ("prog_name", []) -> Text <$> readIORef programName
  ("set_prog_name", [Text name]) -> unit <$ writeIORef programName name
  ("set_env", [Text variable, Text value])
    | Text.null value -> unit <$ unsetEnv (unpack variable)
    | otherwise -> unit <$ setEnv (unpack variable) (unpack value)
  ("unset_env", [Text variable]) -> unit <$ unsetEnv (unpack variable)
  ("environment", []) -> do
    variables <- getEnvironment
    pure $ Text $ Text.pack $ concat [name ++ "\0" ++ value ++ "\0" | (name, value) <- variables]
  ("get_env", [Text variable]) -> do
    found <- lookupEnv (unpack variable)
    pure $ Text $ Text.pack $ case found of
      Just value -> '1' : value
      Nothing -> "0"
  ("executable_path", []) -> Text . Text.pack <$> getExecutablePath
  ("exit", [Int code])
    | code == 0 -> exitSuccess
    | otherwise -> exitWith (ExitFailure (fromIntegral code))
  -- As the runtime has it: a file that cannot be opened is the negative of
  -- the kind of failure, less one.
  ("open", [Text path, Int mode]) -> do
    opened <- tryIOError $ case mode of
      4 -> do
        there <- doesPathExist (unpack path)
        if there
          then ioError (ioeSetErrorString (mkIOError alreadyExistsErrorType "openFile" Nothing (Just (unpack path))) "File exists")
          else openFile (unpack path) ReadWriteMode
      _ -> openFile (unpack path) $ case mode of
        0 -> ReadMode
        1 -> WriteMode
        2 -> AppendMode
        _ -> ReadWriteMode
    case opened of
      Left problem -> do
        writeIORef failure (Text.pack (ioe_description problem))
        pure $ Int $ negate (failureNumber (ioe_type problem)) - 1
      Right handle -> do
        hSetEncoding handle utf8
        hSetNewlineMode handle noNewlineTranslation
        open <- readIORef files
        let descriptor = head [number | number <- [3 ..], Map.notMember number open]
        writeIORef files (Map.insert descriptor (path, handle) open)
        pure $ Int $ fromIntegral descriptor
  ("last_failure", []) -> Text <$> readIORef failure
  ("remove_file", [Text path]) -> do
    removed <- tryIOError (removeFile (unpack path))
    case removed of
      Left problem -> do
        writeIORef failure (Text.pack (ioe_description problem))
        pure $ Int $ negate (failureNumber (ioe_type problem)) - 1
      Right () -> pure $ Int 0
  ("file_name", [Int descriptor]) -> do
    open <- readIORef files
    case Map.lookup (fromIntegral descriptor) open of
      Just (path, _) -> pure $ Text path
      Nothing -> bug $ "file " ++ show descriptor ++ " is not open"
  ("close", [Int descriptor]) -> do
    open <- readIORef files
    case Map.lookup (fromIntegral descriptor) open of
      Just (_, handle) -> do
        hClose handle
        writeIORef files (Map.delete (fromIntegral descriptor) open)
      Nothing -> pure ()
    pure unit
  ("file_state", [Int descriptor]) -> case descriptor of
    0 -> pure $ Int 1
    1 -> pure $ Int 2
    2 -> pure $ Int 2
    _ -> do
      open <- readIORef files
      case Map.lookup (fromIntegral descriptor) open of
        Just (_, handle) -> do
          readable <- hIsReadable handle
          writable <- hIsWritable handle
          pure $ Int $ (if readable then 1 else 0) + (if writable then 2 else 0)
        Nothing -> pure $ Int 0
  ("write", [Int descriptor, Text text]) -> do
    handle <- known descriptor
    Text.IO.hPutStr handle text
    pure unit
  ("flush", [Int descriptor]) -> do
    handle <- known descriptor
    unit <$ hFlush handle
  ("read_all", [Int descriptor]) -> do
    handle <- known descriptor
    Text <$> Text.IO.hGetContents handle
  ("read_line", [Int descriptor]) -> do
    handle <- known descriptor
    end <- hIsEOF handle
    if end
      then pure $ Text $ Text.pack "0"
      else Text . Text.cons '1' <$> Text.IO.hGetLine handle
  ("read_char", [Int descriptor]) -> do
    handle <- known descriptor
    end <- hIsEOF handle
    if end then pure $ Int (-1) else Int . fromIntegral . ord <$> hGetChar handle
  ("peek_char", [Int descriptor]) -> do
    handle <- known descriptor
    end <- hIsEOF handle
    if end then pure $ Int (-1) else Int . fromIntegral . ord <$> hLookAhead handle
  ("is_eof", [Int descriptor]) -> do
    handle <- known descriptor
    bool <$> hIsEOF handle
  ("file_size", [Int descriptor]) -> do
    handle <- known descriptor
    Integer <$> hFileSize handle
  ("set_file_size", [Int descriptor, Integer size]) -> do
    handle <- known descriptor
    unit <$ hSetFileSize handle size
  ("seek", [Int descriptor, Int mode, Integer offset]) -> do
    handle <- known descriptor
    let from = case mode of
          0 -> AbsoluteSeek
          1 -> RelativeSeek
          _ -> SeekFromEnd
    unit <$ hSeek handle from offset
  ("tell", [Int descriptor]) -> do
    handle <- known descriptor
    Integer <$> hTell handle
  ("is_terminal", [Int descriptor]) -> do
    handle <- known descriptor
    terminal <- hIsTerminalDevice handle
    pure $ Int $ if terminal then 1 else 0
  ("list_directory", [Text path]) -> do
    names <- listDirectory (unpack path)
    pure $ Text $ Text.intercalate (Text.pack "\n") $ map Text.pack $ sort names
  ("create_directory", [Text path]) -> unit <$ createDirectoryIfMissing True (unpack path)
  ("path_kind", [Text path]) -> do
    file <- doesFileExist (unpack path)
    directory <- doesDirectoryExist (unpack path)
    pure $ Int $ if directory then 2 else if file then 1 else 0
  -- The machine keeps no time.
  ("clock", []) -> pure $ Int 0
  ("os", []) -> pure $ Text $ Text.pack os
  ("catch_push", [handler, resume]) -> do
    depth <- length <$> readIORef working
    unit <$ modifyIORef catching ((handler, resume, depth) :)
  ("catch_abandon", [again]) ->
    readIORef catching >>= \case
      (_, _, depth) : _ -> do
        thunks <- readIORef working
        let (abandoned, left) = splitAt (length thunks - depth) thunks
        for_ abandoned $ \thunk -> writeIORef thunk (Again again)
        unit <$ writeIORef working left
      [] -> pure unit
  ("catching", []) -> bool . not . null <$> readIORef catching
  ("uncaught", [Int code, Text shown])
    | code == 0 -> exitSuccess
    | code > 0 -> exitWith (ExitFailure (fromIntegral code))
    -- What `error` throws is said as it is.
    | code == -2 -> do
        hPutStrLn stderr (unpack shown)
        exitFailure
    | otherwise -> do
        hPutStrLn stderr $ "uncaught exception: " ++ unpack shown
        exitFailure
  ("catch_pop", []) ->
    readIORef catching >>= \case
      (handler, resume, _) : rest -> do
        writeIORef catching rest
        pure $ Data 0 [handler, resume]
      [] -> do
        hPutStrLn stderr "a value was thrown and nothing catches it"
        exitFailure
  ("pack_new", []) -> do
    texts <- readIORef building
    let buffer = head [number | number <- [0 ..], Map.notMember number texts]
    writeIORef building (Map.insert buffer [] texts)
    pure $ Int $ fromIntegral buffer
  ("pack_push", [Int buffer, Int character]) ->
    unit <$ modifyIORef building (Map.adjust (Text.singleton (chr (fromIntegral character)) :) (fromIntegral buffer))
  ("pack_append", [Int buffer, Text text]) ->
    unit <$ modifyIORef building (Map.adjust (text :) (fromIntegral buffer))
  ("pack_done", [Int buffer]) -> do
    texts <- readIORef building
    writeIORef building (Map.delete (fromIntegral buffer) texts)
    pure $ Text $ Text.concat $ reverse $ Map.findWithDefault [] (fromIntegral buffer) texts
  _ -> bug $ "the machine cannot " ++ name
  where
    known descriptor = case descriptor of
      0 -> pure stdin
      1 -> pure stdout
      2 -> pure stderr
      _ -> do
        open <- readIORef files
        case Map.lookup (fromIntegral descriptor) open of
          Just (_, handle) -> pure handle
          Nothing -> bug $ "file " ++ show descriptor ++ " is not open"
    nulled = filter (not . Text.null) . Text.splitOn (Text.pack "\0")
    -- As the runtime numbers the kinds of failure.
    failureNumber = \case
      AlreadyExists -> 1
      NoSuchThing -> 2
      ResourceBusy -> 3
      ResourceExhausted -> 4
      PermissionDenied -> 7
      InappropriateType -> 8
      InvalidArgument -> 9
      _ -> 0

-- |
-- A string as the bytes of UTF-8, and those bytes as the string, a byte
-- that is not part of a character being the character it is the code of.
encodeUtf8' :: String -> [Word8]
encodeUtf8' = concatMap character
  where
    character c
      | code < 0x80 = [fromIntegral code]
      | code < 0x800 = [0xc0 + high 6, continuing 0]
      | code < 0x10000 = [0xe0 + high 12, continuing 6, continuing 0]
      | otherwise = [0xf0 + high 18, continuing 12, continuing 6, continuing 0]
      where
        code = ord c
        high shift' = fromIntegral (code `div` 2 ^ shift')
        continuing shift' = 0x80 + fromIntegral (code `div` 2 ^ shift' `mod` 64)

decodeUtf8' :: [Word8] -> String
decodeUtf8' = \case
  [] -> []
  byte : rest
    | byte < 0x80 -> chr (fromIntegral byte) : decodeUtf8' rest
    | byte >= 0xc0 && byte < 0xe0 -> continued 1 (byte - 0xc0) rest
    | byte >= 0xe0 && byte < 0xf0 -> continued 2 (byte - 0xe0) rest
    | byte >= 0xf0 && byte < 0xf8 -> continued 3 (byte - 0xf0) rest
    | otherwise -> chr (fromIntegral byte) : decodeUtf8' rest
  where
    continued count first rest = case splitAt count rest of
      (following, after)
        | length following == count && all (\byte -> byte >= 0x80 && byte < 0xc0) following ->
            chr (foldl (\code byte -> code * 64 + fromIntegral byte - 0x80) (fromIntegral first) following) : decodeUtf8' after
      _ -> chr (fromIntegral first) : decodeUtf8' rest
