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
import Data.Char (chr, generalCategory, ord)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
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
    hGetChar,
    hIsEOF,
    hIsReadable,
    hIsTerminalDevice,
    hIsWritable,
    hLookAhead,
    hPutStrLn,
    hSeek,
    hSetEncoding,
    hSetNewlineMode,
    hSetFileSize,
    hTell,
    noNewlineTranslation,
    openFile,
    stderr,
    stdin,
    stdout,
    utf8,
  )
import System.IO.Error (alreadyExistsErrorType, ioeSetErrorString, mkIOError, tryIOError)
import System.Info (os)

data Value
  = Int !Int32
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
  { catching :: !(IORef [(Value, Value)]),
    files :: !(IORef (Map Int (Text, Handle))),
    failure :: !(IORef Text),
    -- | The arguments and the name of the program, which it can change for
    -- a while.
    arguments :: !(IORef [Text]),
    programName :: !(IORef Text),
    -- | The texts that are being built, each as its pieces, the last first.
    building :: !(IORef (Map Int [Text]))
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
  files <- newIORef Map.empty
  failure <- newIORef Text.empty
  arguments <- newIORef []
  programName <- newIORef (Text.pack "<interactive>")
  building <- newIORef Map.empty
  execute World {catching, files, failure, arguments, programName, building} globals entry []

allocate :: Object -> Environment -> IO (IORef Cell)
allocate Object {kind, method, parameters, body} environment = newIORef $ case kind of
  Kind.Closure -> Closure method parameters body environment
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
          _ -> bug $ "bad invoke " ++ show method
      Force -> case environment of
        [(_, consumer), (_, value)] -> force value consumer
        _ -> bug "force with the wrong environment"
      Literal name literal rest ->
        let value = case literal of
              Literal.Int int -> Int int
              Literal.Integer integer -> Integer integer
              Literal.Text text -> Text text
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
            update <- newIORef (Update cell consumer)
            go body (zip parameters [Reference update] ++ environment)
          Done value -> deliver value consumer
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
            deliver value consumer
          _ -> bug "a value was given to something that is not a consumer"
      _ -> bug "a value was given to something that is not an object"

apply :: World -> Primitive -> [Value] -> IO Value
apply world primitive arguments = case (primitive, arguments) of
  (Foreign name _, _) -> outside world (unpack name) arguments
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
  (IntAdd, [Int left, Int right]) -> pure $ Int $ left + right
  (IntMinus, [Int left, Int right]) -> pure $ Int $ left - right
  (IntMultiply, [Int left, Int right]) -> pure $ Int $ left * right
  (IntNegate, [Int value]) -> pure $ Int $ negate value
  (IntAbs, [Int value]) -> pure $ Int $ abs value
  (IntSignum, [Int value]) -> pure $ Int $ signum value
  (IntQuot, [Int left, Int right])
    | right == 0 -> divideByZero
    -- The one quotient that does not fit wraps around.
    | right == -1 -> pure $ Int $ negate left
    | otherwise -> pure $ Int $ left `quot` right
  (IntRem, [Int left, Int right])
    | right == 0 -> divideByZero
    | right == -1 -> pure $ Int 0
    | otherwise -> pure $ Int $ left `rem` right
  (IntToInteger, [Int value]) -> pure $ Integer $ fromIntegral value
  (IntegerCastToInt, [Integer value])
    | value > fromIntegral (maxBound :: Int32) || value < fromIntegral (minBound :: Int32) -> do
        hPutStrLn stderr "Enum Integer Overflow"
        exitFailure
    | otherwise -> pure $ Int $ fromInteger value
  (IntegerTruncateToInt, [Integer value]) -> pure $ Int $ fromInteger value
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

-- |
-- A floating-point number of a precision: 32 for a Float, worked out as a
-- Float, and 64 for a Double.
precise :: Int32 -> Double -> Float -> Value
precise precision double float
  | precision == 32 = Floating (realToFrac float)
  | otherwise = Floating double

single :: Double -> Float
single = realToFrac

-- | The functions of floating point, by number, as the runtime has them.
function :: (Floating a) => Int32 -> a -> a
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

operator :: (Floating a) => Int32 -> a -> a -> a
operator = \case
  0 -> (+)
  1 -> (-)
  2 -> (*)
  3 -> (/)
  4 -> (**)
  operation -> error $ "no such operator of floating point: " ++ show operation

-- |
-- What a place of an array holds.
placed :: IORef (Vector Value) -> Int32 -> IO Value
placed array index = do
  places <- readIORef array
  case places Vector.!? fromIntegral index of
    Just value -> pure value
    Nothing -> bug $ "index " ++ show index ++ " is out of the bounds of an array of " ++ show (Vector.length places)

-- |
-- What the runtime does under a name of its own, as the machine does it.
outside :: World -> String -> [Value] -> IO Value
outside World {catching, files, failure, building, arguments, programName} name given = case (name, given) of
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
  ("catch_push", [handler, resume]) -> unit <$ modifyIORef catching ((handler, resume) :)
  ("catching", []) -> bool . not . null <$> readIORef catching
  ("uncaught", [Int code, Text shown])
    | code == 0 -> exitSuccess
    | code > 0 -> exitWith (ExitFailure (fromIntegral code))
    | otherwise -> do
        hPutStrLn stderr $ "uncaught exception: " ++ unpack shown
        exitFailure
  ("catch_pop", []) ->
    readIORef catching >>= \case
      (handler, resume) : rest -> do
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
