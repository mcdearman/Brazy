-- |
-- An abstract machine for AxCut.
--
-- This is the reference for what an AxCut program means, and it is how the
-- lowering is tested before any code generator exists: a program run here
-- must print what the Javascript backend's output prints. It is not a
-- runtime. Nothing here is tuned, and values are left to Haskell's garbage
-- collector.
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
import Data.Char (chr, generalCategory, ord)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (sort)
import Data.Map (Map)
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
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import System.IO
  ( Handle,
    IOMode (AppendMode, ReadMode, WriteMode),
    hClose,
    hIsTerminalDevice,
    hPutStrLn,
    hSetEncoding,
    openFile,
    stderr,
    stdin,
    stdout,
    utf8,
  )
import System.Info (os)

data Value
  = Int !Int32
  | Integer !Integer
  | Text !Text
  | Data !Int [Value]
  | Reference !(IORef Cell)
  | -- | A place that holds a value, which can be changed for another.
    Box !(IORef Value)

-- |
-- What a program has of the world outside while it runs: what catches a
-- thrown value, the catch begun last first, and the files that are open.
data World = World
  { catching :: !(IORef [(Value, Value)]),
    files :: !(IORef (Map Int Handle))
  }

-- |
-- What an object is at the moment.
data Cell
  = -- | A function or a consumer: its method, the method's parameters and
    -- body, and what was captured.
    Closure !Method [Text] Statement [(Text, Value)]
  | -- | A thunk that has not been forced.
    Delayed [Text] Statement [(Text, Value)]
  | -- | A thunk that is being forced.
    Working
  | -- | A thunk that has been forced.
    Done Value
  | -- | The consumer a thunk's body is given: it keeps the value in the
    -- thunk and then hands it to the consumer that asked.
    Update !(IORef Cell) Value

-- |
-- The row of values. The last value is last. What a closure or a thunk
-- captured is one, written out there: Hazy cannot check a name for a type
-- that is defined by way of the types the name is used in.
type Environment = [(Text, Value)]

type Globals = Map Global (IORef Cell)

run :: Program -> IO ()
run Program {definitions, entry} = do
  globals <- traverse (\object -> allocate object []) definitions
  catching <- newIORef []
  files <- newIORef Map.empty
  execute World {catching, files} globals entry []

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
      Global name global rest -> case Map.lookup global globals of
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
-- What the runtime does under a name of its own, as the machine does it.
outside :: World -> String -> [Value] -> IO Value
outside World {catching, files} name arguments = case (name, arguments) of
  ("new_ref", [value]) -> Box <$> newIORef value
  ("read_ref", [Box box]) -> readIORef box
  ("write_ref", [Box box, value]) -> unit <$ writeIORef box value
  ("same_ref", [Box left, Box right]) -> pure $ bool $ left == right
  -- A program on the machine is given no arguments.
  ("arg_count", []) -> pure $ Int 0
  ("get_env", [Text variable]) -> do
    found <- lookupEnv (unpack variable)
    pure $ Text $ Text.pack $ case found of
      Just value -> '1' : value
      Nothing -> "0"
  ("executable_path", []) -> Text . Text.pack <$> getExecutablePath
  ("exit", [Int code])
    | code == 0 -> exitSuccess
    | otherwise -> exitWith (ExitFailure (fromIntegral code))
  ("open", [Text path, Int mode]) -> do
    handle <- openFile (unpack path) $ case mode of
      0 -> ReadMode
      1 -> WriteMode
      _ -> AppendMode
    hSetEncoding handle utf8
    open <- readIORef files
    let descriptor = head [number | number <- [3 ..], Map.notMember number open]
    writeIORef files (Map.insert descriptor handle open)
    pure $ Int $ fromIntegral descriptor
  ("close", [Int descriptor]) -> do
    open <- readIORef files
    case Map.lookup (fromIntegral descriptor) open of
      Just handle -> do
        hClose handle
        writeIORef files (Map.delete (fromIntegral descriptor) open)
      Nothing -> pure ()
    pure unit
  ("write", [Int descriptor, Text text]) -> do
    handle <- known descriptor
    Text.IO.hPutStr handle text
    pure unit
  ("read_all", [Int descriptor]) -> do
    handle <- known descriptor
    Text <$> Text.IO.hGetContents handle
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
  ("catch_pop", []) ->
    readIORef catching >>= \case
      (handler, resume) : rest -> do
        writeIORef catching rest
        pure $ Data 0 [handler, resume]
      [] -> do
        hPutStrLn stderr "a value was thrown and nothing catches it"
        exitFailure
  _ -> bug $ "the machine cannot " ++ name
  where
    known descriptor = case descriptor of
      0 -> pure stdin
      1 -> pure stdout
      2 -> pure stderr
      _ -> do
        open <- readIORef files
        case Map.lookup (fromIntegral descriptor) open of
          Just handle -> pure handle
          Nothing -> bug $ "file " ++ show descriptor ++ " is not open"
