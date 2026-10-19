module Main (main) where

import qualified AxCut.Lower as AxCut (lower)
import qualified AxCut.Machine as AxCut (run)
import qualified AxCut.Printer as AxCut (print)
import Control.Exception (catch)
import Control.Monad (zipWithM_)
import qualified Core.Eta as Eta (expand)
import qualified Core.Tree.Module as Core (Module, name)
import qualified Core.Tree.Module as Module (simplify)
import Data.Char (toLower, toUpper)
import Data.Foldable (for_, toList, traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Traversable (for)
import Data.Functor.Identity (Identity (..))
import Data.List (sort)
import Data.List.Reverse (List (..))
import qualified Data.Map as Map
import Data.Text (Text, intercalate, pack, unpack)
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Error (HazyError)
import qualified Error (allow, fail, types)
import qualified Generate.Mangle as Mangle (pathListed, pathSys)
import qualified AxCut.Lower as AxCut (lowerUnit)
import qualified LLVM.FromAxCut as LLVM
  ( Unit (..),
    arm64,
    checking,
    generate,
    generateUnit,
    lending,
    windows,
    withoutReuse,
  )
import Package as X (Module (Module), Package (Package))
import qualified Package
import Repl (Compiler (..), repl)
import qualified Repl (source)
import qualified Semantic.Check.Go.Module as Module (check)
import qualified Semantic.Layout as Layout
import qualified Semantic.Resolve.Go.Module as Module (resolve)
import qualified Semantic.Scope as Scope
import qualified Semantic.Stage as Stage
import qualified Semantic.Tree.Module as Module (connect, seperate)
import qualified Semantic.Tree.Module as Semantic (Module, name)
import Syntax.Literate (unlit)
import qualified Sequent.FromCore as Sequent (Unit (..), Unshared (..), complete, reached, start, unit)
import qualified Sequent.Printer as Sequent (print)
import qualified Sequent.Tree as Sequent (Program (..))
import qualified AxCut.Tree as AxCut (Program (..))
import qualified Sequent.Runtime as Runtime (main, showGlobal)
import Syntax.Extensions (Extensions, hazy)
import Syntax.Lexer
  ( FullQualifiers (..),
    Qualifiers (..),
    constructorIdentifier,
    extend,
  )
import qualified Syntax.Lexer as Lexer
import qualified Syntax.Parser as Parser (parse)
import Syntax.ParserCombinator (internal, parse, startStream)
import Syntax.Position (Position)
import qualified Syntax.Tree.Module as Module (parse)
import qualified Syntax.Tree.Module as Syntax (Module, assumeName, name)
import qualified Syntax.Variable as Variable
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), getOpt, usageInfo)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath
  ( dropExtension,
    dropFileName,
    dropTrailingPathSeparator,
    hasDrive,
    pathSeparators,
    takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
  )
import System.IO
  ( IOMode (..),
    hClose,
    hFlush,
    hSetEncoding,
    hSetNewlineMode,
    noNewlineTranslation,
    openFile,
    stderr,
    stdout,
    utf8,
  )
import System.Info (os)
import System.Process (callProcess)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (withArray0)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Status (Status)
import qualified Status (finished, progress, start, status, warning)
import Verbose (runVerbose)
import Prelude hiding (Show (show))
import qualified Prelude

data Loaded
  = Root
      { extensions :: !Extensions,
        name :: !Text,
        contents :: Text
      }
  | Inner
      { extensions :: !Extensions,
        path :: !FullQualifiers,
        name :: !Text,
        contents :: Text
      }

loadModule :: FilePath -> IO Text
loadModule file = unsafeInterleaveIO $ do
  handle <- openFile file ReadMode
  hSetEncoding handle utf8
  Text.IO.hGetContents handle

loadModules :: Extensions -> FilePath -> IO [Loaded]
loadModules extensions = loadModules []
  where
    loadModules :: [FilePath] -> FilePath -> IO [Loaded]
    loadModules modulePath file = case takeExtension file of
      "" -> do
        -- In order, so that what is made of a program does not depend on
        -- the order a system happens to keep its files in.
        children <- sort <$> listDirectory file
        let normalize (h : t) = dropExtension (toUpper h : t)
            normalize [] = error "empty path"
            recurse name = loadModules (normalize name : modulePath) (file </> name)
        inner <- traverse recurse children
        pure (concat inner)
      ".hs" -> source id modulePath file
      -- Literate source, of which only the program is read.
      ".lhs" -> source unlit modulePath file
      ".hs-boot" -> pure []
      extension -> do
        putStrLn $ "Unknown file extension (" ++ extension ++ "): " ++ file
        exitFailure
    source :: (Text -> Text) -> [FilePath] -> FilePath -> IO [Loaded]
    source program modulePath file =
      let qualifiers = foldl (:.) Local (constructorIdentifier . pack <$> reverse modulePath)
       in case qualifiers of
            (modulePath :. moduleName)
              | path <- modulePath :.. moduleName -> do
                  contents <- program <$> loadModule file
                  pure [Inner {extensions, path, name = pack file, contents}]
            _ -> do
              contents <- program <$> loadModule file
              pure [Root {extensions, name = pack file, contents}]

loadPackage :: Package -> [Loaded]
loadPackage Package {extensions, modules} = map loadModule modules
  where
    loadModule Module {path, name, header} =
      Inner
        { extensions = foldl extend hazy extensions,
          path,
          name,
          contents = header
        }

stage1 :: Debug -> Vector Loaded -> IO (Vector (Syntax.Module Position))
stage1 verbose = case verbose of
  Debug -> runVerbose . traverse parse
  Normal -> pure . runIdentity . traverse parse
  where
    parse Root {extensions, name, contents} = Parser.parse extensions Module.parse name contents
    parse Inner {extensions, path, name, contents} =
      Syntax.assumeName path
        <$> Parser.parse extensions Module.parse name contents

stage2 ::
  Debug ->
  Vector (Syntax.Module Position) ->
  IO (Vector (Semantic.Module Identity Layout.Normal Stage.Resolve Scope.Global))
stage2 verbose = case verbose of
  Debug -> runVerbose . Module.resolve
  Normal -> pure . runIdentity . Module.resolve

stage2x ::
  Debug ->
  Vector (Semantic.Module Identity Layout.Normal Stage.Resolve Scope.Global) ->
  IO (Vector (Semantic.Module Identity Layout.Group Stage.Resolve Scope.Global))
stage2x _ = pure . Module.connect

stage3 ::
  Debug ->
  Vector (Semantic.Module Identity Layout.Normal Stage.Resolve Scope.Global) ->
  IO (Vector (Semantic.Module Identity Layout.Normal Stage.Check Scope.Global))
stage3 _ = pure . Module.seperate . Module.check . Module.connect

stage4 ::
  Debug ->
  Vector (Semantic.Module Identity Layout.Normal Stage.Check Scope.Global) ->
  IO (Vector Core.Module)
stage4 _ = pure . Eta.expand . Vector.map Module.simplify

-- |
-- What a directory is called: its own name, without what it is in.
packageName :: FilePath -> String
packageName = takeFileName . dropTrailingPathSeparator

-- |
-- The name of a module, as it is said to someone.
moduleName :: FullQualifiers -> String
moduleName = Lazy.Text.unpack . Builder.toLazyText . Variable.print'

-- |
-- Take some modules through a stage, one at a time, saying how far along it
-- is. A module's errors are found when it is its turn.
forceModules :: (Foldable t, Prelude.Show a) => String -> Show -> Status -> (a -> FullQualifiers) -> t a -> IO ()
forceModules stage show progress name modules = do
  traverse_ go $ zip [1 ..] (toList modules)
  case show of
    NoShow -> Status.finished progress $ map toLower stage ++ " " ++ counted total
    Show -> pure ()
  where
    total = length modules
    counted 1 = "1 module"
    counted count = Prelude.show count ++ " modules"
    go (index, modulex) = do
      Status.progress progress stage index total (moduleName (name modulex))
      case show of
        NoShow -> do
          -- todo, implement deepseq
          -- this is a horrible hack
          () <- pure $ seq (length (Prelude.show modulex)) ()
          pure ()
        Show -> do
          print modulex

data Mode
  = Help
  | Parse
  | Resolve
  | Group
  | Check
  | Simplify
  | Sequent
  | AxCut
  | Run
  | LLVM
  | Native FilePath
  | Object FilePath
  | Repl
  | Generate FilePath

data Verbose
  = Quiet
  | Loud

data Debug
  = Normal
  | Debug

data Show
  = Show
  | NoShow

data Failure
  = Attempt
  | Expect String

data Format
  = Full
  | Pack

data Bare
  = Standard
  | Runtime
  | Bare

-- |
-- How a native program meets its packages.
data Linking
  = -- | Each package was compiled by itself and is linked in.
    Separate
  | -- | The packages are compiled again, as part of the program.
    Whole

data Execute = Execute
  { linking :: !Linking,
    reusing :: !Bool,
    lending :: !Bool,
    checking :: !Bool,
    include :: List String,
    modules :: List String,
    packages :: List String,
    language :: List String,
    mode :: !Mode,
    verbose :: !Verbose,
    debug :: !Debug,
    failure :: !Failure,
    show :: !Show,
    format :: !Format,
    bare :: !Bare
  }

defaultx :: Execute
defaultx =
  Execute
    { linking = Separate,
      reusing = True,
      lending = True,
      checking = False,
      include = Nil,
      modules = Nil,
      packages = Nil,
      language = Nil,
      mode = Check,
      verbose = Loud,
      debug = Normal,
      failure = Attempt,
      show = NoShow,
      format = Full,
      bare = Standard
    }

order :: ArgOrder (Execute -> Execute)
order = ReturnInOrder wrap
  where
    wrap path execute@Execute {modules} = execute {modules = modules :> path}

options :: [OptDescr (Execute -> Execute)]
options =
  [ Option [] ["help"] (NoArg help) "Print Help",
    Option [] ["parse"] (NoArg parse) "Parse source",
    Option [] ["resolve"] (NoArg resolve) "Resolve source",
    Option [] ["check"] (NoArg check) "Typecheck source",
    Option ['o'] [] (ReqArg generate "PATH") "Compile the program to a native executable, or with --pack write the package",
    Option ['I'] [] (ReqArg include "PATH") "Add directory to search path",
    Option [] ["package"] (ReqArg package "NAME or PATH ('./...' or '/...')") "Load a package.",
    Option [] ["pack"] (NoArg pack) "Emit package",
    Option ['X'] [] (ReqArg extension "EXTENSION") "Enable extension",
    Option ['q'] [""] (NoArg quiet) "Don't show messages when compiling",
    Option [] ["bare"] (NoArg bare) "Don't include runtime and base package",
    Option [] ["bare-runtime"] (NoArg bareRuntime) "Don't include base package",
    Option [] ["debug-group"] (NoArg group) "Group source",
    Option [] ["debug-simplify"] (NoArg simplify) "Simplify source",
    Option [] ["debug-sequent"] (NoArg sequent) "Show the program in the sequent calculus",
    Option [] ["debug-axcut"] (NoArg axcut) "Show the program in AxCut",
    Option [] ["run-axcut"] (NoArg runAxCut) "Run the program on the AxCut machine",
    Option [] ["debug-llvm"] (NoArg llvm) "Show the program in LLVM IR",
    Option [] ["native"] (ReqArg native "PATH") "Compile the program to a native executable",
    Option [] ["native-object"] (ReqArg object "PATH") "Compile the modules to a native object, for a package",
    Option [] ["whole"] (NoArg whole') "Compile packages again with the program, instead of linking them",
    Option [] ["no-reuse"] (NoArg noReuse) "Never build a value in the place of one just taken apart",
    Option [] ["no-lend"] (NoArg noLend) "Never lend a function what it only reads",
    Option [] ["checked"] (NoArg checkedx) "Make a program whose counts of references can be checked as it runs",
    Option [] ["repl"] (NoArg interactive) "Start an interactive session",
    Option [] ["debug-message"] (NoArg debug) "Show debug messages",
    Option [] ["debug-show"] (NoArg show) "Show internal AST",
    Option [] ["debug-fail"] (ReqArg fail "ERROR") "Expect failure"
  ]
  where
    help execute = execute {mode = Help}
    parse execute = execute {mode = Parse}
    resolve execute = execute {mode = Resolve}
    group execute = execute {mode = Group}
    check execute = execute {mode = Check}
    simplify execute = execute {mode = Simplify}
    sequent execute = execute {mode = Sequent}
    axcut execute = execute {mode = AxCut}
    runAxCut execute = execute {mode = Run}
    interactive execute = execute {mode = Repl}
    llvm execute = execute {mode = LLVM}
    native path execute = execute {mode = Native path}
    object path execute = execute {mode = Object path}
    whole' execute = execute {linking = Whole}
    noReuse execute = execute {reusing = False}
    noLend execute = execute {lending = False}
    checkedx execute = execute {checking = True}
    generate path execute = execute {mode = Generate path}
    debug execute = execute {debug = Debug}
    fail error execute = execute {failure = Expect error}
    include path execute@Execute {include} = execute {include = include :> path}
    package path execute@Execute {packages} = execute {packages = packages :> path}
    show execute = execute {show = Show}
    quiet execute = execute {verbose = Quiet}
    pack execute = execute {format = Pack}
    bare execute = execute {bare = Bare}
    bareRuntime execute = execute {bare = Runtime}
    extension extension execute@Execute {language} = execute {language = language :> extension}

main :: IO ()
main = getArgs >>= main''

main' :: String -> IO ()
main' args = main'' (words args)

main'' :: [String] -> IO ()
main'' args = case getOpt order options args of
  (_, _, errors@(_ : _)) -> do
    traverse_ putStrLn errors
    exitFailure
  (_, _ : _, _) -> error "flags not processed"
  (flags, [], []) -> do
    -- What is printed ends its lines the same way on every system, so that
    -- it can be compared with what is expected.
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation
    let Execute
          { linking,
            reusing,
            lending,
            checking,
            include,
            modules,
            packages,
            mode,
            verbose,
            debug,
            failure,
            show,
            format,
            bare,
            language
          } =
            foldl (flip id) defaultx flags
    progress <- Status.start $ case verbose of
      Loud -> True
      Quiet -> False
    language <- pure $ pack <$> toList language
    let extensions = foldl extend hazy toggles
          where
            toggles = parse Lexer.toggle . startStream internal <$> language
    let builtin = case bare of
          Bare -> []
          Runtime -> ["hazy-internal"]
          Standard -> ["hazy-internal", "base"]

    exe <- getExecutablePath
    let root = takeDirectory (takeDirectory exe)
        package = root </> "lib" </> "hazy" </> "packages"
        pathize name = case name of
          '.' : c : _ | c `elem` pathSeparators -> name
          _ | hasDrive name -> name
          _ -> package </> name
    packages <- traverse (Package.load . pathize) (builtin ++ toList packages)
    let given = modules
    let system = Vector.fromList $ concatMap loadPackage packages
    include <- Vector.fromList . concat <$> traverse (loadModules extensions) (toList include)
    modules <- Vector.fromList . concat <$> traverse (loadModules extensions) (toList modules)
    let headers = system <> include
        split = length headers
        all = headers <> modules
        help = putStr $ usageInfo "hazy [options] folders..." options
        -- What is made of a program is an executable, however it is asked
        -- for.
        asked = case (mode, format) of
          (Generate target, Full) -> Native target
          _ -> mode
        run = case asked of
          Repl -> do
            parsed <- newIORef Map.empty
            verified <- newIORef Nothing
            repl (session parsed verified) (toList given)
          _ | null modules -> help
          Help -> help
          Parse -> do
            all <- stage1 debug all
            forceModules "Parsing" show progress Syntax.name (Vector.drop split all)
          Resolve -> do
            all <- stage1 debug all
            all <- stage2 debug all
            forceModules "Resolving" show progress Semantic.name (Vector.drop split all)
          Group -> do
            all <- stage1 debug all
            all <- stage2 debug all
            all <- stage2x debug all
            forceModules "Grouping" show progress Semantic.name (Vector.drop split all)
          Check -> do
            all <- stage1 debug all
            all <- stage2 debug all
            all <- stage3 debug all
            forceModules "Checking" show progress Semantic.name (Vector.drop split all)
          Simplify -> do
            all <- stage1 debug all
            all <- stage2 debug all
            all <- stage3 debug all
            all <- stage4 debug all
            forceModules "Simplifying" show progress Core.name (Vector.drop split all)
          Sequent -> do
            (program, _) <- whole all
            hSetEncoding stdout utf8
            Text.Lazy.IO.putStr $ Builder.toLazyText $ Sequent.print program
          AxCut -> do
            (program, _) <- whole all
            hSetEncoding stdout utf8
            Text.Lazy.IO.putStr $ Builder.toLazyText $ AxCut.print $ AxCut.lower program
          Run -> do
            (program, _) <- whole all
            hSetEncoding stdout utf8
            AxCut.run $ AxCut.lower program
          LLVM -> do
            (program, lends) <- whole all
            hSetEncoding stdout utf8
            Text.Lazy.IO.putStr $ Builder.toLazyText $ LLVM.generate (machine lends) $ AxCut.lower program
          Native target -> do
            createDirectoryIfMissing True (dropFileName target)
            let runtime = root </> "lib" </> "hazy" </> "native" </> library
            Status.status progress "Compiling" (packageName target ++ " to native code")
            case linking of
              Whole -> do
                (program, lends) <- whole all
                emit (target ++ ".ll") $ LLVM.generate (machine lends) $ AxCut.lower program
                finishWith (Status.finished progress (packageName target)) "clang" $
                  ["-O1", "-Wno-override-module"]
                    ++ sections
                    ++ [target ++ ".ll", runtime]
                    ++ strip
                    ++ ["-o", executable target]
              Separate -> do
                Sequent.Unit {own, shared, lends, arities, unshared} <-
                  Sequent.reached (Sequent.start Runtime.main) <$> unit all
                warn unshared
                emit (target ++ ".ll") $
                  LLVM.generateUnit
                    (machine lends)
                    LLVM.Unit
                      { defined = AxCut.lowerUnit arities own,
                        shared = AxCut.lowerUnit arities shared,
                        start = Just (AxCut.entry (AxCut.lower (startProgram)))
                      }
                let objects = [directory </> "native.o" | Package {root = directory} <- packages]
                Status.status progress "Linking" target
                finishWith (Status.finished progress (packageName target)) "clang" $
                  ["-O1", "-Wno-override-module"]
                    ++ sections
                    ++ [target ++ ".ll"]
                    ++ objects
                    ++ [runtime]
                    ++ strip
                    ++ ["-o", executable target]
          Object target -> do
            createDirectoryIfMissing True (dropFileName target)
            let label = packageName (takeDirectory target) ++ " to native code"
            Status.status progress "Compiling" label
            Sequent.Unit {own, shared, lends, arities, unshared} <- unit all
            warn unshared
            let source = dropExtension target ++ ".ll"
            emit source $
              LLVM.generateUnit
                (machine lends)
                LLVM.Unit
                  { defined = AxCut.lowerUnit arities own,
                    shared = AxCut.lowerUnit arities shared,
                    start = Nothing
                  }
            finishWith (Status.finished progress label) "clang" $
              ["-O1", "-Wno-override-module"] ++ sections ++ ["-c", source, "-o", target]
          -- A package is the source of its modules, each where its name says,
          -- and what says which they are and how they are read. It is
          -- checked first: what is wrong with it is said here, and not to
          -- whoever uses it.
          Generate target -> do
            all <- stage1 debug all
            all <- stage2 debug all
            all <- stage3 debug all
            let code = Vector.drop split all
            forceModules "Checking" show progress Semantic.name code
            let copy checked loaded = do
                  let file = target </> "header" </> Mangle.pathSys (Semantic.name checked) ++ ".hs"
                  createDirectoryIfMissing True (dropFileName file)
                  handle <- openFile file WriteMode
                  hSetEncoding handle utf8
                  Text.IO.hPutStr handle (contents loaded)
                  hClose handle
            zipWithM_ copy (toList code) (toList modules)
            let format checked = pack "\n" <> Mangle.pathListed (Semantic.name checked) <> pack ".hs"
                files = foldMap format code
                extensions = intercalate (pack ",") language
                meta = extensions <> pack ";" <> files
            createDirectoryIfMissing True target
            handle <- openFile (target </> "package") WriteMode
            hSetEncoding handle utf8
            Text.IO.hPutStr handle meta
            hClose handle
        -- The native pipeline takes the whole program, packages included.
        whole all = do
          all <- stage1 debug all
          all <- stage2 debug all
          all <- stage3 debug all
          all <- stage4 debug all
          pure $ Sequent.complete Runtime.main all
        -- The modules that are not a package's, as a unit to be linked with
        -- the packages.
        unit all = do
          all <- stage1 debug all
          all <- stage2 debug all
          all <- stage3 debug all
          all <- stage4 debug all
          pure $ Sequent.unit [length system .. length all - 1] all
        -- What native code is generated for.
        -- It is given which parameters each definition only reads.
        machine lends = examined (lent (reused base))
          where
            examined
              | checking = LLVM.checking
              | otherwise = id
            base
              | os == "mingw32" = LLVM.windows
              | otherwise = LLVM.arm64
            reused
              | reusing = id
              | otherwise = LLVM.withoutReuse
            lent
              | lending = LLVM.lending lends
              | otherwise = id
        -- What has the linker leave out the code that nothing reaches. Apple's
        -- linker can do it to any object. The others do it a section at a
        -- time, so there every function is given a section of its own.
        -- They also keep the mathematics the runtime uses apart from the
        -- rest of C's library, and have to be asked for it.
        -- Microsoft's linker is asked in its own words, and for the parts of
        -- Windows that the runtime's library uses.
        strip
          | os == "darwin" = ["-Wl,-dead_strip"]
          | os == "mingw32" =
              ["-Wl,/OPT:REF"] ++ map ("-l" ++) ["kernel32", "ntdll", "userenv", "ws2_32", "dbghelp"]
          | otherwise = ["-Wl,--gc-sections", "-lm"]
        sections
          | os == "darwin" = []
          | otherwise = ["-ffunction-sections", "-fdata-sections"]
        -- What a program's file is called, and the runtime's library.
        executable target
          | os == "mingw32" = target ++ ".exe"
          | otherwise = target
        library
          | os == "mingw32" = "hazy_runtime.lib"
          | otherwise = "libhazy_runtime.a"
        -- Tell of each value that is no longer shared with itself.
        warn unshared = for_ unshared $ \(Sequent.Unshared global local) ->
          Status.warning progress $
            concat
              [ "in ",
                dropWhile (== '@') (Runtime.showGlobal global),
                ", `",
                takeWhile (/= '%') (unpack local),
                "` refers to itself and uses variables of where it is defined. ",
                "So that nothing holds itself, it is worked out again each time it refers to itself. ",
                "Defined at the top of the module it would be worked out once."
              ]
        -- Run clang as the last thing the compiler does. Where a process
        -- can become another, it does, and what the compiler held is given
        -- back before clang starts: every module, which clang would otherwise
        -- have to make room beside. Windows has no such thing, and there
        -- clang is called and waited for, and then the compiler is done.
        finishWith done program arguments
          | os == "mingw32" = callProcess program arguments >> done
          | otherwise = do
              hFlush stdout
              hFlush stderr
              withCString program $ \name ->
                withMany withCString (program : arguments) $ \pointers ->
                  withArray0 nullPtr pointers $ \array -> do
                    _ <- execvp name array
                    ioError (userError ("could not run " ++ program))
        -- Where a program starts, lowered by itself.
        startProgram =
          Sequent.Program {definitions = Map.empty, entry = Sequent.start Runtime.main}
        -- Write some generated text to a file, to be compiled and to be read.
        emit file text = do
          handle <- openFile file WriteMode
          hSetEncoding handle utf8
          Text.Lazy.IO.hPutStr handle (Builder.toLazyText text)
          hClose handle
        -- What an interactive session asks of the compiler. The session's
        -- module comes after the packages and the files that were loaded.
        session parsed verified =
          Compiler
            { parses = \text -> unsafePerformIO $ do
                -- A parse error is thrown from pure code, so it has to be
                -- caught to be answered as a Bool. Nothing else is done here.
                parsed <- stage1 Normal (Vector.singleton (entered text))
                succeeded <$> attempt (pure $! length (Prelude.show parsed)),
              check = \files text -> do
                _ <- checked parsed verified files text
                pure (),
              core = checked parsed verified,
              stable = split
            }
        -- The Core of everything, with the errors of what is not a package's
        -- found: the same hack as forceModules, showing a module finds them.
        --
        -- The files that were loaded are the same from one entry to the
        -- next unless someone changed them. Their errors are looked for when
        -- they are first seen and when their text changes, and after that
        -- only the session's own module is gone through: of the rest, an
        -- entry pays for what it uses.
        checked parsed verified files text = do
          (texts, all) <- together parsed files text
          before <- readIORef verified
          let wanted
                | before == Just texts = Vector.drop (length all - 1) all
                | otherwise = Vector.drop split all
          () <- pure $! seq (length (Prelude.show wanted)) ()
          writeIORef verified (Just texts)
          pure all
        entered text = Root {extensions, name = Repl.source, contents = text}
        -- A file is read at every entry, so that a change to it is seen, but
        -- it is parsed again only if what was read is not what was parsed.
        together parsed files text = do
          loaded <- concat <$> traverse (loadModules extensions) files
          known <- readIORef parsed
          modules <- for loaded $ \file -> case Map.lookup (label file) known of
            Just (contents, modulex) | contents == source file -> pure modulex
            _ -> Vector.head <$> stage1 debug (Vector.singleton file)
          writeIORef parsed $ Map.fromList [(label file, (source file, modulex)) | (file, modulex) <- zip loaded modules]
          rest <- stage1 debug (Vector.singleton (entered text))
          all <- pure (parsedHeaders <> Vector.fromList modules <> rest)
          all <- stage2 debug all
          all <- stage3 debug all
          all <- stage4 debug all
          pure (map source loaded, all)
        label = \case
          Root {name} -> name
          Inner {name} -> name
        source = \case
          Root {contents} -> contents
          Inner {contents} -> contents
        -- The packages are parsed once for a whole session. Nothing of a
        -- module is parsed until it is looked at, and then it stays parsed.
        parsedHeaders = unsafePerformIO (stage1 Normal headers)
        attempt :: IO a -> IO (Either HazyError a)
        attempt action = (Right <$> action) `catch` (pure . Left)
        succeeded = either (const False) (const True)
        noFail = do
          putStrLn "Code didn't fail"
          exitFailure
    case failure of
      Attempt
        | Normal <- debug -> catch run Error.fail
        | Debug <- debug -> run
      Expect error
        | Just typex <- Map.lookup error Error.types ->
            catch (run >> noFail) (Error.allow typex)
        | otherwise -> do
            putStrLn $ "Unknown error type: " ++ error
            exitFailure


-- |
-- Become another program, with its name and arguments. It gives back only
-- if it could not.
foreign import ccall unsafe "execvp" execvp :: CString -> Ptr CString -> IO CInt
