-- |
-- An interactive session.
--
-- The compiler takes whole programs, so the session is kept as text: every
-- entry that defined something. Each new entry is compiled as one module
-- made of those entries and the new one, and an expression is run on the
-- AxCut machine.
module Repl
  ( Compiler (..),
    source,
    entry,
    repl,
  )
where

import qualified AxCut.Lower as AxCut (lower)
import qualified AxCut.Machine as AxCut (run)
import qualified AxCut.Printer as AxCut (print, printDefinitions)
import qualified AxCut.Tree as AxCut (Program (..))
import Control.Exception
  ( AsyncException (UserInterrupt),
    Exception,
    SomeException,
    catch,
    throw,
  )
import Control.Monad (unless, when)
import Core.Tree.Declaration (Declaration (..))
import Core.Tree.Declarations (Declarations (..))
import qualified Core.Tree.Module as Core (Module (..))
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, nub, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text (length, pack, toStrict, unpack)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import Data.Vector (Vector)
import qualified Diagnostics
import Error (HazyError (HazyError))
import qualified Error (report)
import Generate.Global (Global)
import qualified Generate.Global as Global (Global (..))
import qualified Generate.Go.Module as Javascript (Module (..), generate)
import qualified Generate.Mangle as Mangle (mangle)
import qualified Javascript.Printer.Lexer as Javascript (print, run)
import qualified LLVM.FromAxCut as LLVM (arm64, generateBody, generateDefinitions)
import qualified Javascript.Tree.Module as Module (print)
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import Lambline
  ( Candidate,
    Cmd (Insert),
    CompletionType (List),
    Config (completionType, continuationPrompt),
    Context,
    Editor,
    Event (KeySeq),
    EventContext (contextLine, contextPos),
    EventHandler (Conditional),
    Helper (complete, highlight, highlightPrompt, validate),
    KeyCode (KEnter),
    KeyEvent (KeyEvent),
    Mods (modShift),
    ReadlineError (Eof, Interrupted),
    ValidationResult (Incomplete, Valid),
    addHistoryEntry,
    alt,
    bindSequence,
    candidate,
    ctrl,
    defaultConfig,
    defaultHelper,
    filenameCompleter,
    key,
    loadHistory,
    matchingBracketHighlighter,
    newEditor,
    noMods,
    readline,
    saveHistory,
    setHelper,
  )
import Repl.Entry (lineBreak, names, needsContinuation, tokens, wordStart)
import qualified Sequent.FromCore as Sequent (generate)
import qualified Sequent.Printer as Sequent (print, printDefinitions)
import qualified Sequent.Runtime as Runtime
import qualified Sequent.Tree as Sequent (Program (..))
import Sequent.Tree (globals)
import qualified Syntax.Position as Position (file, line)
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import System.IO
  ( BufferMode (LineBuffering),
    hFlush,
    hIsTerminalDevice,
    hPutStrLn,
    hSetBuffering,
    hSetEncoding,
    stderr,
    stdin,
    stdout,
    utf8,
  )
import qualified Viewer

-- |
-- What the session needs of the compiler. Each takes the text of the
-- session's module; the files are the ones that were loaded.
data Compiler = Compiler
  { -- | Whether the text parses.
    parses :: Text -> Bool,
    -- | Check the module. An error in it is thrown as a 'HazyError'.
    check :: [FilePath] -> Text -> IO (),
    -- | The Core of every module, the session's last.
    core :: [FilePath] -> Text -> IO (Vector Core.Module)
  }

-- |
-- Run something, and give back what it threw if it threw.
attempt :: (Exception e) => IO a -> IO (Either e a)
attempt action = (Right <$> action) `catch` (pure . Left)

-- |
-- The name of the session's module, and what positions call its text.
moduleName, source :: Text
moduleName = pack "Repl"
source = pack "<repl>"

-- |
-- The name an entered expression is bound to. An apostrophe inside it keeps
-- it out of the way of names anyone would write.
entryName :: String
entryName = "it'repl"

entry :: Global
entry = Runtime.global moduleName (pack entryName)

-- |
-- An entry that defined something.
data Unit = Unit
  { number :: !Int,
    text :: String
  }

data Session = Session
  { -- | The files and directories that were loaded.
    files :: [FilePath],
    -- | What has been defined at the prompt, oldest first.
    units :: [Unit],
    -- | How many entries have been taken, to number the next.
    count :: !Int
  }

-- |
-- A run of lines of the session's module that came from one entry: what to
-- call it, the line it starts on, how far in it was moved, and its text.
data Part = Part
  { label :: Text,
    start :: !Int,
    indent :: !Int,
    content :: String
  }

-- |
-- The session's module as text, and where each entry is in it.
data Layout = Layout
  { modulex :: Text,
    parts :: [Part]
  }

-- |
-- What an expression is put inside to make a declaration of it: the lines
-- before it and the lines after it.
data Wrapper = Wrapper [String] [String]

showing, acting, bare :: Wrapper
showing =
  Wrapper
    [entryName ++ " :: Prelude.IO ()", entryName ++ " = Prelude.print ("]
    ["  )"]
acting =
  Wrapper
    [entryName ++ " :: Prelude.IO ()", entryName ++ " = ("]
    ["  ) Prelude.>> Prelude.return ()"]
bare = Wrapper [entryName ++ " = ("] ["  )"]

rows :: String -> [String]
rows string = case break (== '\n') string of
  (row, _ : rest) -> row : rows rest
  (row, []) -> [row]

-- |
-- What an entry is called in a report of what is wrong with it.
entryLabel :: Int -> Text
entryLabel number = pack $ "<entry " ++ show number ++ ">"

-- |
-- Lay the session's module out: the entries that defined something, and
-- then an expression inside a wrapper, moved in under it.
layout :: [Unit] -> Maybe (Wrapper, Unit) -> Layout
layout units expression =
  Layout
    { modulex = pack $ intercalate "\n" (header ++ concat bodies ++ concat tail) ++ "\n",
      parts = definitions ++ asked
    }
  where
    header = ["module " ++ unpack moduleName ++ " where", ""]
    bodies = [rows text ++ [""] | Unit {text} <- units]
    starts = scanl (+) (length header + 1) (map length bodies)
    definitions = do
      (Unit {number, text}, start) <- zip units starts
      pure Part {label = entryLabel number, start, indent = 0, content = text}
    (tail, asked) = case expression of
      Nothing -> ([], [])
      Just (Wrapper before after, Unit {number, text}) ->
        ( [before, map ("  " ++) (rows text), after],
          [ Part
              { label = entryLabel number,
                start = last starts + length before,
                indent = 2,
                content = text
              }
          ]
        )

-- |
-- The entry a line of the session's module came from.
partAt :: Layout -> Int -> Maybe Part
partAt Layout {parts} line = case filter inside parts of
  part : _ -> Just part
  [] -> Nothing
  where
    inside Part {start, content} = line >= start && line < start + length (rows content)

-- |
-- Say what is wrong, against the entry it is in and not the module the
-- entries were put together into.
complain :: Layout -> HazyError -> IO ()
complain arranged@Layout {modulex, parts} failure = do
  let report = relocate (Error.report failure)
      relocate
        Diagnostics.Report
          { Diagnostics.code = code,
            Diagnostics.message = message,
            Diagnostics.location = location,
            Diagnostics.related = related
          } =
          Diagnostics.Report
            { Diagnostics.code = code,
              Diagnostics.message = message,
              Diagnostics.location = move location,
              Diagnostics.related = [(move other, note) | (other, note) <- related]
            }
      move
        location@Diagnostics.Location
          { Diagnostics.file = file,
            Diagnostics.line = line,
            Diagnostics.column = column
          }
          | file == source,
            Just Part {label, start, indent} <- partAt arranged line =
              Diagnostics.Location
                { Diagnostics.file = label,
                  Diagnostics.line = line - start + 1,
                  Diagnostics.column = max 1 (column - indent)
                }
          | otherwise = location
      known =
        Map.fromList $
          (source, modulex) : [(label, pack content) | Part {label, content} <- parts]
  loaded <- Diagnostics.load [report]
  hFlush stdout
  Diagnostics.print stderr (Map.union known loaded) [report]

-- |
-- Whether an entry parses, as declarations or as an expression.
parsesEntry :: Compiler -> String -> Bool
parsesEntry compiler text = declares compiler text || parses compiler (modulex asked)
  where
    asked = layout [] (Just (bare, Unit {number = 0, text}))

-- |
-- Whether an entry is declarations.
declares :: Compiler -> String -> Bool
declares Compiler {parses} text = parses (modulex (layout [Unit {number = 0, text}] Nothing))

-- |
-- Take declarations into the session. A declaration of a name that an
-- earlier entry declared replaces that entry, as a whole.
--
-- Hazy lets the equations of a function be apart from each other, so a
-- second definition of a name would otherwise be taken as more equations
-- of the first, which would never be reached.
define :: Compiler -> Session -> String -> IO Session
define Compiler {check} session@Session {files, units, count} text = do
  let again Unit {text = old} = any (`elem` names text) (names old)
  forget (filter again units)
  go (filter (not . again) units)
  where
    new = Unit {number = count + 1, text}
    -- Say which other names went with the entries that were replaced.
    forget replaced = do
      let lost = filter (`notElem` names text) (concat [names old | Unit {text = old} <- replaced])
      unless (null lost) $
        putStrLn $
          "(forgot " ++ intercalate ", " lost ++ ", declared in the entry that was replaced)"
    go older = do
      let arranged = layout (older ++ [new]) Nothing
      result <- attempt (check files (modulex arranged))
      case result of
        Right () -> pure session {units = older ++ [new], count = count + 1}
        Left failure@(HazyError typex position others _)
          -- A name the text did not show, such as a constructor.
          | "Duplicate" `isPrefixOf` show typex,
            replaced@(_ : _) <- filter (declared arranged (position : others)) older -> do
              forget replaced
              go (filter (not . declared arranged (position : others)) older)
          | otherwise -> session {count = count + 1} <$ complain arranged failure
    -- Whether one of some places is in an entry.
    declared arranged places Unit {number} = or $ do
      place <- places
      pure $
        Position.file place == source && case partAt arranged (Position.line place) of
          Just Part {label} -> label == entryLabel number
          Nothing -> False

-- |
-- Evaluate an expression. It is shown if it can be, and run if it is an
-- action. Which it is is found by trying: first as something to show, and
-- if that does not check, as an action.
evaluate :: Compiler -> Session -> String -> IO ()
evaluate compiler@Compiler {check} Session {files, units, count} text = do
  let asked = Unit {number = count + 1, text}
      shown = layout units (Just (showing, asked))
      acted = layout units (Just (acting, asked))
  first <- attempt (check files (modulex shown))
  case first of
    Right () -> run compiler files shown
    Left failure -> do
      second <- attempt (check files (modulex acted))
      case second of
        Right () -> run compiler files acted
        Left HazyError {} -> complain shown failure

-- |
-- Run the session's module on the AxCut machine. The session goes on
-- whatever the program does.
run :: Compiler -> [FilePath] -> Layout -> IO ()
run Compiler {core} files arranged = do
  evaluation `catch` complain arranged `catch` stopped `catch` interrupted
  hFlush stdout
  where
    evaluation = do
      modules <- core files (modulex arranged)
      AxCut.run (AxCut.lower (Sequent.generate entry modules))
    -- The program stopped itself, having said why.
    stopped :: ExitCode -> IO ()
    stopped _ = pure ()
    interrupted :: AsyncException -> IO ()
    interrupted = \case
      UserInterrupt -> putStrLn "Interrupted."
      other -> hPutStrLn stderr (show other)

-- |
-- A form the compiler puts a program in on the way to running it: what it is
-- called, and the text of it for some of the session's module. A new form,
-- such as the native backend's, is one more of these.
--
-- The words are the ones that belong to the form's notation, which the
-- viewer picks out.
data Form = Form String [String] (Selection -> Vector Core.Module -> Lazy.Text)

-- |
-- What to show: the body of one definition, from any module, or everything
-- of the session's module that the expression that was asked about needs.
data Selection
  = Definition Global
  | Asked

-- |
-- The definition a name stands for, if what was asked about is only a name
-- and the name is of a definition at the top of some module. Then it is the
-- definition's body that is wanted, and not the use of its name.
named :: String -> Vector Core.Module -> Selection
named text modules = case filter matches (Set.toList mentioned) of
  global : _ -> Definition global
  [] -> Asked
  where
    wanted = case trim text of
      '(' : operator | not (null operator), last operator == ')' -> init operator
      name -> name
    matches Global.Global {name} = unpack name == wanted
    -- A use of a name compiles to a mention of its definition.
    mentioned = case Map.lookup entry definitions of
      Just definition | single -> globals definition
      _ -> Set.empty
    single = length (tokens (trim text)) == 1 || take 1 (trim text) == "("
    Sequent.Program {definitions} = Sequent.generate entry modules

forms :: [Form]
forms =
  [ Form "Core" [] $ \selection modules ->
      let (home, wanted) = case selection of
            Definition Global.Global {path, name} -> (path, name)
            Asked -> (Global.path entry, pack entryName)
       in Lazy.Text.pack $ concat $ do
            Core.Module {name = path, declarations = Declarations {terms}} <- toList modules
            Declaration {name, definition} <- if path == home then toList terms else []
            if Mangle.mangle name == wanted
              then [unpack (Mangle.mangle name) ++ " =\n" ++ reflow (show definition)]
              else [],
    Form "Sequent" ["mu", "fun", "thunk", "case", "apply", "force", "letrec", "abort", "halt", "entry"] $
      \selection modules ->
      Builder.toLazyText $ case selection of
        Definition _ -> Sequent.printDefinitions (Sequent.definitions (chosen selection modules))
        Asked -> Sequent.print (chosen selection modules),
    Form "AxCut" axcutWords $ \selection modules ->
      Builder.toLazyText $ case selection of
        Definition _ -> AxCut.printDefinitions (AxCut.definitions (AxCut.lower (chosen selection modules)))
        Asked -> AxCut.print (AxCut.lower (chosen selection modules)),
    Form "LLVM" llvmWords $ \selection modules ->
      Builder.toLazyText $ case selection of
        Definition _ -> LLVM.generateDefinitions LLVM.arm64 (AxCut.lower (chosen selection modules))
        Asked -> LLVM.generateBody LLVM.arm64 (AxCut.lower (chosen selection modules)),
    Form "Javascript" ["const", "let", "return", "if", "break", "export", "import", "from", "this"] $
      \selection modules ->
        let home = case selection of
              Definition Global.Global {path} -> path
              Asked -> Global.path entry
            -- Javascript is generated a module at a time. A definition is
            -- the constant the module exports under its name.
            chosen' statements = case selection of
              Asked -> statements
              Definition Global.Global {name = wanted} ->
                let sources = [source | Javascript.Export source target <- statements, target == wanted]
                    keep = \case
                      Javascript.Const source _ -> source `elem` sources
                      Javascript.Export _ target -> target == wanted
                      _ -> False
                 in filter keep statements
         in Lazy.Text.pack $ concat $ do
              Javascript.Module {name, statements} <- toList (Javascript.generate modules)
              if name == home
                then
                  [ reflow $
                      Lazy.Text.unpack $
                        Builder.toLazyText $
                          Javascript.run $
                            Javascript.print $
                              Module.print (chosen' statements)
                  ]
                else []
  ]
  where
    axcutWords =
      [ "substitute",
        "let",
        "switch",
        "new",
        "invoke",
        "force",
        "literal",
        "global",
        "primitive",
        "rec",
        "abort",
        "halt",
        "entry"
      ]
    llvmWords =
      [ "define",
        "declare",
        "internal",
        "global",
        "private",
        "constant",
        "tailcc",
        "musttail",
        "call",
        "ret",
        "store",
        "load",
        "getelementptr",
        "inttoptr",
        "ptrtoint",
        "lshr",
        "and",
        "switch",
        "label",
        "unreachable",
        "align",
        "to",
        "void",
        "ptr",
        "i64",
        "i8"
      ]
    -- The program for the entry, with only the definitions that are wanted.
    chosen selection modules =
      let Sequent.Program {definitions, entry = starting} = Sequent.generate entry modules
          mine global@Global.Global {path} _ = case selection of
            Definition wanted -> global == wanted
            Asked -> path == Global.path entry
       in Sequent.Program
            { definitions = Map.filterWithKey mine definitions,
              entry = starting
            }

-- |
-- Break text that is all on one line, as derived 'Show' instances and the
-- Javascript printer write it, at its brackets and separators.
reflow :: String -> String
reflow = unlines . filter (not . null) . map (dropWhileEnd isSpace) . lines . go (0 :: Int)
  where
    go depth = \case
      [] -> []
      '"' : rest ->
        let (literal, after) = string rest
         in '"' : literal ++ go depth after
      '{' : '}' : rest -> '{' : '}' : go depth rest
      '[' : ']' : rest -> '[' : ']' : go depth rest
      character : rest
        | character `elem` "{[" -> character : line (depth + 1) ++ go (depth + 1) (dropWhile (== ' ') rest)
        | character `elem` "}]" -> line (depth - 1) ++ character : go (depth - 1) rest
        | character `elem` ",;" -> character : line depth ++ go depth (dropWhile (== ' ') rest)
        | otherwise -> character : go depth rest
    line depth = '\n' : replicate (2 * max 0 depth) ' '
    string = \case
      '\\' : escaped : rest ->
        let (literal, after) = string rest
         in ('\\' : escaped : literal, after)
      '"' : rest -> ("\"", rest)
      character : rest ->
        let (literal, after) = string rest
         in (character : literal, after)
      [] -> ([], [])

-- |
-- An expression or a definition in each of the compiler's forms, or in the
-- ones that are named.
rendered ::
  Compiler ->
  Session ->
  [String] ->
  String ->
  IO (Either (Layout, HazyError) [(String, [String], Lazy.Text)])
rendered Compiler {check, core} Session {files, units, count} wanted text = do
  let arranged = layout units (Just (bare, Unit {number = count + 1, text}))
  result <- attempt $ do
    check files (modulex arranged)
    modules <- core files (modulex arranged)
    let selection = named text modules
    sequence $ do
      Form name words render <- forms
      if null wanted || name `elem` wanted
        then pure $ do
          let page = render selection modules
          -- An error in the program is found as the text is made.
          () <- pure $! seq (Lazy.Text.length page) ()
          pure (name, words, page)
        else []
  pure $ case result of
    Left failure -> Left (arranged, failure)
    Right pages -> Right pages

-- |
-- Print one form of an expression or a definition.
inspect :: Compiler -> Session -> String -> String -> IO ()
inspect compiler session form text = do
  result <- rendered compiler session [form] text
  case result of
    Left (arranged, failure) -> complain arranged failure
    Right pages -> mapM_ (\(_, _, page) -> Text.Lazy.IO.putStr page) pages

-- |
-- Show every form of an expression or a definition, a page for each. With
-- no terminal to draw on, they are printed one after another.
browse :: Compiler -> Session -> String -> IO ()
browse compiler session text = do
  result <- rendered compiler session [] text
  terminal <- (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stdout
  case result of
    Left (arranged, failure) -> complain arranged failure
    Right pages
      | terminal ->
          Viewer.view
            (pack (trim text))
            [ Viewer.Page
                { Viewer.title = pack name,
                  Viewer.content = Lazy.Text.toStrict page,
                  Viewer.keywords = map pack words
                }
              | (name, words, page) <- pages
            ]
      | otherwise -> mapM_ section pages
  where
    section (name, _, page) = do
      putStrLn $ "=== " ++ name ++ " ==="
      Text.Lazy.IO.putStr page

help :: String
help =
  unlines
    [ "  EXPR            evaluate: an IO action is run, any other value is shown",
      "  DECLS           define functions, types, classes, instances or imports",
      "  :load PATH      load a file or a directory of modules, to import from",
      "  :reset          forget what was defined and loaded",
      "  :ir EXPR        browse an expression or a definition in every form the",
      "                  compiler puts it in, a page for each; Tab turns the page",
      "  :sequent EXPR   show an expression in the sequent calculus",
      "  :axcut EXPR     show an expression in AxCut",
      "  :help",
      "  :quit",
      "",
      "Enter submits an entry once it parses. One that does not is carried on,",
      "indented, and a blank line ends it. An entry that opens with a type",
      "signature takes equations until the blank line.",
      "Alt-Enter or Ctrl-J starts a new line whatever the entry looks like.",
      "Up and Down move between the lines of an entry, then through the history,",
      "which is kept in .repl_history. Ctrl-R searches it. Tab completes.",
      "",
      "Declaring a name again replaces the entry that declared it.",
      "The compiler does not default numeric types yet: write (1 + 2 :: Int)."
    ]

logo :: String
logo =
  unlines
    [ "      ____",
      "     / __ )_________ _____  __  __",
      "    / __  / ___/ __ `/_  / / / / /",
      "   / /_/ / /  / /_/ / / /_/ /_/ /",
      "  /_____/_/   \\__,_/ /___/\\__, /",
      "                         /____/"
    ]

-- |
-- Whether to colour what is printed: stdout is a terminal and @NO_COLOR@ is
-- not set.
coloured :: IO Bool
coloured = do
  terminal <- hIsTerminalDevice stdout
  noColour <- lookupEnv "NO_COLOR"
  pure $ terminal && maybe True null noColour

printBanner :: IO ()
printBanner = do
  colour <- coloured
  let paint code string
        | colour = "\ESC[" ++ code ++ "m" ++ string ++ "\ESC[0m"
        | otherwise = string
      keyName = paint "1;36"
  putStr $
    unlines $
      [""]
        ++ map (paint "1;32") (rows (dropWhileEnd (== '\n') logo))
        ++ [ "",
             "  " ++ paint "1" "Welcome to the Brazy interactive environment!",
             "",
             "  " ++ keyName ":help" ++ " commands and keys   " ++ keyName ":q" ++ " quit",
             "  "
               ++ keyName "Tab"
               ++ " completes   "
               ++ keyName "Ctrl+R"
               ++ " searches the history   "
               ++ keyName "Ctrl+J"
               ++ " starts a new line",
             ""
           ]

-- |
-- Where the history is kept between sessions: in the directory the prompt
-- was started from.
historyFile :: FilePath
historyFile = ".repl_history"

-- |
-- Start a session with some files loaded.
repl :: Compiler -> [FilePath] -> IO ()
repl compiler files = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- So that what a program prints and what is said about it stay in order.
  hSetBuffering stdout LineBuffering
  -- A script fed through a pipe gets no banner and is not remembered.
  interactive <- hIsTerminalDevice stdin
  when interactive printBanner
  -- Only the first line of an entry carries the prompt. The rest start under
  -- it, behind a prompt of spaces, so that what lines up on screen lines up
  -- for the layout rule.
  editor <- newEditor defaultConfig {continuationPrompt = "  ", completionType = List}
  when interactive (loadHistory editor historyFile)
  loop compiler editor Session {files, units = [], count = 0}
  when interactive (saveHistory editor historyFile)

loop :: Compiler -> Editor -> Session -> IO ()
loop compiler editor session = do
  setHelper editor $
    Just
      defaultHelper
        { validate = \text ->
            if needsContinuation (parsesEntry compiler) text
              then Incomplete
              else Valid Nothing,
          complete = completeInput session,
          highlight = matchingBracketHighlighter,
          -- Green for the prompt, and not for a history search's.
          highlightPrompt = \prompt isDefault ->
            if isDefault then "\ESC[1;32m" ++ prompt ++ "\ESC[0m" else prompt
        }
  -- Enter carries an unfinished entry on, indented, and otherwise falls
  -- through to the validator, which submits it. Alt-Enter and Ctrl-J break
  -- the line whatever the entry looks like, and so does Shift-Enter on a
  -- terminal that tells it apart from Enter.
  let breaking always =
        Conditional $ \_ _ _ context ->
          Insert 1 <$> lineBreak (parsesEntry compiler) always (contextLine context) (contextPos context)
  bindSequence editor (KeySeq [key '\r']) (breaking False)
  mapM_
    (\event -> bindSequence editor (KeySeq [event]) (breaking True))
    [alt '\r', KeyEvent KEnter noMods {modShift = True}, ctrl 'j']
  -- Where lambline is not editing the line itself, as with no TERM, Ctrl-C
  -- arrives as an exception. It abandons the entry there too.
  line <- readline editor "> " `catch` interrupted
  case line of
    Left Eof -> pure ()
    -- Ctrl-C abandons the entry being typed.
    Left Interrupted -> loop compiler editor session
    Right text
      | all isSpace text -> loop compiler editor session
      | otherwise -> do
          _ <- addHistoryEntry editor (dropWhileEnd isSpace text)
          next <- attempt (step compiler session text)
          case next of
            Right Nothing -> pure ()
            Right (Just session) -> loop compiler editor session
            -- Something the compiler does not report as an error in the
            -- program. The session goes on.
            Left failure -> do
              hFlush stdout
              hPutStrLn stderr $ "internal error: " ++ show (failure :: SomeException)
              loop compiler editor session
  where
    interrupted :: AsyncException -> IO (Either ReadlineError String)
    interrupted = \case
      UserInterrupt -> Left Interrupted <$ putStrLn ""
      other -> throw other

-- |
-- Complete by what the cursor is on: a command, a path for @:load@, or a
-- name the session defined.
completeInput :: Session -> String -> Int -> Context -> IO (Int, [Candidate])
completeInput Session {units} line position context
  | ":load " `isPrefixOf` before || ":l " `isPrefixOf` before = filenameCompleter line position context
  | ":" `isPrefixOf` before && not (any isSpace before) = pure (0, matching (map (':' :) commands) before)
  | null word = pure (start, [])
  | otherwise = pure (start, matching defined word)
  where
    before = take position line
    start = wordStart line position
    word = drop start before
    matching candidates prefix = map candidate (sort (nub (filter (prefix `isPrefixOf`) candidates)))
    commands = ["axcut", "help", "ir", "load", "quit", "reset", "sequent"]
    defined = concat [names text | Unit {text} <- units]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- |
-- Handle one entry. 'Nothing' ends the session.
step :: Compiler -> Session -> String -> IO (Maybe Session)
step compiler session line = case break isSpace (trim line) of
  ("", _) -> same
  (':' : command, argument) -> run' command (trim argument)
  _
    | declares compiler text -> Just <$> define compiler session text
    | otherwise -> do
        evaluate compiler session text
        pure $ Just session {count = count session + 1}
  where
    -- A blank line ended the entry; it is not part of it.
    text = dropWhileEnd isSpace line
    same = pure (Just session)
    run' command argument
      | command `elem` ["q", "quit"] = pure Nothing
      | command `elem` ["h", "help", "?"] = putStr help >> same
      | command `elem` ["l", "load"], not (null argument) = do
          let loaded = session {files = nub (files session ++ words argument)}
          -- Compile with nothing asked, to say now what is wrong with the files.
          result <- attempt (check compiler (files loaded) (modulex (layout (units session) Nothing)))
          case result of
            Right () -> pure (Just loaded)
            Left failure -> do
              complain (layout (units session) Nothing) failure
              same
      | command == "reset" = pure $ Just session {files = [], units = []}
      | command == "ir", not (null argument) = do
          browse compiler session argument
          same
      | command == "sequent", not (null argument) = do
          inspect compiler session "Sequent" argument
          same
      | command == "axcut", not (null argument) = do
          inspect compiler session "AxCut" argument
          same
      | command `elem` ["ir", "sequent", "axcut"] = do
          putStrLn $ ":" ++ command ++ " takes an expression or the name of a definition, as in :" ++ command ++ " map"
          same
      | command `elem` ["l", "load"] = do
          putStrLn ":load takes the path of a file or a directory"
          same
      | otherwise = do
          putStrLn $ "unknown command :" ++ command ++ "; try :help"
          same
