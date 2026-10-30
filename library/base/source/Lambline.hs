-- |
-- The line editor of the interactive session, as a compiler made by Hazy has
-- it: lines are read as they are given, and nothing is edited, completed,
-- coloured or remembered. The compiler GHC builds has the real one.
module Lambline where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (pack, unpack)
import System.IO (hFlush, hIsTerminalDevice, stdin, stdout)
import qualified System.IO.Internal as Internal

data Editor = Editor Config (IORef (Maybe Helper))

newEditor :: Config -> IO Editor
newEditor config = Editor config <$> newIORef Nothing

-- An entry: lines are read until what has been read is not said to be
-- unfinished. A prompt is shown only to someone who is there to see it.
readline :: Editor -> String -> IO (Either ReadlineError String)
readline (Editor Config {continuationPrompt} helper) prompt = do
  interactive <- hIsTerminalDevice stdin
  known <- readIORef helper
  let finished text = case known of
        Just Helper {validate} -> case validate text of
          Incomplete -> False
          _ -> True
        Nothing -> True
      ask shown = if interactive then putStr shown >> hFlush stdout else pure ()
      more text = do
        ask continuationPrompt
        next <- Internal.line (Internal.Handle 0)
        case next of
          Nothing -> pure (Right text)
          Just line -> continue (text ++ "\n" ++ unpack line)
      continue text
        | finished text = pure (Right text)
        | otherwise = more text
  ask prompt
  first <- Internal.line (Internal.Handle 0)
  case first of
    Nothing -> pure (Left Eof)
    Just line -> continue (unpack line)

data ReadlineError
  = Eof
  | Interrupted

instance Show ReadlineError where
  showsPrec _ = \case
    Eof -> showString "Eof"
    Interrupted -> showString "Interrupted"

setHelper :: Editor -> Maybe Helper -> IO ()
setHelper (Editor _ helper) = writeIORef helper

addHistoryEntry :: Editor -> String -> IO Bool
addHistoryEntry _ _ = pure False

loadHistory :: Editor -> FilePath -> IO ()
loadHistory _ _ = pure ()

saveHistory :: Editor -> FilePath -> IO ()
saveHistory _ _ = pure ()

bindSequence :: Editor -> Event -> EventHandler -> IO ()
bindSequence _ _ _ = pure ()

data Config = Config
  { completionType :: CompletionType,
    continuationPrompt :: String
  }

defaultConfig :: Config
defaultConfig = Config {completionType = Circular, continuationPrompt = ""}

data CompletionType
  = Circular
  | List

data Helper = Helper
  { complete :: String -> Int -> Context -> IO (Int, [Candidate]),
    highlight :: String -> Int -> CmdKind -> String,
    highlightPrompt :: String -> Bool -> String,
    validate :: String -> ValidationResult
  }

defaultHelper :: Helper
defaultHelper =
  Helper
    { complete = \_ _ _ -> pure (0, []),
      highlight = \text _ _ -> text,
      highlightPrompt = \prompt _ -> prompt,
      validate = \_ -> Valid Nothing
    }

data Context

data CmdKind

data Candidate = Candidate String

candidate :: String -> Candidate
candidate = Candidate

filenameCompleter :: String -> Int -> Context -> IO (Int, [Candidate])
filenameCompleter _ _ _ = pure (0, [])

matchingBracketHighlighter :: String -> Int -> CmdKind -> String
matchingBracketHighlighter text _ _ = text

data ValidationResult
  = Valid (Maybe String)
  | Incomplete
  | Invalid (Maybe String)

data Event
  = AnyKey
  | KeySeq [KeyEvent]

data EventHandler
  = Simple Cmd
  | Conditional (Event -> Int -> Bool -> EventContext -> Maybe Cmd)

data EventContext = EventContext
  { contextLine :: String,
    contextPos :: Int
  }

data Cmd = Insert Int String

data KeyCode = KEnter

data Mods = Mods
  { modCtrl :: Bool,
    modAlt :: Bool,
    modShift :: Bool
  }

noMods :: Mods
noMods = Mods {modCtrl = False, modAlt = False, modShift = False}

data KeyEvent = KeyEvent KeyCode Mods

key, ctrl, alt :: Char -> KeyEvent
key _ = KeyEvent KEnter noMods
ctrl _ = KeyEvent KEnter Mods {modCtrl = True, modAlt = False, modShift = False}
alt _ = KeyEvent KEnter Mods {modCtrl = False, modAlt = True, modShift = False}
