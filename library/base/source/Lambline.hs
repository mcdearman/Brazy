module Lambline where

import Hazy.Prelude (placeholder)

data Editor

newEditor :: Config -> IO Editor
newEditor = placeholder

readline :: Editor -> String -> IO (Either ReadlineError String)
readline = placeholder

data ReadlineError
  = Eof
  | Interrupted

instance Show ReadlineError where
  showsPrec = placeholder

setHelper :: Editor -> Maybe Helper -> IO ()
setHelper = placeholder

addHistoryEntry :: Editor -> String -> IO Bool
addHistoryEntry = placeholder

loadHistory :: Editor -> FilePath -> IO ()
loadHistory = placeholder

saveHistory :: Editor -> FilePath -> IO ()
saveHistory = placeholder

bindSequence :: Editor -> Event -> EventHandler -> IO ()
bindSequence = placeholder

data Config = Config
  { completionType :: CompletionType,
    continuationPrompt :: String
  }

defaultConfig :: Config
defaultConfig = placeholder

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
defaultHelper = placeholder

data Context

data CmdKind

data Candidate

candidate :: String -> Candidate
candidate = placeholder

filenameCompleter :: String -> Int -> Context -> IO (Int, [Candidate])
filenameCompleter = placeholder

matchingBracketHighlighter :: String -> Int -> CmdKind -> String
matchingBracketHighlighter = placeholder

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
noMods = placeholder

data KeyEvent = KeyEvent KeyCode Mods

key, ctrl, alt :: Char -> KeyEvent
key = placeholder
ctrl = placeholder
alt = placeholder
