-- |
-- What the compiler says about what it is doing, in the manner of Cargo and
-- of Meadow: a line for each thing done, with a bold verb set to the right,
--
-- >    Compiling base
-- >     Finished base in 0.42s
--
-- and, while a thing is being done, a bar that is drawn over itself:
--
-- >     Building [============>            ] 12/69: Data.List
--
-- All of it goes to stderr, so that stdout holds only what was asked for.
-- The bar is drawn only on a terminal, and so is colour, which @NO_COLOR@
-- turns off.
module Status
  ( Status,
    start,
    status,
    progress,
    finished,
    warning,
    barLine,
  )
where

import Control.Monad (when)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import System.IO (hFlush, hIsTerminalDevice, hPutStr, hPutStrLn, stderr)

data Status = Status
  { -- | Whether anything is said at all.
    enabled :: !Bool,
    -- | Whether stderr is a terminal, where a bar can be drawn.
    terminal :: !Bool,
    colour :: !Bool,
    -- | When the work started, in billionths of a second.
    started :: !Word64
  }

-- |
-- Begin. Nothing is said if the compiler was asked to be quiet.
start :: Bool -> IO Status
start enabled = do
  terminal <- hIsTerminalDevice stderr
  noColour <- lookupEnv "NO_COLOR"
  started <- getMonotonicTimeNSec
  pure
    Status
      { enabled,
        terminal,
        colour = terminal && maybe True null noColour,
        started
      }

-- |
-- A verb set to the right in twelve columns, in a colour.
verb :: Bool -> String -> String -> String
verb colour code word
  | colour = "\ESC[1;" ++ code ++ "m" ++ padded ++ "\ESC[0m"
  | otherwise = padded
  where
    padded = replicate (12 - length word) ' ' ++ word

-- |
-- Take the bar off the screen, so that a line can be written where it was.
clear :: Status -> IO ()
clear Status {terminal} = when terminal $ hPutStr stderr "\r\ESC[K"

-- |
-- A thing that is done, in bold green: @   Compiling base@.
status :: Status -> String -> String -> IO ()
status state@Status {enabled, colour} word text = when enabled $ do
  clear state
  hPutStrLn stderr $ verb colour "32" word ++ " " ++ text
  hFlush stderr

-- |
-- Something the program's writer should know of, in bold yellow. It is said
-- even when the compiler was asked to be quiet.
warning :: Status -> String -> IO ()
warning state@Status {colour} text = do
  clear state
  hPutStrLn stderr $ verb colour "33" "Warning" ++ " " ++ text
  hFlush stderr

-- |
-- How far along a thing is: how many of how many, and what is being done
-- now. On a terminal this is a bar, drawn over the last one. Anywhere else
-- nothing is said.
progress :: Status -> String -> Int -> Int -> String -> IO ()
progress Status {enabled, terminal, colour} word done total current =
  when (enabled && terminal) $ do
    hPutStr stderr $ "\r\ESC[K" ++ barLine colour word done total current
    hFlush stderr

-- |
-- The bar, apart from the drawing of it: the verb in bold cyan, twenty five
-- columns of bar, the count and the name, cut short to fit a line.
barLine :: Bool -> String -> Int -> Int -> String -> String
barLine colour word done total current =
  verb colour "36" word ++ " [" ++ bar ++ "] " ++ counts ++ take room tail
  where
    width = 25
    filled
      | total <= 0 = width
      | otherwise = min width (done * width `div` total)
    bar
      | filled >= width = replicate width '='
      | filled == 0 = replicate width ' '
      | otherwise = replicate (filled - 1) '=' ++ ">" ++ replicate (width - filled) ' '
    counts = show done ++ "/" ++ show total
    tail
      | null current = ""
      | otherwise = ": " ++ current
    -- What is left of eighty columns, less one so the line never wraps.
    room = max 0 (79 - 12 - 1 - (width + 2) - 1 - length counts)

-- |
-- The work is over: @    Finished base in 0.42s@.
finished :: Status -> String -> IO ()
finished state@Status {started} text = do
  now <- getMonotonicTimeNSec
  let hundredths = fromIntegral ((now - started + 5000000) `div` 10000000) :: Int
      (seconds, part) = hundredths `divMod` 100
      elapsed = show seconds ++ "." ++ (if part < 10 then "0" else "") ++ show part ++ "s"
  status state "Finished" (text ++ " in " ++ elapsed)
