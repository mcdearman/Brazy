-- |
-- A small library for showing what is wrong with a program against its
-- source. It knows nothing about the compiler: a report is a message and the
-- places it is about.
--
-- The drawing is done by the @diagnose@ package. Nothing of that package is
-- exported, so that it can be replaced without touching the compiler.
module Diagnostics
  ( Location (..),
    Report (..),
    Sources,
    load,
    render,
    print,
  )
where

import Control.Exception (IOException, try)
import Data.Char (isAlphaNum, isPunctuation, isSpace, isSymbol)
import Data.Foldable (foldl')
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Error.Diagnose as Diagnose
import Prettyprinter (unAnnotate)
import System.Environment (lookupEnv)
import System.IO (Handle, IOMode (ReadMode), hIsTerminalDevice, hPutStr, hSetEncoding, utf8, withFile)
import Prelude hiding (print)

-- |
-- A place in a file. Lines and columns count from one.
data Location = Location
  { file :: !Text,
    line :: !Int,
    column :: !Int
  }
  deriving (Eq, Show)

data Report = Report
  { -- | A short name for the kind of error, if it has one.
    code :: !(Maybe Text),
    message :: !Text,
    -- | Where the error is.
    location :: !Location,
    -- | Other places the error is about, each with a note.
    related :: [(Location, Text)]
  }
  deriving (Eq, Show)

-- |
-- The text of files, by the name locations call them.
type Sources = Map Text Text

-- |
-- Read the files that some reports are about. A file that cannot be read is
-- left out, and its reports are shown without their source.
load :: [Report] -> IO Sources
load reports = do
  let names = nub $ do
        Report {location, related} <- reports
        Location {file} <- location : map fst related
        pure file
  found <- traverse read names
  pure $ Map.fromList [(name, text) | (name, Just text) <- zip names found]
  where
    read :: Text -> IO (Maybe Text)
    read name = do
      result <- try $ withFile (Text.unpack name) ReadMode $ \handle -> do
        hSetEncoding handle utf8
        text <- Text.IO.hGetContents handle
        -- The file is closed when this returns, so read all of it now.
        Text.length text `seq` pure text
      pure $ case result of
        Left failure -> const Nothing (failure :: IOException)
        Right text -> Just text

-- |
-- A location that can be shown. The end of the input is past its last line,
-- and the end of a line is past its last character; point at the nearest
-- place there is something to show.
settle :: Sources -> Location -> Maybe Location
settle sources location@Location {file, line, column} = do
  text <- Map.lookup file sources
  let rows = Text.lines text
  if null rows || line < 1
    then Nothing
    else
      Just $
        if line > length rows
          then location {line = length rows, column = Text.length (last rows) + 1}
          else location {column = max 1 (min column (Text.length (rows !! (line - 1)) + 1))}

-- |
-- The line a location is on, if its file is known and has that line.
lineOf :: Sources -> Location -> Maybe Text
lineOf sources location = do
  Location {file, line} <- settle sources location
  text <- Map.lookup file sources
  pure $ Text.lines text !! (line - 1)

-- |
-- Whether a report can be drawn against its source.
placed :: Sources -> Report -> Bool
placed sources Report {location} = not $ isNothing $ lineOf sources location

-- |
-- A report with no source to draw it against: one line, as a compiler
-- prints it.
plain :: Report -> String
plain Report {message, location = Location {file, line, column}} =
  concat [Text.unpack file, ":", show line, ":", show column, " error: ", Text.unpack message]

-- |
-- A location is a point. Stretch it over the word or the operator it starts,
-- so that there is something to underline.
position :: Sources -> Location -> Diagnose.Position
position sources unsettled =
  Diagnose.Position (line, column) (line, column + width) (Text.unpack file)
  where
    location@Location {file, line, column} = maybe unsettled id (settle sources unsettled)
    rest = maybe Text.empty (Text.drop (column - 1)) (lineOf sources location)
    width = max 1 $ case Text.uncons rest of
      Nothing -> 1
      Just (first, _)
        | word first -> Text.length (Text.takeWhile word rest)
        | operator first -> Text.length (Text.takeWhile operator rest)
        | otherwise -> 1
    word character = isAlphaNum character || character == '_' || character == '\''
    operator character =
      (isSymbol character || isPunctuation character)
        && not (isSpace character)
        && character `notElem` "()[]{},;`\"'_"

diagnostic :: Sources -> [Report] -> Diagnose.Diagnostic String
diagnostic sources reports = foldl' Diagnose.addReport files (map report reports)
  where
    files = foldl' add mempty (Map.toList sources)
    add known (name, text) = Diagnose.addFile known (Text.unpack name) (Text.unpack text)
    report Report {code, message, location, related} =
      Diagnose.Err
        (Text.unpack <$> code)
        (Text.unpack message)
        ( (position sources location, Diagnose.This "here")
            : [ (position sources other, Diagnose.Where (Text.unpack note))
                | (other, note) <- related,
                  not $ isNothing $ lineOf sources other
              ]
        )
        []

-- |
-- The reports as text without colour.
render :: Sources -> [Report] -> String
render sources reports = unlines (map plain unplaced) ++ if null drawn then "" else show drawing
  where
    drawn = filter (placed sources) reports
    unplaced = filter (not . placed sources) reports
    drawing =
      unAnnotate $
        Diagnose.prettyDiagnostic Diagnose.WithUnicode (Diagnose.TabSize 4) (diagnostic sources drawn)

-- |
-- Write the reports to a handle, in colour if it is a terminal and
-- @NO_COLOR@ is not set.
print :: Handle -> Sources -> [Report] -> IO ()
print handle sources reports = do
  terminal <- hIsTerminalDevice handle
  noColour <- lookupEnv "NO_COLOR"
  let drawn = filter (placed sources) reports
      unplaced = filter (not . placed sources) reports
  hPutStr handle $ unlines (map plain unplaced)
  if null drawn
    then pure ()
    else
      if terminal && maybe True null noColour
        then
          Diagnose.printDiagnostic
            handle
            Diagnose.WithUnicode
            (Diagnose.TabSize 4)
            Diagnose.defaultStyle
            (diagnostic sources drawn)
        else hPutStr handle (render sources drawn)
