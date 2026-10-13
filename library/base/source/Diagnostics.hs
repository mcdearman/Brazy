-- |
-- A small library for showing what is wrong with a program against its
-- source. It knows nothing about the compiler: a report is a message and the
-- places it is about.
--
-- The compiler that GHC builds has this drawn by the @diagnose@ package. Here
-- the drawing is written out, after the look of that package, so that a
-- compiler made by Hazy says what is wrong in the same way.
module Diagnostics
  ( Location (..),
    Report (..),
    Sources,
    load,
    render,
    print,
  )
where

import Data.Char (isAlphaNum, isPunctuation, isSpace, isSymbol)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Directory (doesFileExist)
import System.IO (Handle, hPutStr)
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
-- Read the files that some reports are about. A file that is not there is
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
      exists <- doesFileExist (Text.unpack name)
      if exists
        then fmap Just (Text.IO.readFile (Text.unpack name))
        else pure Nothing

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
          then Location {file, line = length rows, column = Text.length (last rows) + 1}
          else Location {file, line, column = max 1 (min column (Text.length (rows !! (line - 1)) + 1))}

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
placed sources Report {location} = case lineOf sources location of
  Just _ -> True
  Nothing -> False

-- |
-- A report with no source to draw it against: one line, as a compiler
-- prints it.
plain :: Report -> String
plain Report {message, location = Location {file, line, column}} =
  concat [Text.unpack file, ":", show line, ":", show column, " error: ", Text.unpack message]

-- |
-- A location is a point. Stretch it over the word or the operator it starts,
-- so that there is something to underline: the location as it is shown, and
-- how many columns it covers.
stretch :: Sources -> Location -> (Location, Int)
stretch sources unsettled = (location, width)
  where
    location@Location {column} = maybe unsettled id (settle sources unsettled)
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

-- |
-- One report against its source: what it says, and under that each place it
-- is about, with the line it is on and a mark under what it points at.
draw :: Sources -> Report -> [String]
draw sources Report {code, message, location, related} =
  heading : concat (zipWith place (True : repeat False) places) ++ [replicate (margin + 2) '─' ++ "╯"]
  where
    heading = case code of
      Just name -> "[error " ++ Text.unpack name ++ "]: " ++ Text.unpack message
      Nothing -> "[error]: " ++ Text.unpack message
    places =
      (location, "here")
        : [(other, Text.unpack note) | (other, note) <- related, Just _ <- [lineOf sources other]]
    margin = maximum (3 : [length (show line) | (Location {line}, _) <- map (\(l, n) -> (fst (stretch sources l), n)) places])
    gutter = replicate (margin + 2) ' '
    place first (unsettled, note) =
      [ gutter ++ (if first then "╭──▶ " else "├──▶ ") ++ Text.unpack file ++ "@" ++ range,
        gutter ++ "│",
        " " ++ replicate (margin - length (show line)) ' ' ++ show line ++ " │ " ++ shown,
        -- A mark that stops short of the end of its line is followed by a
        -- space, as the package this is drawn after has it.
        gutter ++ "• " ++ indent ++ "┬" ++ replicate (width - 1) '─' ++ (if column + width <= length text then " " else ""),
        gutter ++ "• " ++ indent ++ "╰╸ " ++ note
      ]
      where
        (Location {file, line, column}, width) = stretch sources unsettled
        range = show line ++ ":" ++ show column ++ "-" ++ show line ++ ":" ++ show (column + width)
        text = maybe "" Text.unpack (lineOf sources unsettled)
        shown = concatMap (\c -> if c == '\t' then "    " else [c]) text
        indent = concatMap (\c -> if c == '\t' then "    " else " ") (take (column - 1) (text ++ repeat ' '))

-- |
-- The reports as text without colour.
render :: Sources -> [Report] -> String
render sources reports = unlines (map plain unplaced ++ concatMap (draw sources) drawn)
  where
    drawn = filter (placed sources) reports
    unplaced = filter (not . placed sources) reports

-- |
-- Write the reports to a handle.
print :: Handle -> Sources -> [Report] -> IO ()
print handle sources reports = hPutStr handle (render sources reports)
