-- |
-- A small full-screen viewer for pages of text, with a tab for each page.
-- It knows nothing about the compiler: a page is a title, its text and the
-- words to pick out in it.
--
-- The text is coloured by its shape alone, so that it is easier to scan:
-- the page's keywords, capitalised names, names that start with @\@@,
-- numbers, strings and punctuation.
--
-- The drawing is done by the @brick@ package. Nothing of that package is
-- exported, so that it can be replaced without touching the compiler.
module Viewer
  ( Page (..),
    view,
  )
where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (VtyEvent),
    Direction (Down, Up),
    EventM,
    Padding (Pad),
    ViewportScroll,
    ViewportType (Both),
    Widget,
    attrMap,
    attrName,
    defaultMain,
    get,
    hBox,
    hScrollBy,
    halt,
    modify,
    neverShowCursor,
    padLeftRight,
    padRight,
    raw,
    txt,
    vBox,
    vLimit,
    vScrollBy,
    vScrollPage,
    vScrollToBeginning,
    vScrollToEnd,
    viewport,
    viewportScroll,
    withAttr,
  )
import Brick.Widgets.Border (hBorder)
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isSpace, isUpper)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty

data Page = Page
  { title :: !Text,
    content :: !Text,
    -- | The words that are part of the notation and not names.
    keywords :: [Text]
  }

data State = State
  { pages :: [Page],
    -- | Each page, drawn. A page is drawn the first time it is shown.
    images :: [Vty.Image],
    -- | Which page is showing, counting from zero.
    current :: !Int
  }

-- |
-- What a viewport is called. Each page has its own, so that each keeps its
-- place when another is showing.
newtype Name = Body Int
  deriving (Eq, Ord, Show)

-- |
-- Show some pages under a heading until the viewer is closed. Tab moves to
-- the next page.
view :: Text -> [Page] -> IO ()
view heading pages
  | null pages = pure ()
  | otherwise = void $ defaultMain (app heading) State {pages, images = map image pages, current = 0}

app :: Text -> App State () Name
app heading =
  App
    { appDraw = draw heading,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handle,
      appStartEvent = pure (),
      appAttrMap = const attributes
    }

selected, quiet :: AttrName
selected = attrName "selected"
quiet = attrName "quiet"

attributes :: AttrMap
attributes =
  attrMap
    Vty.defAttr
    [ (selected, Vty.defAttr `Vty.withStyle` Vty.reverseVideo `Vty.withStyle` Vty.bold),
      (quiet, Vty.defAttr `Vty.withStyle` Vty.dim)
    ]

draw :: Text -> State -> [Widget Name]
draw heading State {pages, images, current} =
  [ vBox
      [ vLimit 1 $ hBox $ withAttr quiet (padRight (Pad 2) (txt heading)) : zipWith tab [0 ..] pages,
        hBorder,
        viewport (Body current) Both $ raw $ images !! current,
        hBorder,
        vLimit 1 $ withAttr quiet $ txt help
      ]
  ]
  where
    tab index Page {title}
      | index == current = withAttr selected (padLeftRight 1 (txt title))
      | otherwise = padLeftRight 1 (txt title)
    help = Text.pack "Tab next page   Shift-Tab previous   arrows, PgUp, PgDn, Home, End scroll   q close"

-- |
-- What a piece of text is, as far as its shape says.
data Class
  = Keyword
  | -- | A capitalised name: a constructor, a type or a kind of object.
    Capital
  | -- | A name that starts with @\@@: a definition at the top.
    Global
  | Number
  | Quoted
  | Punctuation
  | Plain

colour :: Class -> Vty.Attr
colour = \case
  Keyword -> Vty.defAttr `Vty.withForeColor` Vty.magenta `Vty.withStyle` Vty.bold
  Capital -> Vty.defAttr `Vty.withForeColor` Vty.cyan
  Global -> Vty.defAttr `Vty.withForeColor` Vty.green
  Number -> Vty.defAttr `Vty.withForeColor` Vty.yellow
  Quoted -> Vty.defAttr `Vty.withForeColor` Vty.yellow
  Punctuation -> Vty.defAttr `Vty.withStyle` Vty.dim
  Plain -> Vty.defAttr

-- |
-- A page as a picture: each line cut into pieces and each piece coloured.
image :: Page -> Vty.Image
image Page {content, keywords} = Vty.vertCat $ map line $ rows content
  where
    known = Set.fromList keywords
    -- An empty page has no size, and neither has an empty line.
    rows text = case Text.lines text of
      [] -> [Text.pack " "]
      found -> found
    line text = case pieces known (Text.unpack (Text.replace (Text.pack "\t") (Text.pack "    ") text)) of
      [] -> Vty.string Vty.defAttr " "
      found -> Vty.horizCat [Vty.string (colour classx) piece | (classx, piece) <- found]

-- |
-- Cut a line into pieces by what they are.
pieces :: Set Text -> String -> [(Class, String)]
pieces known = \case
  [] -> []
  '"' : rest ->
    let (literal, after) = quoted rest
     in (Quoted, '"' : literal) : pieces known after
  '@' : rest ->
    let (name, after) = break (\character -> isSpace character || character `elem` "|;,()") rest
     in (Global, '@' : name) : pieces known after
  '#' : rest@(digit : _)
    | isDigit digit ->
        let (number, after) = span isDigit rest
         in (Number, '#' : number) : pieces known after
  text@(first : rest)
    | isSpace first ->
        let (space, after) = span isSpace text
         in (Plain, space) : pieces known after
    | isDigit first ->
        let (number, after) = span word text
         in (Number, number) : pieces known after
    | word first ->
        let (name, after) = span word text
            classx
              | Text.pack name `Set.member` known = Keyword
              | isUpper first = Capital
              | otherwise = Plain
         in (classx, name) : pieces known after
    | otherwise -> (Punctuation, [first]) : pieces known rest
  where
    word character = isAlphaNum character || character `elem` "_'%"
    -- The rest of a string, and what is after it.
    quoted = \case
      '\\' : escaped : rest ->
        let (literal, after) = quoted rest
         in ('\\' : escaped : literal, after)
      '"' : rest -> ("\"", rest)
      character : rest ->
        let (literal, after) = quoted rest
         in (character : literal, after)
      [] -> ([], [])

handle :: BrickEvent Name () -> EventM Name State ()
handle = \case
  VtyEvent (Vty.EvKey key _) -> case key of
    Vty.KChar '\t' -> turn 1
    Vty.KBackTab -> turn (-1)
    Vty.KChar 'q' -> halt
    Vty.KEsc -> halt
    Vty.KDown -> scroll $ \port -> vScrollBy port 1
    Vty.KChar 'j' -> scroll $ \port -> vScrollBy port 1
    Vty.KUp -> scroll $ \port -> vScrollBy port (-1)
    Vty.KChar 'k' -> scroll $ \port -> vScrollBy port (-1)
    Vty.KRight -> scroll $ \port -> hScrollBy port 4
    Vty.KChar 'l' -> scroll $ \port -> hScrollBy port 4
    Vty.KLeft -> scroll $ \port -> hScrollBy port (-4)
    Vty.KChar 'h' -> scroll $ \port -> hScrollBy port (-4)
    Vty.KPageDown -> scroll $ \port -> vScrollPage port Down
    Vty.KChar ' ' -> scroll $ \port -> vScrollPage port Down
    Vty.KPageUp -> scroll $ \port -> vScrollPage port Up
    Vty.KChar 'b' -> scroll $ \port -> vScrollPage port Up
    Vty.KHome -> scroll vScrollToBeginning
    Vty.KChar 'g' -> scroll vScrollToBeginning
    Vty.KEnd -> scroll vScrollToEnd
    Vty.KChar 'G' -> scroll vScrollToEnd
    _ -> pure ()
  _ -> pure ()

-- |
-- Move a number of pages on, going round at the ends.
turn :: Int -> EventM Name State ()
turn step = modify $ \state@State {pages, current} ->
  state {current = (current + step) `mod` length pages}

-- |
-- Scroll the page that is showing.
scroll :: (ViewportScroll Name -> EventM Name State ()) -> EventM Name State ()
scroll move = do
  State {current} <- get
  move (viewportScroll (Body current))
