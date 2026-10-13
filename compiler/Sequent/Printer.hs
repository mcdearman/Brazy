-- |
-- Sequent programs as text, for reading.
module Sequent.Printer (print, printDefinitions, global, parts, literal) where

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text (Text)
import qualified Data.Text as Text (map, split)
import Data.Text.Lazy.Builder (Builder, fromString, fromText)
import qualified Generate.Global as Global (Global (..))
import qualified Generate.Mangle as Mangle
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Literal (..),
    Producer (..),
    Program (..),
    Statement (..),
    Strategy (..),
    separator,
  )
import Prelude hiding (print)

print :: Program -> Builder
print Program {definitions, entry} =
  mconcat
    [ printDefinitions definitions,
      fromString "entry = ",
      statement 1 entry,
      fromString "\n"
    ]

printDefinitions :: Map Global.Global Producer -> Builder
printDefinitions definitions = mconcat $ do
  (name, definition) <- Map.toList definitions
  pure $ mconcat [global name, fromString " = ", producer 1 definition, fromString "\n"]

global :: Global.Global -> Builder
global Global.Global {path, name} =
  mconcat
    [ fromString "@",
      mconcat $ intersperse (fromString ".") $ fromText <$> toList (Mangle.path' path),
      fromString ".",
      -- A definition that was inside another is shown under its name.
      fromText (Text.map (\character -> if character == separator then '.' else character) name)
    ]

-- |
-- The name of a definition in parts: the definition at the top of a module
-- that it is or was inside, as that is shown, and what it is called from
-- there, one name for each definition it was inside.
parts :: Global.Global -> (Builder, [Text])
parts whole@Global.Global {name} = case Text.split (== separator) name of
  first : inside -> (global whole {Global.name = first}, inside)
  [] -> (global whole, [])

literal :: Literal -> Builder
literal = \case
  Int int -> fromString (show int)
  Integer integer -> fromString (show integer) <> fromString "n"
  Text text -> fromString (show text)

commas :: [Builder] -> Builder
commas = mconcat . intersperse (fromText $ pack ", ")

indent :: Int -> Builder
indent depth = fromString $ '\n' : replicate (2 * depth) ' '

binder :: Binder -> Builder
binder Binder {name, strategy} = case strategy of
  ByNeed -> fromText name
  ByValue -> fromString "!" <> fromText name
  ByName -> fromString "~" <> fromText name

producer :: Int -> Producer -> Builder
producer depth = \case
  Variable name -> fromText name
  Global name -> global name
  Literal value -> literal value
  Constructor tag [] -> fromString "#" <> fromString (show tag)
  Constructor tag fields ->
    mconcat
      [ fromString "#",
        fromString (show tag),
        fromString "(",
        commas (producer depth <$> fields),
        fromString ")"
      ]
  Mu covariable body ->
    mconcat
      [ fromString "mu ",
        fromText covariable,
        fromString ".",
        indent (depth + 1),
        statement (depth + 1) body
      ]
  Function parameters covariable body ->
    mconcat
      [ fromString "fun(",
        commas (binder <$> parameters),
        fromString "; ",
        fromText covariable,
        fromString ").",
        indent (depth + 1),
        statement (depth + 1) body
      ]
  Thunk covariable body ->
    mconcat
      [ fromString "thunk(",
        fromText covariable,
        fromString ").",
        indent (depth + 1),
        statement (depth + 1) body
      ]

consumer :: Int -> Consumer -> Builder
consumer depth = \case
  Covariable name -> fromText name
  MuTilde name body ->
    mconcat
      [ fromString "mu~ ",
        binder name,
        fromString ".",
        indent (depth + 1),
        statement (depth + 1) body
      ]
  Case alternatives otherwise ->
    mconcat
      [ fromString "case {",
        mconcat $ do
          Alternative tag names body <- alternatives
          pure $
            mconcat
              [ indent (depth + 1),
                fromString "#",
                fromString (show tag),
                fromString "(",
                commas (binder <$> names),
                fromString ") => ",
                statement (depth + 2) body
              ],
        case otherwise of
          Nothing -> mempty
          Just body ->
            mconcat
              [ indent (depth + 1),
                fromString "_ => ",
                statement (depth + 2) body
              ],
        indent depth,
        fromString "}"
      ]
  Apply arguments result ->
    mconcat
      [ fromString "apply(",
        commas (producer depth <$> arguments),
        fromString "; ",
        consumer depth result,
        fromString ")"
      ]
  Force result ->
    mconcat
      [ fromString "force(",
        consumer depth result,
        fromString ")"
      ]

statement :: Int -> Statement -> Builder
statement depth = \case
  Cut left right ->
    mconcat
      [ fromString "<",
        producer depth left,
        fromString " | ",
        consumer depth right,
        fromString ">"
      ]
  Primitive primitive arguments result ->
    mconcat
      [ fromString (show primitive),
        fromString "(",
        commas (producer depth <$> arguments),
        fromString "; ",
        consumer depth result,
        fromString ")"
      ]
  LetRec bindings body ->
    mconcat
      [ fromString "letrec {",
        mconcat $ do
          (name, value) <- bindings
          pure $
            mconcat
              [ indent (depth + 1),
                fromText name,
                fromString " = ",
                producer (depth + 2) value
              ],
        indent depth,
        fromString "} ",
        statement depth body
      ]
  Abort -> fromString "abort"
  Halt -> fromString "halt"
