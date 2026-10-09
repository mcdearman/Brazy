-- |
-- AxCut programs as text, for reading.
module AxCut.Printer (print, printDefinitions) where

import AxCut.Tree
  ( Clause (..),
    Object (..),
    Program (..),
    Recursive (..),
    Statement (..),
  )
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, fromString, fromText)
import Generate.Global (Global)
import qualified Sequent.Printer as Sequent (global, literal)
import Prelude hiding (print)

print :: Program -> Builder
print Program {definitions, entry} =
  mconcat
    [ printDefinitions definitions,
      fromString "entry =",
      statement 1 entry,
      fromString "\n"
    ]

printDefinitions :: Map Global Object -> Builder
printDefinitions definitions = mconcat $ do
  (name, definition) <- Map.toList definitions
  pure $
    mconcat
      [ Sequent.global name,
        fromString " = ",
        objectx 0 definition,
        fromString "\n"
      ]

indent :: Int -> Builder
indent depth = fromString $ '\n' : replicate (2 * depth) ' '

names :: [Text] -> Builder
names = mconcat . intersperse (fromString " ") . map fromText

objectx :: Int -> Object -> Builder
objectx depth Object {kind, captured, method, parameters, body} =
  mconcat
    [ fromString (show kind),
      fromString "[",
      names captured,
      fromString "] { ",
      fromString (show method),
      fromString "(",
      names parameters,
      fromString ") =>",
      statement (depth + 2) body,
      fromString " }"
    ]

statement :: Int -> Statement -> Builder
statement depth = \case
  Substitute pairs rest ->
    mconcat
      [ indent depth,
        fromString "substitute [",
        mconcat $ intersperse (fromString ", ") $ do
          (new, old) <- pairs
          pure $
            if new == old
              then fromText new
              else mconcat [fromText new, fromString " := ", fromText old],
        fromString "]",
        statement depth rest
      ]
  Let name tag count rest ->
    mconcat
      [ indent depth,
        fromString "let ",
        fromText name,
        fromString " = #",
        fromString (show tag),
        fromString "/",
        fromString (show count),
        statement depth rest
      ]
  Switch clauses otherwise ->
    mconcat
      [ indent depth,
        fromString "switch {",
        mconcat $ do
          Clause tag fields body <- clauses
          pure $
            mconcat
              [ indent (depth + 1),
                fromString "#",
                fromString (show tag),
                fromString "(",
                names fields,
                fromString ") =>",
                statement (depth + 2) body
              ],
        case otherwise of
          Nothing -> mempty
          Just body ->
            mconcat
              [ indent (depth + 1),
                fromString "_ =>",
                statement (depth + 2) body
              ],
        indent depth,
        fromString "}"
      ]
  New name value rest ->
    mconcat
      [ indent depth,
        fromString "new ",
        fromText name,
        fromString " = ",
        objectx depth value,
        statement depth rest
      ]
  Invoke method -> indent depth <> fromString "invoke " <> fromString (show method)
  Force -> indent depth <> fromString "force"
  Literal name literal rest ->
    mconcat
      [ indent depth,
        fromString "literal ",
        fromText name,
        fromString " = ",
        Sequent.literal literal,
        statement depth rest
      ]
  Global name global rest ->
    mconcat
      [ indent depth,
        fromString "global ",
        fromText name,
        fromString " = ",
        Sequent.global global,
        statement depth rest
      ]
  Primitive name primitive rest ->
    mconcat
      [ indent depth,
        fromString "primitive ",
        fromText name,
        fromString " = ",
        fromString (show primitive),
        statement depth rest
      ]
  Rec objects rest ->
    mconcat
      [ indent depth,
        fromString "rec {",
        mconcat $ do
          Recursive {name, object = value, others} <- objects
          pure $
            mconcat
              [ indent (depth + 1),
                fromText name,
                fromString " = ",
                objectx (depth + 1) value,
                fromString " with [",
                names others,
                fromString "]"
              ],
        indent depth,
        fromString "}",
        statement depth rest
      ]
  Abort -> indent depth <> fromString "abort"
  Halt -> indent depth <> fromString "halt"
