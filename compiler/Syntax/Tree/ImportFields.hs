{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for import data fields
module Syntax.Tree.ImportFields where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser
  ( Parser,
    asum,
    betweenParens,
    sepByComma,
    token,
    try,
  )
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked)
import qualified Syntax.Tree.Marked as Marked
import Syntax.Variable (Name)

data Fields
  = -- |
    -- > import M ( C ( a ) )
    -- >              ^^^^^
    Fields {picks :: !(Strict.Vector (Marked Name Position))}
  | AllFields
  deriving (Show)

parse :: Parser Fields
parse = asum [parseGiven, pure Fields {picks = Strict.Vector.empty}]

-- |
-- What of a type is named, when something is written for it.
parseGiven :: Parser Fields
parseGiven =
  asum
    [ AllFields <$ try (betweenParens (token "..")),
      fields <$> betweenParens (Strict.Vector.fromList <$> sepByComma Marked.parseLiteral)
    ]
  where
    fields picks = Fields {picks}
