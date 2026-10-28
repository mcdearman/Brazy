{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for module import symbols
module Syntax.Tree.ImportSymbol where

import Syntax.Parser
  ( Parser,
    asum,
    betweenParens,
    position,
    token,
    try,
  )
import Syntax.Position (Position)
import Syntax.Tree.ImportFields (Fields)
import qualified Syntax.Tree.ImportFields as ImportFields
import qualified Syntax.Tree.Marked as Marked
import qualified Syntax.Variable as Variable (parseLocalTypeOperator)

data Symbol
  = -- |
    -- > import M ( x )
    -- >            ^
    Definition
      { variable :: !(Marked.Variable Position)
      }
  | -- |
    -- > import M ( C )
    -- >            ^
    Data
      { typeVariable :: !(Marked.ConstructorIdentifier Position),
        fields :: !Fields
      }
  deriving (Show)

parse :: Parser Symbol
parse =
  asum
    [ datax <$> (token "type" *> operator) <*> ImportFields.parse,
      try (datax <$> operator <*> ImportFields.parseGiven),
      definition <$> Marked.parseLiteral,
      datax <$> Marked.parse <*> ImportFields.parse,
      datax <$> operator <*> ImportFields.parse
    ]
  where
    definition variable =
      Definition
        { variable
        }
    datax typeVariable fields =
      Data
        { typeVariable,
          fields
        }
    -- A type operator, in brackets.
    operator = try (betweenParens ((Marked.:@) <$> position <*> Variable.parseLocalTypeOperator))
