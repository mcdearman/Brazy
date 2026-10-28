{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for module export symbols
module Syntax.Tree.ExportSymbol where

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
import qualified Syntax.Variable as Variable (parseTypeOperator)

data Symbol
  = -- |
    -- > module M ( x )
    -- >            ^
    Definition
      { variable :: !(Marked.QualifiedVariable Position)
      }
  | -- |
    -- > module M ( X )
    -- >            ^
    Data
      { typeVariable :: !(Marked.QualifiedConstructorIdentifier Position),
        fields :: !Fields
      }
  | -- |
    -- > module M ( module A )
    -- >            ^^^^^^^^
    Module
      { modulex :: !(Marked.FullQualifiers Position)
      }
  deriving (Show)

parse :: Parser Symbol
parse =
  asum
    [ datax <$> (token "type" *> operator) <*> ImportFields.parse,
      try (datax <$> operator <*> ImportFields.parseGiven),
      definition <$> Marked.parseLiteral,
      datax <$> Marked.parse <*> ImportFields.parse,
      datax <$> operator <*> ImportFields.parse,
      token "module" *> (modulex <$> Marked.parse)
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
    modulex modulex =
      Module
        { modulex
        }
    -- A type operator, in brackets.
    operator = try (betweenParens ((Marked.:@) <$> position <*> Variable.parseTypeOperator))
