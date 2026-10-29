{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A field selector of a data type: where its field is in the first
-- constructor that has it, and whether every constructor holds it there
-- and in the same way ('Uniform', only for a @data@ type), or else where
-- each constructor holds it, if it does ('Disjoint').
module Semantic.Tree.Selector where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Syntax.Position (Position)
import Syntax.Variable (Variable)

data Selector
  = Selector
  { name :: !Variable,
    position :: !Position,
    first, index :: !Int,
    uniform :: !Uniform
  }
  deriving (Show)

data Uniform
  = Uniform
  | Disjoint
      { indexes :: !(Strict.Vector (Strict.Maybe Int))
      }
  deriving (Show)
