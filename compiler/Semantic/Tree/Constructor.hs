{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Constructor where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.FreeVariables (FreeTypeVariables (freeTypeVariables))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Entry (Entry)
import Semantic.Tree.Type (Type)
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity)
import Syntax.Variable (Variable)
import qualified Syntax.Variable as Variable (Constructor)

data Syntax
  = Standard
  | Infix !Fixity
  | Record !(Strict.Vector Variable)
  deriving (Show)

-- |
-- What type a constructor makes a value of.
data Shape stage scope
  = -- | The type it is declared in, given the type's own parameters, which
    -- are what is in scope for the constructor.
    Ordinary
  | -- | The type it is declared in, given arguments of the constructor's
    -- own: a constructor of a type that is declared with @where@. What is
    -- in scope for it is so many variables of its own, and not the type's
    -- parameters.
    Indexed
      { variables :: !Int,
        arguments :: !(Strict.Vector (Type Position stage scope))
      }
  deriving (Show)

data Constructor stage scope
  = Constructor
  { position :: !Position,
    name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (Entry Position stage scope)),
    shape :: !(Shape stage scope)
  }
  deriving (Show)

instance Shift0.Functor (Constructor stage) where
  map = Shift.mapDefault

instance Shift.Functor (Constructor stage) where
  map category = \case
    Constructor {position, name, syntax, entries, shape} ->
      Constructor
        { position,
          name,
          syntax,
          entries = fmap (Shift.map category) entries,
          shape = case shape of
            Ordinary -> Ordinary
            Indexed {variables, arguments} ->
              Indexed {variables, arguments = fmap (Shift.map category) arguments}
        }

instance FreeTypeVariables Constructor where
  freeTypeVariables target (Constructor {entries, shape}) =
    foldMap (freeTypeVariables target) entries ++ case shape of
      Ordinary -> []
      Indexed {arguments} -> foldMap (freeTypeVariables target) arguments
