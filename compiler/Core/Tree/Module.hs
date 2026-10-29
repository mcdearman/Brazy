{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A module in Core: its name and its declarations. A checked module is
-- simplified into one, and the sequent IR is made from these.
module Core.Tree.Module where

import Core.Tree.Declarations (Declarations)
import qualified Core.Tree.Declarations as Declarations (simplify)
import Data.Functor.Identity (Identity)
import qualified Semantic.Check.Go.Module as Semantic
import Semantic.Layout (Normal)
import Semantic.Scope (Global)
import Semantic.Stage (Check)
import Syntax.Variable (FullQualifiers)

data Module = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global
  }
  deriving (Show)

simplify :: Semantic.Module Identity Normal Check Global -> Module
simplify Semantic.Module {name, declarations} =
  Module
    { name,
      declarations = Declarations.simplify declarations
    }
