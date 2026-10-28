module Core.Tree.Entry where

import Core.Instanciate (Normal, Store (Empty))
import qualified Core.Substitute as Substitute
import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.Forall (ForallOver (..))
import qualified Core.Tree.Forall as Forall
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import Data.Void (Void)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Entry (Restricted (..))
import qualified Semantic.Tree.Entry as Solved
import Semantic.Tree.StrictnessAnnotation (StrictnessAnnotation (..))
import Syntax.Position (Position)

type Entry = EntryF Normal Void

data EntryF instanciate logical scope = Entry
  { position :: !(Store instanciate Position),
    -- | The type of what is held, which may have a @forall@ of its own.
    entry :: !(ForallOver TypeF logical scope),
    strict :: !(TypeF logical scope)
  }
  deriving (Show)

-- |
-- Whether the type of what is held has a @forall@ of its own.
quantified :: EntryF instanciate logical scope -> Bool
quantified Entry {entry = ForallOver {parameters, constraints}} = case constraints of
  None -> not (null parameters)
  Constraints {} -> True

instance Shift0.Functor (EntryF instanciate logical) where
  map = Shift.mapDefault

instance Shift.Functor (EntryF instanciate logical) where
  map category Entry {position, entry, strict} =
    Entry
      { position,
        entry = Shift.map category entry,
        strict = Shift.map category strict
      }

instance (logical ~ Void) => Substitute.Functor (EntryF instanciate logical) where
  map category Entry {position, entry, strict} =
    Entry
      { position,
        entry = Substitute.map category entry,
        strict = Substitute.map category strict
      }

simplify :: Solved.Entry position Check scope -> Entry scope
simplify Solved.Entry {entry = Restricted entry, strict} =
  Entry
    { position = Empty,
      entry = Forall.simplify entry,
      strict = case strict of
        Lazy -> Type.Constructor Type2.Lazy
        Strict -> Type.Constructor Type2.Strict
        Polymorphic {levity} -> Type.simplify levity
    }
