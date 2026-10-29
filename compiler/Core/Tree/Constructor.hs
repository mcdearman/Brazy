{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A data constructor in Core: its name, the entries it holds, and, for a
-- constructor of a type declared with @where@, the arguments it gives the
-- type and what it requires of them.
module Core.Tree.Constructor where

import Core.Instanciate (Normal, Store (Empty))
import qualified Core.Substitute as Substitute
import Core.Tree.Entry (EntryF)
import qualified Core.Tree.Entry as Entry
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict
import qualified Semantic.Index.Type2 as Type2
import Data.Void (Void)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Constructor (Syntax)
import qualified Semantic.Tree.Constructor as Solved
import Syntax.Tree.Brand (Brand)
import qualified Syntax.Variable as Variable

-- |
-- What type a constructor makes a value of: see 'Solved.Shape'.
data Shape scope
  = Ordinary
  | Indexed
      { variables :: !Int,
        asked :: !(Strict.Vector (Asked scope)),
        equated :: !(Strict.Vector (Type scope, Type scope)),
        arguments :: !(Strict.Vector (Type scope))
      }
  deriving (Show)

-- |
-- Something a constructor requires of its types: see 'Solved.Asked'.
data Asked scope = Asked
  { classx :: !(Type2.Index scope),
    subject :: !(Type scope)
  }
  deriving (Show)

type Constructor = ConstructorF Normal Void

data ConstructorF instanciate logical scope = Constructor
  { brand :: !(Store instanciate Brand),
    name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (EntryF instanciate logical scope)),
    shape :: !(Shape scope)
  }
  deriving (Show)

instance Shift0.Functor (ConstructorF instanciate logical) where
  map = Shift.mapDefault

instance Shift.Functor (ConstructorF instanciate logical) where
  map category Constructor {brand, name, syntax, entries, shape} =
    Constructor
      { brand,
        name,
        syntax,
        entries = Shift.map category <$> entries,
        shape = Shift.map category shape
      }

instance Shift0.Functor Shape where
  map = Shift.mapDefault

instance Shift.Functor Shape where
  map category = \case
    Ordinary -> Ordinary
    Indexed {variables, asked, equated, arguments} ->
      Indexed
        { variables,
          asked = Shift.map category <$> asked,
          equated = fmap (\(left, right) -> (Shift.map category left, Shift.map category right)) equated,
          arguments = Shift.map category <$> arguments
        }

instance Shift0.Functor Asked where
  map = Shift.mapDefault

instance Shift.Functor Asked where
  map category Asked {classx, subject} =
    Asked {classx = Shift.map category classx, subject = Shift.map category subject}

instance (logical ~ Void) => Substitute.Functor (ConstructorF instanciate logical) where
  map category Constructor {brand, name, syntax, entries, shape} =
    Constructor
      { brand,
        name,
        syntax,
        entries = Substitute.map category <$> entries,
        shape = case shape of
          Ordinary -> Ordinary
          Indexed {variables, asked, equated, arguments} ->
            Indexed
              { variables,
                asked = mapAsked <$> asked,
                equated = fmap (\(left, right) -> (Substitute.map category left, Substitute.map category right)) equated,
                arguments = Substitute.map category <$> arguments
              }
      }
    where
      mapAsked Asked {classx, subject} =
        Asked {classx = Substitute.map category classx, subject = Substitute.map category subject}

simplifyAsked :: Solved.Asked Check scope -> Asked scope
simplifyAsked Solved.Asked {classx, subject} = Asked {classx, subject = Type.simplify subject}

simplify :: Solved.Constructor Check scope -> Constructor scope
simplify = \case
  Solved.Constructor {name, syntax, entries, shape} ->
    Constructor
      { brand = Empty,
        name,
        syntax,
        entries = Entry.simplify <$> entries,
        shape = case shape of
          Solved.Ordinary -> Ordinary
          Solved.Indexed {variables, asked, equated, arguments} ->
            Indexed
              { variables,
                asked = simplifyAsked <$> asked,
                equated = fmap (\(left, right) -> (Type.simplify left, Type.simplify right)) equated,
                arguments = Type.simplify <$> arguments
              }
      }
