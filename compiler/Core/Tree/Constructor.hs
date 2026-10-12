module Core.Tree.Constructor where

import qualified Core.Substitute as Substitute
import Core.Tree.Entry (Entry)
import qualified Core.Tree.Entry as Entry
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Constructor (Syntax)
import qualified Semantic.Tree.Constructor as Solved
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

data Constructor scope = Constructor
  { name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (Entry scope)),
    shape :: !(Shape scope)
  }
  deriving (Show)

instance Shift0.Functor Constructor where
  map = Shift.mapDefault

instance Shift.Functor Constructor where
  map = Substitute.mapDefault

instance Substitute.Functor Constructor where
  map category Constructor {name, syntax, entries, shape} =
    Constructor
      { name,
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
      { name,
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
