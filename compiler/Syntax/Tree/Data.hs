{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for data types
module Syntax.Tree.Data where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.FreeVariables (TermBindingVariables (..))
import Syntax.Parser
  ( Parser,
    asum,
    betweenBraces,
    betweenParens,
    sepByComma,
    sepEndBySemicolon,
    token,
  )
import Syntax.Position (Position)
import Syntax.Tree.Constructor (Constructor)
import qualified Syntax.Tree.Constructor as Constructor
import Syntax.Tree.GADTConstructor (GADTConstructor)
import qualified Syntax.Tree.GADTConstructor as GADTConstructor
import Syntax.Tree.Type (Type)
import qualified Syntax.Tree.Type as Type
import Syntax.Variable (QualifiedConstructorIdentifier)
import qualified Syntax.Variable as Variable

data Data position
  = -- |
    --  > data T = A
    --  >        ^^^
    ADT
      { constructors :: !(Strict.Vector (Constructor position)),
        derivingx :: !(Strict.Vector QualifiedConstructorIdentifier)
      }
  | -- |
    --  > data T where { A :: T }
    --  >        ^^^^^^^^^^^^^^^^
    --
    -- The type's kind may be said before the constructors, and then the
    -- constructors may be left out:
    --
    --  > data T :: Type -> Type where { A :: T a }
    --  >        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    GADT
      { kind :: !(Strict.Maybe (Type position)),
        gadtConstructors :: !(Strict.Vector (GADTConstructor position)),
        derivingx :: !(Strict.Vector QualifiedConstructorIdentifier)
      }
  deriving (Show)

instance TermBindingVariables Data where
  termBindingVariables ADT {constructors} = foldMap termBindingVariables constructors
  termBindingVariables GADT {gadtConstructors} = foldMap termBindingVariables gadtConstructors

parse :: Parser (Data Position)
parse =
  asum
    [ gadt . Strict.Just
        <$> (token "::" *> Type.parse)
        <*> asum [token "where" *> block, pure Strict.Vector.empty]
        <*> parseDerive,
      gadt Strict.Nothing <$ token "where" <*> block <*> parseDerive,
      adt <$> Constructor.parseMany <*> parseDerive
    ]
  where
    adt constructors derivingx = ADT {constructors, derivingx}
    gadt kind gadtConstructors derivingx = GADT {kind, gadtConstructors, derivingx}
    block = betweenBraces (Strict.Vector.fromList <$> sepEndBySemicolon GADTConstructor.parse)

parseDerive :: Parser (Strict.Vector QualifiedConstructorIdentifier)
parseDerive =
  asum
    [ token "deriving"
        *> asum
          [ betweenParens (Strict.Vector.fromList <$> sepByComma Variable.parse),
            Strict.Vector.singleton <$> Variable.parse
          ],
      pure Strict.Vector.empty
    ]
