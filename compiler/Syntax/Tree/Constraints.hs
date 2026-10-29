-- |
-- What comes before the type in a type scheme: the class constraints
-- written before @=>@, or else how its values are kept, lazy when nothing
-- is written, strict after @!@, or of a levity given as a type after @~!@.
module Syntax.Tree.Constraints where

import qualified Data.Vector.Strict as Strict
import Syntax.FreeVariables (FreeTypeVariables (..))
import Syntax.Tree.Constraint (Constraint)
import Syntax.Tree.Type (Type)

data Constraints position
  = Constraints !(Strict.Vector (Constraint position))
  | Value {levity :: !(Type position)}
  | Lazy
  | Strict
  deriving (Show)

instance FreeTypeVariables Constraints where
  freeTypeVariables = \case
    Constraints constraints -> foldMap freeTypeVariables constraints
    Value {levity} -> freeTypeVariables levity
    Lazy -> []
    Strict -> []
