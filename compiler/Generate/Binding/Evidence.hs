-- |
-- What a piece of evidence in scope is called while Core is translated
-- into the sequent IR.
module Generate.Binding.Evidence where

import Data.Kind (Type)
import Data.Text (Text)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

type Binding :: Environment -> Type
newtype Binding scope = Binding Text

instance Shift0.Functor Binding where
  map = Shift.mapDefault

instance Shift.Functor Binding where
  map _ (Binding text) = Binding text
