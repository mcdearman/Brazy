module Generate.Binding.Term where

import Data.Kind (Type)
import Generate.Variable (Variable)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

type Binding :: Environment -> Type
data Binding scope = Binding
  { name :: !Variable,
    strict :: !Bool,
    -- | Whether the variable is yet to be evaluated even where it is given
    -- evidence. A definition that takes evidence is a function of it and
    -- nothing to evaluate, but a name for what a newtype wraps is whatever
    -- was wrapped, and that is evaluated to the function first.
    delayed :: !Bool,
    -- | The form of the variable that takes every argument at once, if it
    -- is a definition with one, and how many arguments that is: see
    -- @uncurried@ in "Generate.Go.Expression".
    worker :: !(Maybe (Variable, Int))
  }

binding :: Variable -> Binding scope
binding name = Binding {name, strict = False, delayed = False, worker = Nothing}

-- |
-- A definition, which may have a form that takes every argument at once.
defined :: Variable -> Maybe (Variable, Int) -> Binding scope
defined name worker = Binding {name, strict = False, delayed = False, worker}

instance Shift0.Functor Binding where
  map = Shift.mapDefault

instance Shift.Functor Binding where
  map _ Binding {name, strict, delayed, worker} = Binding {name, strict, delayed, worker}
