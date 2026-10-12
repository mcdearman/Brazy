module Semantic.Check.ConstructorInstance where

import Control.Monad.ST (ST)
import Core.Tree.Type ((-#>))
import Data.Foldable (traverse_)
import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Check.EntryInstance (EntryInstance, entry)
import qualified Semantic.Check.EntryInstance as EntryInstance
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo (..))
import Semantic.Tree.Constructor (Syntax)
import qualified Semantic.Unify as Unify
import Syntax.Tree.Brand (Brand)
import qualified Syntax.Tree.Brand as Brand
import qualified Syntax.Variable as Variable

-- |
-- What a constructor is wanted for. A constructor of a type declared with
-- @where@ has type variables of its own, and they are not the same thing in
-- the two uses: to make a value they may be any types, and where a value is
-- taken apart they are whatever types the value was made with.
data Use
  = Build
  | Match

data ConstructorInstance s scope = ConstructorInstance
  { brand :: !Brand,
    name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (EntryInstance s scope)),
    -- | The arguments of the type it makes a value of. For most this is
    -- what the type was given. A constructor of a type declared with
    -- @where@ has its own.
    result :: !(Strict.Vector (Unify.Type s scope)),
    -- | Whether it is one of those, so that matching on it may tell
    -- something of the type that is matched.
    indexed :: !Bool,
    -- | For each argument of the type it makes, whether matching may tell
    -- something there. It may not where the constructor gives the type a
    -- variable of its own that it gives nowhere before: the variable is then
    -- whatever the type that is matched has there.
    refined :: !(Strict.Vector Bool),
    -- | The types it hides, where it is matched on: those of its own
    -- variables that the type it makes does not settle.
    hidden :: ![Unify.Type s scope]
  }

info :: ConstructorInstance s scope -> ConstructorInfo s scope
info ConstructorInstance {entries, brand} = case brand of
  Brand.Newtype -> Newtype
  Brand.Boxed -> ConstructorInfo {entries = EntryInstance.info <$> entries}

types :: ConstructorInstance s scope -> Strict.Vector (Unify.Type s scope)
types ConstructorInstance {entries} = entry <$> entries

function :: ConstructorInstance s scope -> Unify.Type s scope -> Unify.Type s scope
function ConstructorInstance {entries} base = foldr ((-#>) . entry) base entries

mark :: Context s scope -> ConstructorInstance s scope -> ST s ()
mark context ConstructorInstance {entries} =
  traverse_ (EntryInstance.mark context) entries
