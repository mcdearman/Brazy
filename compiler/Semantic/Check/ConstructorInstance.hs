module Semantic.Check.ConstructorInstance where

import Control.Monad.ST (ST)
import Core.Tree.Type ((-#>))
import Core.Tree.Type (TypeF (Constructor))
import Data.Foldable (for_, traverse_)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector (fromList)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Check.EntryInstance (EntryInstance, entry)
import qualified Semantic.Check.EntryInstance as EntryInstance
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo (..))
import Semantic.Check.Temporary.EntryInfo (EntryInfo (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Tree.Constructor (Syntax)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
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
  { position :: !Position,
    brand :: !Brand,
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
    hidden :: ![Unify.Type s scope],
    -- | What it requires of its types: each a class, and the type it is
    -- required of.
    asked :: ![(Type2.Index scope, Unify.Type s scope)],
    -- | Pairs of types that it requires to be the same.
    equated :: ![(Unify.Type s scope, Unify.Type s scope)]
  }

info :: ConstructorInstance s scope -> ConstructorInfo s scope
info ConstructorInstance {position, entries, brand, asked} = case brand of
  Brand.Newtype -> Newtype
  -- After everything else, a value holds the evidence for what its
  -- constructor requires, unevaluated as anything else it holds may be.
  Brand.Boxed -> ConstructorInfo {entries = fmap EntryInstance.info entries <> evidence}
    where
      evidence = Strict.Vector.fromList [EntryInfo {position, strict = Constructor Type2.Lazy} | _ <- asked]

-- |
-- What says how a constructor is laid out, where it is used to make a
-- value. What the constructor requires of its types is asked for here, and
-- the evidence is what the value will hold.
built :: Context s scope -> ConstructorInstance s scope -> ST s (ConstructorInfo s scope)
built context instancex@ConstructorInstance {position, asked, equated} = do
  for_ equated $ \(left, right) -> Unify.unify context position left right
  case info instancex of
    ConstructorInfo {entries} | not (null asked) -> do
      held <- for asked $ \(classx, subject) -> do
        evidence <- Unify.constrain context position classx subject
        pure (position, evidence)
      pure Holding {entries, held = Strict.Vector.fromList held}
    plain -> pure plain

types :: ConstructorInstance s scope -> Strict.Vector (Unify.Type s scope)
types ConstructorInstance {entries} = entry <$> entries

function :: ConstructorInstance s scope -> Unify.Type s scope -> Unify.Type s scope
function ConstructorInstance {entries} base = foldr ((-#>) . entry) base entries

mark :: Context s scope -> ConstructorInstance s scope -> ST s ()
mark context ConstructorInstance {entries} =
  traverse_ (EntryInstance.mark context) entries
