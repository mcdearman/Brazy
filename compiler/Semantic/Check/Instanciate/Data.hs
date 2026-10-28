module Semantic.Check.Instanciate.Data where

import Control.Monad.ST (ST)
import Core.Tree.Type ((#), (-#>))
import qualified Core.Tree.Type as Core
import Data.Foldable (toList, traverse_)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Order (orderListInt')
import Semantic.Check.Instanciate.Constructor (ConstructorInstance (..))
import qualified Semantic.Check.Instanciate.Constructor as Constructor (info)
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Check.Instanciate.Entry (EntryInstance (..))
import Semantic.Check.Temporary.SelectorInfo (Select (..), SelectorInfo (..))
import Semantic.Check.Temporary.UpdateInfo (UpdateInfo (..))
import qualified Semantic.Check.Temporary.UpdateInfo as UpdateInfo
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Selector as Selector (Index (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Tree.Selector (Selector (..))
import qualified Semantic.Tree.Selector as Selector (Uniform (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data DataInstance s scope = DataInstance
  { position :: !Position,
    types :: !(Strict.Vector (Unify.Type s scope)),
    constructors :: !(Strict.Vector (ConstructorInstance s scope)),
    selectors :: !(Strict.Vector Selector)
  }

baseType :: DataInstance s scope -> Type2.Index scope -> Unify.Type s scope
baseType DataInstance {types} typeIndex = foldl (#) (Core.constructor typeIndex) types

constructorFunction :: DataInstance s scope -> Constructor.Index scope -> Unify.Type s scope
constructorFunction DataInstance {constructors} Constructor.Index {typeIndex, constructorIndex} =
  ConstructorInstance.function constructor (constructed constructor typeIndex)
  where
    constructor = constructors Strict.Vector.! constructorIndex

-- |
-- What the type is given.
given :: DataInstance s scope -> Strict.Vector (Unify.Type s scope)
given DataInstance {types} = types

-- |
-- The type a constructor makes a value of.
constructed :: ConstructorInstance s scope -> Type2.Index scope -> Unify.Type s scope
constructed ConstructorInstance {result} typeIndex = foldl (#) (Core.constructor typeIndex) result

selectorFunction :: DataInstance s scope -> Selector.Index scope -> Unify.Type s scope
selectorFunction instancex (Selector.Index typeIndex selectorIndex) =
  selected instancex typeIndex selectorIndex -#> selectorType instancex selectorIndex

-- |
-- The type of what a selector selects from. It is what the first
-- constructor that holds the thing makes: for a constructor that says what
-- type it makes, that type is what the type of what it holds is said in
-- terms of.
selected :: DataInstance s scope -> Type2.Index scope -> Int -> Unify.Type s scope
selected DataInstance {constructors, selectors} typeIndex selectorIndex =
  constructed (constructors Strict.Vector.! first) typeIndex
  where
    Selector {first} = selectors Strict.Vector.! selectorIndex

selectorType :: DataInstance s scope -> Int -> Unify.Type s scope
selectorType instancex selectorIndex = entry
  where
    EntryInstance {entry} = selectorEntry instancex selectorIndex

-- |
-- What a selector selects, as its first constructor holds it.
selectorEntry :: DataInstance s scope -> Int -> EntryInstance s scope
selectorEntry DataInstance {constructors, selectors} selectorIndex = entries Strict.Vector.! index
  where
    Selector {first, index} = selectors Strict.Vector.! selectorIndex
    ConstructorInstance {entries} = constructors Strict.Vector.! first

selectorInfo :: DataInstance s scope -> Int -> SelectorInfo s scope
selectorInfo DataInstance {position, constructors, selectors} index = case selectors Strict.Vector.! index of
  Selector {first, index, uniform = Selector.Uniform} -> Uniform {position, strict}
    where
      ConstructorInstance {entries} = constructors Strict.Vector.! first
      EntryInstance {strict} = entries Strict.Vector.! index
  Selector {uniform = Selector.Disjoint {indexes}} ->
    Disjoint
      { select = Strict.Vector.imap pick indexes
      }
    where
      pick index selectIndex =
        Select
          { selectIndex,
            constructorInfo = Constructor.info (constructors Strict.Vector.! index)
          }

updateInfo :: DataInstance s scope -> UpdateInfo s scope
updateInfo DataInstance {constructors, selectors} =
  UpdateInfo
    { updateInfo = Strict.Vector.imap update constructors
    }
  where
    update :: Int -> ConstructorInstance s scope -> UpdateInfo.Update s scope
    update constructorIndex constructorInstance@ConstructorInstance {entries, asked} =
      UpdateInfo.Update
        { constructorInfo = ConstructorInstance.info constructorInstance,
          selectorIndexes =
            -- Where each selector's field is in this constructor, with which
            -- selector it is. A field need not be in the same place in every
            -- constructor that has it.
            let selections = do
                  (selector, Selector {index, uniform}) <- zip [0 :: Int ..] (toList selectors)
                  case uniform of
                    Selector.Uniform -> [(index, selector)]
                    Selector.Disjoint {indexes} ->
                      case indexes Strict.Vector.! constructorIndex of
                        Strict.Nothing -> []
                        Strict.Just index -> [(index, selector)]
                combine = \case
                  [index] -> Strict.Just index
                  [] -> Strict.Nothing
                  _ -> error "duplicate selector"
             in -- The evidence a value holds is kept as it is.
                orderListInt' combine (length entries + length asked) selections
        }

mark :: Context s scope -> DataInstance s scope -> ST s ()
mark context DataInstance {constructors} =
  traverse_ (ConstructorInstance.mark context) constructors
