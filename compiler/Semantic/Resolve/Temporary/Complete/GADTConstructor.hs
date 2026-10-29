{-# LANGUAGE_HAZY UnorderedRecords #-}
module Semantic.Resolve.Temporary.Complete.GADTConstructor where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (duplicateFieldEntries)
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Scheme as Scheme
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Constructor as Real (Syntax (..))
import qualified Semantic.Tree.GADTConstructor as Real (GADTConstructor (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.GADTConstructor as Syntax
import Syntax.Tree.Marked (Marked ((:@)))
import qualified Syntax.Tree.Scheme as Syntax.Scheme
import qualified Syntax.Tree.Type as Syntax.Type
import Syntax.Variable (Constructor, Variable)

-- |
-- Something a constructor holds that has a name.
data Field = Field
  { at :: !Position,
    field :: !Variable,
    strict :: !Bool
  }

data GADTConstructor scope = GADTConstructor
  { position :: !Position,
    name :: !Constructor,
    -- | What it holds, in order, if it is written as a record.
    fields :: !(Strict.Vector Field),
    -- | For each selector of the type, where in this constructor it
    -- selects from, if anywhere.
    selections :: Strict.Vector (Strict.Maybe Int),
    constructor :: Real.GADTConstructor Resolve scope
  }

shrink :: GADTConstructor scope -> Real.GADTConstructor Resolve scope
shrink = constructor

resolve :: Context scope -> Strict.Vector Variable -> Syntax.GADTConstructor Position -> [GADTConstructor scope]
resolve context selectorNames Syntax.GADTConstructor {names, scheme, fields = named} = do
  position :@ name <- toList names
  pure
    GADTConstructor
      { position,
        name,
        fields,
        selections,
        constructor =
          Real.GADTConstructor
            { position,
              name,
              syntax =
                if null fields
                  then Real.Standard
                  else Real.Record (fmap field fields),
              typex = Scheme.resolve context scheme
            }
      }
  where
    fields = Strict.Vector.fromList (zipWith held (toList named) (stricts (Syntax.Scheme.result scheme)))
    held (at :@ field) strict = Field {at, field, strict}
    -- Whether each thing the type says is held is held strictly.
    stricts = \case
      Syntax.Type.Function {result} -> False : stricts result
      Syntax.Type.StrictFunction {result} -> True : stricts result
      _ -> []
    indexes = Map.fromListWithKey twice (zip (field <$> toList fields) [0 ..])
    twice name _ _ = duplicateFieldEntries [at | Field {at, field} <- toList fields, field == name]
    selections = fmap selector selectorNames
      where
        selector name = maybe Strict.Nothing Strict.Just (indexes Map.!? name)
