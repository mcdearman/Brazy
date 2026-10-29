{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Something for each type declaration of a module or of a local block,
-- made with 'indexes' and looked up by a local 'Type.Link'. Strongly
-- connected components are found over it to put the declarations in groups
-- of ones that depend on each other.
-- "Semantic.Group.Functor.Type.ModuleSet" is the same for every module.
module Semantic.Group.Functor.Type.Declarations where

import Data.Functor.Identity (Identity)
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)
import qualified Semantic.Index.Link.Type as Type
import Semantic.Layout (Normal)
import Semantic.Locality (Local)
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Declarations as Proper

newtype Declarations a = Declarations
  { types :: Vector a
  }

instance Functor Declarations where
  fmap = fmapDefault

instance Foldable Declarations where
  foldMap = foldMapDefault

instance Traversable Declarations where
  traverse f Declarations {types} = declarations <$> traverse f types
    where
      declarations types = Declarations {types}

indexes ::
  (Int -> Type.Link locality) ->
  Proper.Declarations Identity Void locality' Identity Normal Resolve scope ->
  Declarations (Type.Link locality)
indexes index Proper.Declarations {types} =
  Declarations
    { types = Vector.generate (length types) index
    }

(!) :: Declarations a -> Type.Link Local -> a
Declarations {types} ! Type.Declaration index = types Vector.! index
