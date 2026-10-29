-- |
-- Something for each top-level term declaration of every module, looked up
-- by a global 'Term.Link'. Strongly connected components are found over it
-- to put the top-level declarations in groups of ones that depend on each
-- other.
module Semantic.Group.Functor.Term.ModuleSet where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Semantic.Group.Functor.Term.Declarations (Declarations)
import qualified Semantic.Group.Functor.Term.Declarations as Declarations
import qualified Semantic.Index.Link.Term as Proper.Term
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Locality as Locality

newtype ModuleSet a = ModuleSet (Vector (Declarations a))

instance Functor ModuleSet where
  fmap = fmapDefault

instance Foldable ModuleSet where
  foldMap = foldMapDefault

instance Traversable ModuleSet where
  traverse f (ModuleSet set) = ModuleSet <$> traverse (traverse f) set

(!) :: ModuleSet a -> Term.Link Locality.Global -> a
ModuleSet vector ! index = case index of
  Proper.Term.Global global local ->
    Declarations.terms (vector Vector.! global) Vector.! local
