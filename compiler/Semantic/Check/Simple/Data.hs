-- |
-- Make a fresh instance of a data type from its Core declaration, to make
-- values of it or to take them apart, with the levity of everything its
-- constructors hold marked as needed when the program runs.
module Semantic.Check.Simple.Data where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import Core.Tree.Data (Data (..), DefinitionF (..))
import Semantic.Check.Instanciate.Constructor (Use (..))
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Check.Instanciate.Data (DataInstance (DataInstance))
import qualified Semantic.Check.Instanciate.Data as DataInstance
import qualified Semantic.Check.Simple.Constructor as Constructor
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

instanciate :: Use -> Context s scope -> Position -> Data scope -> ST s (DataInstance s scope)
instanciate use _ position Data {parameters, definition = Definition {constructors, selectors, brand}} = do
  types <- traverse (Unify.fresh . logicalType) parameters
  constructors <- traverse (Constructor.instanciate use position brand types) constructors
  let datax =
        DataInstance
          { position,
            types,
            selectors,
            constructors
          }
  pure datax

-- |
-- A type as it is where a value of it is taken apart.
instanciateApart :: Context s scope -> Position -> Data scope -> ST s (DataInstance s scope)
instanciateApart context position datax = do
  datax <- instanciate Match context position datax
  DataInstance.mark context datax
  pure datax

instanciateMark :: Context s scope -> Position -> Data scope -> ST s (DataInstance s scope)
instanciateMark context position datax = do
  datax <- instanciate Build context position datax
  DataInstance.mark context datax
  pure datax
