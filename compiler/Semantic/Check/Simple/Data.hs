module Semantic.Check.Simple.Data where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import Core.Tree.Data (Data (..))
import Semantic.Check.ConstructorInstance (Use (..))
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Check.DataInstance (DataInstance (DataInstance))
import qualified Semantic.Check.DataInstance as DataInstance
import qualified Semantic.Check.Simple.Constructor as Constructor
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

instanciate :: Use -> Context s scope -> Position -> Data scope -> ST s (DataInstance s scope)
instanciate use _ position Data {parameters, constructors, selectors, brand} = do
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

instanciateMark :: Context s scope -> Position -> Data scope -> ST s (DataInstance s scope)
instanciateMark context position datax = do
  datax <- instanciate Build context position datax
  DataInstance.mark context datax
  pure datax
