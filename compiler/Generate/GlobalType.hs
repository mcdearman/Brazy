{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What is known of a top-level type when Core is translated into the
-- sequent IR: the global names of the instances that go with it, and which
-- fields of its constructors are strict.
module Generate.GlobalType where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import Generate.Global (Global)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Scope as Scope

data GlobalType = GlobalType
  { classInstances :: !(Map (Type2.Index Scope.Global) Global),
    dataInstances :: !(Map (Type2.Index Scope.Global) Global),
    -- | For each constructor, if it is a data type, which of what it holds
    -- it holds strictly.
    strictness :: !(Vector [Bool])
  }

indexClass :: GlobalType -> Type2.Index Scope.Global -> Global
GlobalType {classInstances} `indexClass` index = classInstances Map.! index

indexData :: GlobalType -> Type2.Index Scope.Global -> Global
GlobalType {dataInstances} `indexData` index = dataInstances Map.! index
