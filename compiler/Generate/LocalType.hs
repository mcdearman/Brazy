{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- What the instances that go with a type of a local scope are called, for
-- "Generate.Context" when Core is translated into the sequent IR.
module Generate.LocalType where

import Data.Map (Map)
import Data.Text (Text)
import qualified Semantic.Index.Type2 as Type2

data LocalType scope = LocalType
  { classInstances :: !(Map (Type2.Index scope) Text),
    dataInstances :: !(Map (Type2.Index scope) Text)
  }
