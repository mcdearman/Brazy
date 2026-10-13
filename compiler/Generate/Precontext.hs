module Generate.Precontext where

import Data.Vector (Vector)
import Generate.Global (Global)
import Generate.GlobalType (GlobalType)

data Precontext = Precontext
  { terms :: !(Vector (Vector Global)),
    -- | For each definition, how many arguments its form that takes every
    -- argument at once takes, if it has one.
    workers :: !(Vector (Vector (Maybe Int))),
    types :: !(Vector (Vector GlobalType))
  }
