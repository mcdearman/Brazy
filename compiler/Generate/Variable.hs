-- |
-- A name in the generated code: a local one, or a global one.
module Generate.Variable where

import Data.Text (Text)
import Generate.Global (Global)

data Variable
  = Local !Text
  | Global !Global
