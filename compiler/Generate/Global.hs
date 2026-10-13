module Generate.Global where

import Data.Text (Text, pack)
import Syntax.Variable (FullQualifiers)

data Global = Global
  { path :: !FullQualifiers,
    name :: !Text
  }
  deriving (Eq, Ord)

-- |
-- What the form of a definition that takes every argument at once is
-- called. No name that is written in a program has a space in it.
direct :: Global -> Global
direct global@Global {name} = global {name = name <> pack " direct"}
