{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- The name of a top-level definition or instance in the generated code:
-- the module it is in and its mangled name.
module Generate.Global where

import Data.Text (Text)
import Syntax.Variable (FullQualifiers)

data Global = Global
  { path :: !FullQualifiers,
    name :: !Text
  }
  deriving (Eq, Ord, Show)
