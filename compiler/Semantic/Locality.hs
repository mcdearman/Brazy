-- |
-- Whether declarations are at the top level of a module ('Global') or in a
-- local block ('Local'), as a type-level tag.
module Semantic.Locality where

data Locality
  = Global
  | Local

type Global = 'Global

type Local = 'Local
