{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A class method, as one piece of a term declaration: the class it is of,
-- and its place among that class's methods.
module Semantic.Resolve.Temporary.Partial.More.Method where

data Method = Method
  { typeIndex :: !Int,
    methodIndex :: !Int
  }
