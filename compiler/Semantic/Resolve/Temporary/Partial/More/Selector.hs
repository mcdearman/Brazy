{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A record selector, as one piece of a term declaration: the type it is
-- of, and its place among that type's selectors.
module Semantic.Resolve.Temporary.Partial.More.Selector where

data Selector = Selector
  { typeIndex :: !Int,
    selectorIndex :: !Int
  }
