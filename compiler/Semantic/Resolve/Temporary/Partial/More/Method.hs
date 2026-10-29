{-# LANGUAGE_HAZY UnorderedRecords #-}
module Semantic.Resolve.Temporary.Partial.More.Method where

data Method = Method
  { typeIndex :: !Int,
    methodIndex :: !Int
  }
