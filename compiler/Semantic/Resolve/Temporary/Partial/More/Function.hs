{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A clause of a definition, as one piece of a term declaration, resolved
-- twice: as it is, and with the type variables of the term's signature in
-- scope. Which of them is kept depends on the signature.
module Semantic.Resolve.Temporary.Partial.More.Function where

import Semantic.Layout (Normal)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Stage (Resolve)
import Semantic.Tree.Function as Tree (Function)

data Function scope = Function
  { functionAuto :: Tree.Function Normal Resolve scope,
    functionManual :: Tree.Function Normal Resolve (Local ':+ scope)
  }
