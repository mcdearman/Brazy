-- |
-- What a type that is never worked out may be taken to be when its module
-- has no @default@ declaration: @Integer@ and then @Double@, as the report
-- has it. A module that has one lists the types itself.
module Semantic.Default (standard) where

import qualified Semantic.Index.Type2 as Type2

-- | The report's: @default (Integer, Double)@.
standard :: [Type2.Index scope]
standard = [Type2.Integer, Type2.Double]
