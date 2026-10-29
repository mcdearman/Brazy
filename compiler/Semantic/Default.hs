-- |
-- What a type that is never worked out may be taken to be: one of the
-- types a module's @default@ declaration lists, in its order, or else
-- @Integer@ and then @Double@, as the report has it. Only the builtin types
-- of numbers can be listed.
module Semantic.Default
  ( Default (..),
    standard,
    typeOf,
  )
where

import qualified Semantic.Index.Type2 as Type2

data Default
  = Integer
  | Int
  | Double
  | Float
  deriving (Eq, Show)

-- | The report's: @default (Integer, Double)@.
standard :: [Default]
standard = [Integer, Double]

typeOf :: Default -> Type2.Index scope
typeOf = \case
  Integer -> Type2.Integer
  Int -> Type2.Int
  Double -> Type2.Double
  Float -> Type2.Float
