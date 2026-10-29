-- |
-- Whether the names under one qualifier would stay the same if an imported
-- module exported more, as the @StableImports@ extension requires. With it
-- on, an import of a whole module other than the @Prelude@, or of all of
-- one but what it hides, is unstable, and may not share its qualifier with
-- any other import.
module Semantic.Resolve.Stability where

import Error (unstableImports)
import Syntax.Position (Position)

data Stability
  = Ignore
  | Stable [Position]
  | Unstable Position
  deriving (Show)

instance Semigroup Stability where
  Ignore <> stability = stability
  stability <> Ignore = stability
  Stable positions <> Stable positions' = Stable (positions ++ positions')
  Stable positions <> Unstable position = unstableImports (positions ++ [position])
  Unstable position <> Stable positions = unstableImports (position : positions)
  Unstable position <> Unstable position' = unstableImports [position, position']

instance Monoid Stability where
  mempty = Ignore
