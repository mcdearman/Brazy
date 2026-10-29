{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A cell of the cycle-checking Löb of "Graph.Topological" over a
-- 'Traversable': what to fail with if working it out needs the cell
-- itself, and how it works its value out from the others. It has a module
-- of its own so that its field names do not clash with @Formula@'s there.
module Graph.Topological1 where

import Control.Monad.ST (ST)

data Formula1 t s a = Formula1
  { cycle :: forall a. a,
    run :: t (ST s a) -> ST s a
  }
