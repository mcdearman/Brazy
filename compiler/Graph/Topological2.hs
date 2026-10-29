{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A cell of the cycle-checking Löb of "Graph.Topological" over a
-- 'Bitraversable' of cells of two types: what to fail with if working it
-- out needs the cell itself, and how it works its value out from the
-- others. It has a module of its own so that its field names do not clash
-- with @Formula@'s there.
module Graph.Topological2 where

import Control.Monad.ST (ST)

data Formula2 t s a b z = Formula2
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) -> ST s z
  }
