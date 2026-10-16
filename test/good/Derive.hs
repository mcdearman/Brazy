module Derive where

-- Freddy's example: what is derived does not depend on what the module has
-- in scope. The Prelude is here only qualified.

import Prelude ()
import qualified Prelude

data MyType a = MyType a
  deriving (Prelude.Read, Prelude.Functor)
