module Semantic.Check.Temporary.Entry where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Core
import qualified Data.Vector.Strict as Strict.Vector (empty)
import Error (Position)
import Semantic.Check.Context (Context (..))
import Semantic.Check.Temporary.Scheme (Scheme)
import qualified Semantic.Check.Temporary.Scheme as Scheme
import Semantic.Check.Temporary.StrictnessAnnotation (StrictnessAnnotation)
import qualified Semantic.Check.Temporary.StrictnessAnnotation as StrictnessAnnotation
import Semantic.Check.Temporary.Type (Type)
import qualified Semantic.Check.Temporary.Type as Type
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Constraints as Semantic (Constraints (Lazy))
import qualified Semantic.Tree.Entry as Semantic (Entry (..), Restricted (..))
import qualified Semantic.Tree.Entry as Solved
import qualified Semantic.Tree.Scheme as Semantic (Scheme (..))
import qualified Semantic.Unify as Unify

-- |
-- The type of what a constructor holds: a type, or a type with a @forall@
-- of its own.
data Held s scope
  = Plain !(Type s scope)
  | Quantified !(Scheme s scope)

data Entry s scope = Entry
  { startPosition :: !Position,
    entry :: !(Held s scope),
    strict :: !(StrictnessAnnotation s scope)
  }

check :: Context s scope -> Semantic.Entry Position Resolve scope -> ST s (Entry s scope)
check context Semantic.Entry {startPosition, entry = Semantic.Canonical scheme, strict} = do
  entry <- case scheme of
    Semantic.Scheme {parameters, constraints = Semantic.Lazy, result}
      | null parameters -> do
          let plain = Shift.map (Shift.Unshift $ error "a type variable that nothing binds") result
          Plain <$> Type.check context Core.typex plain
    _ -> Quantified <$> Scheme.check context scheme
  strict <- StrictnessAnnotation.check context strict
  pure
    Entry
      { startPosition,
        entry,
        strict
      }

solve :: Context s scope -> Entry s scope -> Unify.Solve s (Solved.Entry Position Check scope)
solve context Entry {startPosition, entry, strict} = do
  entry <- case entry of
    Plain typex -> do
      typex <- Type.solve context typex
      pure
        Semantic.Scheme
          { startPosition,
            implicit = True,
            parameters = Strict.Vector.empty,
            constraints = Semantic.Lazy,
            result = shift typex
          }
    Quantified scheme -> Scheme.solve context scheme
  strict <- StrictnessAnnotation.solve context strict
  pure
    Solved.Entry
      { startPosition,
        entry = Semantic.Restricted entry,
        strict
      }
