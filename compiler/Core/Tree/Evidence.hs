-- |
-- Evidence in Core that a class holds of a type, which is passed for a
-- constraint: evidence in scope, a superclass of other evidence, evidence
-- a constructor holds, or a function of evidence and its application. The
-- type checker builds it with logical variables; finished evidence has none.
module Core.Tree.Evidence (Evidence, EvidenceF (..)) where

import qualified Core.Evidence.Functor as Core
import Core.Substitute (Category (..), Evidences (..))
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Instanciation (InstanciationF)
import {-# SOURCE #-} qualified Core.Tree.Instanciation as Instanciation
import qualified Core.Tree.Type as Type
import qualified Data.Vector as Vector
import Data.Void (Void)
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Term as Term
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import Semantic.Shift0 (shift)
import qualified Semantic.Shift0 as Shift0

type Evidence = EvidenceF Void Void

data EvidenceF logicalEvidence logicalType scope
  = Logical !logicalEvidence
  | Variable
      { variable :: !(Evidence.Index scope),
        instanciation :: !(InstanciationF logicalEvidence logicalType scope)
      }
  | Super
      { base :: !(EvidenceF logicalEvidence logicalType scope),
        index :: !Int
      }
  | -- | Evidence that a value holds, and that a match on the value's
    -- constructor names: a constructor that requires something of its types
    -- keeps the evidence for it with what it holds.
    Held
      { term :: !(Term.Index scope)
      }
  | -- | Evidence for something that holds given every type: a function of
    -- the evidence that is required of the type, which is so many things.
    -- The body has that evidence as what its innermost scope assumes.
    Abstract
      { count :: !Int,
        body :: !(EvidenceF logicalEvidence logicalType (Scope.Local 'Scope.:+ scope))
      }
  | -- | Such evidence, given what it requires.
    Applied
      { function :: !(EvidenceF logicalEvidence logicalType scope),
        arguments :: ![EvidenceF logicalEvidence logicalType scope]
      }
  deriving (Show)

instance
  (Show logicalEvidence, Show logicalType) =>
  Scope.Show (EvidenceF logicalEvidence logicalType)
  where
  showsPrec = showsPrec

instance Shift0.Functor (EvidenceF logicalEvidence logicalType) where
  map = Shift.mapDefault

instance Shift.Functor (EvidenceF logicalEvidence logicalType) where
  map category = \case
    Logical logical -> Logical logical
    Variable {variable, instanciation} ->
      Variable
        { variable = Shift.map category variable,
          instanciation = Shift.map category instanciation
        }
    Super {base, index} ->
      Super
        { base = Shift.map category base,
          index
        }
    Held {term} -> Held {term = Shift.map category term}
    Abstract {count, body} -> Abstract {count, body = Shift.map (Shift.Over category) body}
    Applied {function, arguments} ->
      Applied {function = Shift.map category function, arguments = Shift.map category <$> arguments}

instance
  (logicalEvidence ~ Void, logicalType ~ Void) =>
  Substitute.Functor (EvidenceF logicalEvidence logicalType)
  where
  map category = Substitute.mapEvidence category

instance Substitute.EvidenceFunctor EvidenceF where
  -- todo: Find a better way of doing this. Pattern matching on
  -- `Type.Constructor Type2.Strict` will probably cause problems later
  mapEvidence
    (Substitute _ _ (Evidences replacements))
    Variable
      { variable = Evidence.Index (Evidence0.Assumed index),
        instanciation = Instanciation.Mono {levity = Type.Constructor Type2.Strict}
      } = replacements Vector.! index
  mapEvidence
    (Substitute.Over category)
    Variable
      { variable = Evidence.Index (Evidence0.Shift index),
        instanciation = Instanciation.Mono {levity = Type.Constructor Type2.Strict}
      } =
      shift $
        Substitute.mapEvidence
          category
          Variable
            { variable = Evidence.Index index,
              instanciation = Instanciation.Mono {levity = Type.Constructor Type2.Strict}
            }
  mapEvidence category evidence = case evidence of
    Variable {variable, instanciation} ->
      Variable
        { variable = Shift.map general variable,
          instanciation = Substitute.mapEvidence (Lift general) instanciation
        }
      where
        general = Substitute.general category
    Super {base, index} ->
      Super
        { base = Substitute.mapEvidence category base,
          index
        }
    Held {term} -> Held {term = Shift.map (Substitute.general category) term}
    Abstract {count, body} -> Abstract {count, body = Substitute.mapEvidence (Substitute.Over category) body}
    Applied {function, arguments} ->
      Applied
        { function = Substitute.mapEvidence category function,
          arguments = Substitute.mapEvidence category <$> arguments
        }

instance Core.Functor EvidenceF where
  map f g = \case
    Logical logical -> Logical (f logical)
    Variable {variable, instanciation} ->
      Variable
        { variable,
          instanciation = Core.map f g instanciation
        }
    Super {base, index} ->
      Super
        { base = Core.map f g base,
          index
        }
    Held {term} -> Held {term}
    Abstract {count, body} -> Abstract {count, body = Core.map f g body}
    Applied {function, arguments} -> Applied {function = Core.map f g function, arguments = Core.map f g <$> arguments}

{-# UnorderedRecords Variable, Super, Abstract, Applied #-}
