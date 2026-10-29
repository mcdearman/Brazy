{-# LANGUAGE_HAZY UnorderedRecords #-}
module Semantic.Tree.CallHead where

import qualified Core.Tree.Evidence as Simple (Evidence)
import qualified Core.Tree.Evidence as Evidence (EvidenceF (Variable, instanciation, variable))
import qualified Core.Tree.Instanciation as Instanciation (InstanciationF (Instanciation, Mono, levity))
import qualified Core.Tree.Instanciation as Simple (Instanciation)
import qualified Core.Tree.Type as Core.Type (strict)
import qualified Data.Vector.Strict as Strict.Vector (fromList)
import qualified Semantic.Index.Evidence as Evidence (Index (Index))
import Data.Kind (Type)
import Semantic.Check.Info.Constructor (ConstructorInfo)
import Semantic.Check.Info.Method (MethodInfo)
import Semantic.Check.Info.Selector (SelectorInfo)
import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term as Term
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Stage)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Syntax.Position (Position)

type CallHead :: Layout -> Stage -> Environment -> Type
data CallHead layout stage scope
  = Variable
      { variablePosition :: !Position,
        variable :: !(Term.Index scope),
        instanciation :: !(Inferred Simple.Instanciation stage scope)
      }
  | Constructor
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Selector
      { selectorPosition :: !Position,
        selector :: !(Selector.Index scope),
        selectorInfo :: !(Inferred SelectorInfo stage scope)
      }
  | Method
      { methodPosition :: !Position,
        method :: !(Method.Index scope),
        evidence :: !(Inferred Simple.Evidence stage scope),
        instanciation :: !(Inferred Simple.Instanciation stage scope),
        methodInfo :: !(Inferred MethodInfo stage scope)
      }
  deriving (Show)

instance Shift0.Functor (CallHead layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (CallHead layout stage) where
  map category = \case
    Variable {variablePosition, variable, instanciation} ->
      Variable
        { variablePosition,
          variable = Shift.map category variable,
          instanciation = given category variable instanciation
        }
    Constructor {constructorPosition, constructor, constructorInfo} ->
      Constructor
        { constructorPosition,
          constructor = Shift.map category constructor,
          constructorInfo = Shift.map category constructorInfo
        }
    Selector {selectorPosition, selector, selectorInfo} ->
      Selector
        { selectorPosition,
          selector = Shift.map category selector,
          selectorInfo = Shift.map category selectorInfo
        }
    Method {methodPosition, method, evidence, instanciation, methodInfo} ->
      Method
        { methodPosition,
          method = Shift.map category method,
          evidence = Shift.map category evidence,
          instanciation = Shift.map category instanciation,
          methodInfo = Shift.map category methodInfo
        }

-- |
-- What a variable is given, where it is now. A use of a definition by one
-- of the group it is defined with is given what the group was: see
-- 'Shift.grouped'.
given ::
  Shift.Category scope scope' ->
  Term.Index scope ->
  Inferred Simple.Instanciation stage scope ->
  Inferred Simple.Instanciation stage scope'
given category variable = \case
  Solved Instanciation.Mono {}
    | Just evidence <- Shift.grouped category variable ->
        Solved $
          Instanciation.Instanciation $
            Strict.Vector.fromList
              [ Evidence.Variable {variable = Evidence.Index assumed, instanciation = Instanciation.Mono {levity = Core.Type.strict}}
              | assumed <- evidence
              ]
  instanciation -> Shift.map category instanciation

instance FreeTermVariables CallHead where
  freeTermVariables target = \case
    Variable {variable} -> FreeVariables.term target variable
    Constructor {} -> []
    Selector {} -> []
    Method {} -> []

instance Connect CallHead where
  connect = layout
  seperate = layout

layout :: CallHead layout stage scope -> CallHead layout' stage scope
layout = \case
  Variable {variablePosition, variable, instanciation} ->
    Variable {variablePosition, variable, instanciation}
  Constructor {constructorPosition, constructor, constructorInfo} ->
    Constructor {constructorPosition, constructor, constructorInfo}
  Selector {selectorPosition, selector, selectorInfo} ->
    Selector {selectorPosition, selector, selectorInfo}
  Method {methodPosition, method, evidence, instanciation, methodInfo} ->
    Method {methodPosition, method, evidence, instanciation, methodInfo}
