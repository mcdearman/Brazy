-- |
-- Turns the index of the variable or constructor at the head of a call
-- into a 'CallHead': a variable, a field selector, a class method or a
-- constructor. @runST@ cannot be one; "Semantic.Resolve.Go.Expression"
-- handles a call of it apart.
module Semantic.Resolve.Go.CallHead where

import Error (badRunSTCall)
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Term2 as Term2
import Semantic.Stage (Resolve)
import Semantic.Tree.CallHead (CallHead (..))
import Semantic.Tree.Combinators.Inferred (Inferred (Inferred))
import Syntax.Position (Position)

resolveVariable :: Position -> Term2.Index scope -> CallHead layout Resolve scope
resolveVariable variablePosition@selectorPosition@methodPosition = \case
  Term2.Index variable ->
    Variable
      { variablePosition,
        variable,
        instanciation = Inferred
      }
  Term2.Select selector ->
    Selector
      { selectorPosition,
        selector,
        selectorInfo = Inferred
      }
  Term2.Method method ->
    Method
      { methodPosition,
        method,
        evidence = Inferred,
        instanciation = Inferred,
        methodInfo = Inferred
      }
  Term2.RunST -> badRunSTCall variablePosition

resolveConstructor :: Position -> Constructor.Index scope -> CallHead layout Resolve scope
resolveConstructor constructorPosition constructor =
  Constructor
    { constructorPosition,
      constructor,
      constructorInfo = Inferred
    }
