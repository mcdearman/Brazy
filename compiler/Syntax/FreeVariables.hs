-- |
-- Classes for the variables a syntax tree mentions: the type variables
-- free in it, and the term variables a declaration binds.
module Syntax.FreeVariables where

import Syntax.Tree.Marked (Marked)
import Syntax.Variable (Variable, VariableIdentifier)

class FreeTypeVariables typex where
  freeTypeVariables :: typex position -> [Marked VariableIdentifier position]

class TermBindingVariables declaration where
  termBindingVariables :: declaration position -> [Marked Variable position]
