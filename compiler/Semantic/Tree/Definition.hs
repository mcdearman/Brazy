module Semantic.Tree.Definition where

import Data.List.NonEmpty (NonEmpty (..))
import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Show as Term
import Semantic.Tree.Function (Function)

data Definition layout stage scope
  = Definition {definition :: !(Function layout stage scope)}
  | Alternative
      { definition :: !(Function layout stage scope),
        alternative :: !(Definition layout stage scope)
      }
  deriving (Show)

instance Term.Show Definition where
  showsPrec = showsPrec

instance Scope.Show (Definition layout stage) where
  showsPrec = showsPrec

instance Shift0.TermFunctor Definition where
  mapTerm = Shift0.map

instance Shift0.Functor (Definition layout stage) where
  map = Shift.mapDefault

instance Shift.TermFunctor Definition where
  mapTerm = Shift.map

instance Shift.Functor (Definition layout stage) where
  map category = \case
    Definition function -> Definition (Shift.map category function)
    Alternative {definition, alternative} ->
      Alternative
        { definition = Shift.map category definition,
          alternative = Shift.map category alternative
        }

instance FreeTermVariables Definition where
  freeTermVariables target = \case
    Definition function -> freeTermVariables target function
    Alternative {definition, alternative} ->
      concat
        [ freeTermVariables target definition,
          freeTermVariables target alternative
        ]

instance Connect Definition where
  connect = \case
    Definition function -> Definition (connect function)
    Alternative {definition, alternative} -> Alternative {definition = connect definition, alternative = connect alternative}
  seperate = \case
    Definition function -> Definition (seperate function)
    Alternative {definition, alternative} -> Alternative {definition = seperate definition, alternative = seperate alternative}

merge :: NonEmpty (Function layout stage scope) -> Definition layout stage scope
merge (definition :| []) = Definition definition
merge (definition1 :| (definition2 : definitions)) =
  Alternative {definition = definition1, alternative = merge (definition2 :| definitions)}

{-# UnorderedRecords Alternative #-}
