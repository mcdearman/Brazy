-- |
-- The index a term variable is resolved to: which scope it is bound in,
-- counted outward by 'Shift', and where in that scope. It is a declaration
-- of a local block, a variable a pattern binds (a 'Bound' path into the
-- pattern), a member of a group of declarations checked together, a
-- top-level declaration (by module and place), or a variable of one of the
-- simpler scopes Core makes as it simplifies patterns.
module Semantic.Index.Term where

import Semantic.Scope
  ( Declaration,
    Environment (..),
    Global,
    GroupTerm,
    Pattern,
    SimpleDeclaration,
    SimplePattern,
  )
import Prelude hiding (Functor, map)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Pattern :: !Bound -> Index (Pattern ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)
  Global :: !Int -> !Int -> Index Global
  Group :: !Int -> Index (GroupTerm ':+ scope)
  SimplePattern :: !Int -> Index (SimplePattern ':+ scopes)
  SimpleDeclaration :: Index (SimpleDeclaration ':+ scopes)

instance Eq (Index scope) where
  Declaration local1 == Declaration local2 = local1 == local2
  Pattern bound1 == Pattern bound2 = bound1 == bound2
  Shift index1 == Shift index2 = index1 == index2
  Global global1 local1 == Global global2 local2 = global1 == global2 && local1 == local2
  _ == _ = False

instance Show (Index scope) where
  showsPrec d = \case
    Declaration local -> showParen (d > 10) $ showString "Declaration " . showsPrec 11 local
    Pattern bound -> showParen (d > 10) $ showString "Pattern " . showsPrec 11 bound
    Shift index -> showParen (d > 10) $ showString "Shift " . showsPrec 11 index
    Group index -> showParen (d > 10) $ showString "Group " . showsPrec 11 index
    Global global local ->
      showParen (d > 10) $
        showString "Global "
          . showsPrec 11 global
          . showString " "
          . showsPrec 11 local
    SimplePattern local -> showParen (d > 10) $ showString "SimplePattern " . showsPrec 11 local
    SimpleDeclaration -> showString "SimpleDeclaration"

data Bound
  = At
  | Select !Int !Bound
  deriving (Show, Eq)
