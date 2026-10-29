-- |
-- Scopes as a type-level list. An 'Environment' is the scopes a name can
-- be seen from, innermost first, joined by ':+' and ending in 'Global',
-- the top level of every module. A 'Scope' is the kind of one: a block of
-- declarations, the variables of a pattern, type variables bound together
-- ('Local'), a group of declarations checked together, or one of the
-- simpler scopes Core makes of patterns. Indexes and trees are typed by
-- the environment they are in; the classes here show and compare such
-- types.
module Semantic.Scope
  ( Environment ((:+)),
    Scope,
    Local,
    Declaration,
    Pattern,
    GroupTerm,
    GroupType,
    SimplePattern,
    SimpleDeclaration,
    Global,
    Show (..),
    Eq (..),
    shows,
    Vacuous,
    Singleton (..),
  )
where

import Data.Kind (Constraint, Type)
import Prelude hiding (Eq (..), Show, shows, showsPrec)

data Environment
  = Scope :+ Environment
  | Global

type Global = 'Global

infixr 5 :+

data Scope
  = Local
  | Declaration
  | Pattern
  | GroupTerm
  | GroupType
  | SimplePattern
  | SimpleDeclaration

type Local = 'Local

type Declaration = 'Declaration

type Pattern = 'Pattern

type GroupTerm = 'GroupTerm

type GroupType = 'GroupType

type SimplePattern = 'SimplePattern

type SimpleDeclaration = 'SimpleDeclaration

type Show :: (Environment -> Type) -> Constraint
class Show typex where
  showsPrec :: Int -> typex scope -> ShowS

shows :: (Show typex) => typex scope -> ShowS
shows = showsPrec 0

type Eq :: (Environment -> Type) -> Constraint
class Eq typex where
  (==) :: typex scope -> typex scope -> Bool
  infix 4 ==

type Vacuous :: Environment -> Type
data Vacuous scope

instance Show Vacuous where
  showsPrec _ = \case {}

instance Eq Vacuous where
  (==) = \case {}

type Singleton :: Environment -> Type
data Singleton scope = Singleton

instance Show Singleton where
  showsPrec _ Singleton = showString "Singleton"

instance Eq Singleton where
  Singleton == Singleton = True
