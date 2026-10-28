{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for gadt constructors
module Syntax.Tree.GADTConstructor where

import Data.Foldable (toList)
import qualified Data.Strict.Vector1 as Strict (Vector1, fromNonEmpty)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeaturePolymorphicComponents)
import Syntax.FreeVariables (TermBindingVariables (..))
import Syntax.Parser
  ( Parser,
    asum,
    betweenBraces,
    many,
    optional,
    position,
    sepBy1Comma,
    sepByComma,
    token,
  )
import Syntax.Position (Position)
import Syntax.Tree.Constraints (Constraints)
import qualified Syntax.Tree.Constraints as Constraints
import qualified Syntax.Tree.Entry as Entry
import Syntax.Tree.Field (Field (Field))
import qualified Syntax.Tree.Field as Field
import Syntax.Tree.Marked (Marked)
import qualified Syntax.Tree.Marked as Marked
import Syntax.Tree.Scheme (Scheme)
import qualified Syntax.Tree.Scheme as Scheme
import Syntax.Tree.Type (Type)
import qualified Syntax.Tree.Type as Type
import Syntax.Tree.TypePattern (TypePattern)
import qualified Syntax.Tree.TypePattern as TypePattern
import Syntax.Variable (Constructor, Variable)

data GADTConstructor position
  = -- |
    -- > data T where { C :: T }
    -- >                ^^^^^^
    GADTConstructor
    { names :: !(Strict.Vector1 (Marked Constructor Position)),
      -- | The whole type of the constructor. One that is written as a
      -- record,
      --
      -- > C :: { x :: a, y :: !b } -> T a b
      --
      -- has here the type it would have written without the names.
      scheme :: !(Scheme Position),
      -- | The names of what it holds, in order, if it is written as a
      -- record.
      fields :: !(Strict.Vector (Marked Variable position))
    }
  deriving (Show)

instance TermBindingVariables GADTConstructor where
  termBindingVariables GADTConstructor {fields} = toList fields

parse :: Parser (GADTConstructor Position)
parse =
  gadtConstructor . Strict.fromNonEmpty
    <$> sepBy1Comma Marked.parseLiteral
    <*> (token "::" *> position)
    <*> optional forallx
    <*> Constraints.parse
    <*> asum
      [ Left <$> record,
        Right <$> Type.parse
      ]
  where
    forallx = token "forall" *> (Strict.Vector.fromList <$> many TypePattern.parse) <* token "."
    record =
      (,)
        <$> betweenBraces (sepByComma Field.parse)
        <*> (token "->" *> Type.parse)
    gadtConstructor ::
      Strict.Vector1 (Marked Constructor Position) ->
      Position ->
      Maybe (Strict.Vector (TypePattern Position)) ->
      Constraints Position ->
      Either ([Field Position], Type Position) (Type Position) ->
      GADTConstructor Position
    gadtConstructor names startPosition parameters constraints body =
      GADTConstructor
        { names,
          scheme = case parameters of
            Just parameters -> Scheme.Explicit {startPosition, parameters, constraints, result}
            Nothing -> Scheme.Implicit {startPosition, constraints, result},
          fields = Strict.Vector.fromList fields
        }
      where
        (fields, result) = case body of
          Right result -> ([], result)
          Left (declared, made) ->
            let held = [(name, entry) | Field {names, entry} <- declared, name <- toList names]
             in (fst <$> held, foldr (holding . snd) made held)
    -- The type of a constructor that holds one thing more.
    holding :: Entry.Entry -> Type Position -> Type Position
    holding entry result = case entry of
      Entry.Lazy {startPosition, entry} ->
        Type.Function
          { startPosition,
            parameter = plain startPosition entry,
            operatorPosition = startPosition,
            result
          }
      Entry.Strict {startPosition, entry} ->
        Type.StrictFunction
          { startPosition,
            parameter = plain startPosition entry,
            operatorPosition = startPosition,
            result
          }
      Entry.Polymorphic {startPosition} -> unsupportedFeaturePolymorphicComponents startPosition
    plain :: Position -> Scheme Position -> Type Position
    plain startPosition = \case
      Scheme.Implicit {constraints = Constraints.Value, result} -> result
      _ -> unsupportedFeaturePolymorphicComponents startPosition
