{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for declarations
module Syntax.Tree.Declaration where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Text as Text (pack)
import qualified Data.Strict.Vector1 as Strict (Vector1, fromNonEmpty)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.FreeVariables (TermBindingVariables (..))
import qualified Syntax.Lexer as Lexer (variableIdentifier)
import Syntax.Parser
  ( Parser,
    asum,
    betweenFieldPragma,
    betweenParens,
    betweenUnorderedPragma,
    many,
    position,
    sepBy1Comma,
    sepByComma,
    token,
    try,
    (<**>),
  )
import Syntax.Position (Position)
import Syntax.Tree.Alias (Alias)
import Syntax.Tree.Brand (Brand)
import qualified Syntax.Tree.Brand as Brand
import Syntax.Tree.ClassDeclaration (ClassDeclaration)
import qualified Syntax.Tree.ClassDeclaration as ClassDeclaration
import {-# SOURCE #-} Syntax.Tree.ClassDeclarations (ClassDeclarations)
import {-# SOURCE #-} qualified Syntax.Tree.ClassDeclarations as ClassDeclarations
import Syntax.Tree.Constraint (Constraint)
import qualified Syntax.Tree.Constraint as Constraint
import Syntax.Tree.Constraints (Constraints)
import qualified Syntax.Tree.Constraints as Constraints
import Syntax.Tree.Data (Data)
import qualified Syntax.Tree.Data as Data
import Syntax.Tree.Fixity (Fixity)
import Syntax.Tree.Import (Import)
import qualified Syntax.Tree.Import as Import
import Syntax.Tree.ImportSymbols (Symbols)
import {-# SOURCE #-} Syntax.Tree.InstanceDeclarations (InstanceDeclarations)
import {-# SOURCE #-} qualified Syntax.Tree.InstanceDeclarations as InstanceDeclarations
import Syntax.Tree.InstanceHead (InstanceHead)
import qualified Syntax.Tree.InstanceHead as InstanceHead
import Syntax.Tree.LeftHandSide (LeftHandSide)
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Marked as Marked
import Syntax.Tree.Qualification (Qualification)
import Syntax.Tree.RightHandSide (RightHandSide)
import Syntax.Tree.Scheme (Scheme)
import Syntax.Tree.Type (Type)
import qualified Syntax.Tree.Type as Type
import Syntax.Tree.TypePattern (TypePattern)
import qualified Syntax.Tree.TypePattern as TypePattern
import Syntax.Variable
  ( ConstructorIdentifier,
    FullQualifiers,
    Name (..),
    QualifiedConstructorIdentifier,
    Variable,
  )
import qualified Syntax.Variable as Variable

data Declaration position
  = -- |
    -- > type T = A
    Synonym
      { startPosition :: !position,
        typePosition :: !position,
        typeName :: !ConstructorIdentifier,
        parameters :: !(Strict.Vector (TypePattern Position)),
        typeDefinition :: !(Type position)
      }
  | -- |
    -- > data T
    Data
      { startPosition :: !position,
        brand :: !Brand,
        typePosition :: !position,
        typeName :: !ConstructorIdentifier,
        parameters :: !(Strict.Vector (TypePattern Position)),
        -- | How many of the parameters are written. The rest stand for what
        -- the kind that is said for the type takes: the two parameters of
        --
        -- > data T :: Type -> Type -> Type where
        named :: !Int,
        dataDefinition :: !(Data position)
      }
  | -- |
    -- > class C
    Class
      { startPosition :: !position,
        superClasses :: !(Strict.Vector (Constraint position)),
        typePosition :: !position,
        typeName :: !ConstructorIdentifier,
        parameter :: !(TypePattern position),
        classDefinition :: !(ClassDeclarations position)
      }
  | -- |
    -- > instance C T
    Instance
      { startPosition :: !position,
        prerequisites :: !(Constraints position),
        classPosition :: !position,
        className :: !QualifiedConstructorIdentifier,
        instanceHead :: !InstanceHead,
        instanceDefinition :: !(InstanceDeclarations position)
      }
  | -- |
    -- > x :: a
    Annotation
      { termNames :: !(Strict.Vector1 (Marked Variable position)),
        annotation :: !(Scheme position)
      }
  | -- |
    -- > type T :: A
    KindAnnotation
      { typeNames :: !(Strict.Vector1 (Marked ConstructorIdentifier position)),
        kindAnnotation :: !(Type position)
      }
  | -- |
    -- > infix `x`
    Infix
      { fixity :: !Fixity,
        termNames' :: !(Strict.Vector1 (Marked Name position))
      }
  | -- |
    -- > x = e
    Definition
      { startPosition :: !position,
        leftHandSide :: !(LeftHandSide position),
        rightHandSide :: !(RightHandSide position)
      }
  | -- |
    -- > import M
    Import
      { qualification :: !Qualification,
        targetPosition :: !position,
        target :: !FullQualifiers,
        alias :: !Alias,
        symbols :: !Symbols
      }
  | -- |
    -- > import {-# BUILTIN #-} M
    Builtin
      { targetPosition :: !position,
        target :: !FullQualifiers
      }
  | -- |
    -- > {-# UNORDEREDRECORDS A #-}
    Unordered
      { constructorNames :: !(Strict.Vector1 (Marked Variable.Constructor position))
      }
  | -- |
    -- > {-# CONSTRUCTORFIELDS A #-}
    Fields
      { constructorNames :: !(Strict.Vector1 (Marked Variable.Constructor position))
      }
  deriving (Show)

instance TermBindingVariables Declaration where
  termBindingVariables = \case
    Class {classDefinition} -> termBindingVariables classDefinition
    Data {dataDefinition} -> termBindingVariables dataDefinition
    Annotation {termNames} -> toList termNames
    Infix {termNames'} -> [position :@ name | position :@ Variable name <- toList termNames']
    Definition {leftHandSide} -> termBindingVariables leftHandSide
    _ -> []

typeBindingVariables :: Declaration Position -> [(ConstructorIdentifier, Position)]
typeBindingVariables = \case
  Class {typePosition, typeName} -> [(typeName, typePosition)]
  Data {typePosition, typeName} -> [(typeName, typePosition)]
  Synonym {typePosition, typeName} -> [(typeName, typePosition)]
  _ -> []

fromClass :: ClassDeclaration position -> Declaration position
fromClass = \case
  ClassDeclaration.Annotation
    { termNames,
      annotation
    } -> Annotation {termNames, annotation}
  ClassDeclaration.Definition
    { startPosition,
      leftHandSide,
      rightHandSide
    } -> Definition {startPosition, leftHandSide, rightHandSide}
  ClassDeclaration.Infix
    { fixity,
      termNames'
    } -> Infix {fixity, termNames'}

fromImport :: Import position -> Declaration position
fromImport = \case
  Import.Import
    { qualification,
      targetPosition,
      target,
      alias,
      symbols
    } ->
      Import
        { qualification,
          targetPosition,
          target,
          alias,
          symbols
        }
  Import.Builtin {targetPosition, target} -> Builtin {targetPosition, target}

toImport :: Declaration position -> Maybe (Import position)
toImport = \case
  Import
    { qualification,
      targetPosition,
      target,
      alias,
      symbols
    } ->
      Just
        Import.Import
          { qualification,
            targetPosition,
            target,
            alias,
            symbols
          }
  Builtin {targetPosition, target} -> Just Import.Builtin {targetPosition, target}
  _ -> Nothing

-- |
-- The instances that a declaration asks for with a @deriving@ clause, each
-- as the declaration that asks for it by itself:
--
-- > data T a = A a deriving (Eq)
--
-- is as if it were followed by
--
-- > deriving instance (Eq a) => Eq (T a)
--
-- The instance is written here as requiring the class of every parameter
-- of the type, and is marked as coming from a clause. Where it is checked,
-- the class is only required of the parameters it can be asked of: not of
-- one that stands for a type that takes arguments, or for something that
-- is not a type of values at all, such as a stage or a scope. It is still
-- required of a parameter that is the type of nothing the value holds,
-- which Haskell would not require.
--
-- A type whose kind is said where it is declared asks as well for that to
-- be the kind it is checked against, as if it were said on a line of its
-- own. This is only so when the type has no parameters that are written:
-- the kind that is said is then the whole kind.
derived :: Declaration Position -> [Declaration Position]
derived = \case
  Data {startPosition, typeName, parameters, named, dataDefinition} -> kinded ++ instances
    where
      kinded = case dataDefinition of
        Data.GADT {kind = Strict.Just kindAnnotation}
          | named == 0 ->
              [ KindAnnotation
                  { typeNames = Strict.fromNonEmpty ((startPosition :@ typeName) :| []),
                    kindAnnotation
                  }
              ]
        _ -> []
      instances = do
        className <- toList (Data.derivingx dataDefinition)
        let requires pattern =
              Constraint.Constraint
                { startPosition,
                  classVariable = startPosition :@ className,
                  typeVariable = startPosition :@ TypePattern.name pattern,
                  arguments = Strict.Vector.empty
                }
            -- What the instance is, and whether it is of the type without
            -- its last parameter, is found once the class is known (see
            -- 'Semantic.Resolve.Temporary.Complete.DataInstance').
            instanceDefinition = InstanceDeclarations._derivingClause
        pure
          Instance
            { startPosition,
              prerequisites =
                if null parameters
                  then Constraints.None
                  else Constraints.Constraints (fmap requires parameters),
              classPosition = startPosition,
              className,
              instanceHead =
                InstanceHead.Head
                  { startPosition,
                    typeName = Variable.Local Variable.:=. typeName,
                    parameters
                  },
              instanceDefinition
            }
  _ -> []

parse :: Parser (Declaration Position)
parse =
  asum
    [ datax
        <$> position
        <*> Brand.parse
        <*> position
        <*> Variable.parse
        <*> (Strict.Vector.fromList <$> many TypePattern.parse)
        <*> Data.parse,
      kindAnnotation
        <$> try (token "type" *> (Strict.fromNonEmpty <$> sepBy1Comma Marked.parse) <* token "::")
        <*> Type.parse,
      synonym
        <$> position
        <*> (token "type" *> position)
        <*> Variable.parse
        <*> (Strict.Vector.fromList <$> many TypePattern.parse)
        <*> (token "=" *> Type.parse),
      position <**> (token "class" *> parseClass),
      token "instance" *> parseInstance True,
      token "deriving" *> token "instance" *> parseInstance False,
      fromClass <$> ClassDeclaration.parse,
      fromImport <$> Import.parse,
      unordered <$> betweenUnorderedPragma (Strict.fromNonEmpty <$> sepBy1Comma Marked.parseLiteral),
      fields <$> betweenFieldPragma (Strict.fromNonEmpty <$> sepBy1Comma Marked.parseLiteral)
    ]
  where
    unordered constructorNames =
      Unordered
        { constructorNames
        }
    fields constructorNames =
      Fields
        { constructorNames
        }
    kindAnnotation typeNames kindAnnotation =
      KindAnnotation
        { typeNames,
          kindAnnotation
        }
    synonym startPosition typePosition typeName parameters typeDefinition =
      Synonym
        { startPosition,
          typePosition,
          typeName,
          parameters,
          typeDefinition
        }
    datax startPosition brand typePosition typeName parameters dataDefinition =
      Data
        { startPosition,
          brand,
          typePosition,
          typeName,
          parameters = parameters <> Strict.Vector.fromList (unnamed <$> [1 .. taken]),
          named = length parameters,
          dataDefinition
        }
      where
        -- What the kind that is said for the type takes.
        taken :: Int
        taken = case dataDefinition of
          Data.GADT {kind = Strict.Just kind} -> arrows kind
          _ -> 0
        arrows :: Type Position -> Int
        arrows = \case
          Type.Function {result} -> 1 + arrows result
          _ -> 0
        -- A parameter nobody wrote. No constructor can mean it: the
        -- constructors of such a type have variables of their own.
        unnamed :: Int -> TypePattern Position
        unnamed index =
          TypePattern.TypePattern
            { position = typePosition,
              name = Lexer.variableIdentifier (Text.pack ("t" ++ show index))
            }
    parseClass :: Parser (Position -> Declaration Position)
    parseClass =
      asum
        [ classx
            <$> try (betweenParens (Strict.Vector.fromList <$> sepByComma Constraint.parse) <* token "=>")
            <*> position
            <*> Variable.parse
            <*> TypePattern.parse
            <*> ClassDeclarations.parse,
          classx
            <$> try (singleConstraint <$> Constraint.parse <* token "=>")
            <*> position
            <*> Variable.parse
            <*> TypePattern.parse
            <*> ClassDeclarations.parse,
          classx Strict.Vector.empty
            <$> position
            <*> Variable.parse
            <*> TypePattern.parse
            <*> ClassDeclarations.parse
        ]
      where
        singleConstraint = Strict.Vector.singleton
        classx superClasses typePosition typeName parameter classDefinition startPosition =
          Class
            { startPosition,
              superClasses,
              typePosition,
              typeName,
              parameter,
              classDefinition
            }

    parseInstance :: Bool -> Parser (Declaration Position)
    parseInstance manual =
      instancex
        <$> position
        <*> Constraints.parse
        <*> position
        <*> Variable.parse
        <*> InstanceHead.parse
        <*> if manual
          then InstanceDeclarations.parse
          else pure InstanceDeclarations._derivingInstance
      where
        instancex startPosition prerequisites classPosition className instanceHead instanceDefinition =
          Instance
            { startPosition,
              prerequisites,
              classPosition,
              className,
              instanceHead,
              instanceDefinition
            }
