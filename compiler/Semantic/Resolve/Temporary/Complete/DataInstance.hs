{-# LANGUAGE_HAZY UnorderedRecords #-}
module Semantic.Resolve.Temporary.Complete.DataInstance where

import Data.Functor.Identity (Identity (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Error (illegalInstanceClass)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import Semantic.Layout (Normal)
import qualified Semantic.Resolve.Binding.Type as Type
import qualified Semantic.Index.Method as Method (Index (..))
import qualified Semantic.Index.Term2 as Term2 (Index (..))
import qualified Semantic.Resolve.Binding.Term as Term
import qualified Semantic.Resolve.Bindings as Bindings (BindingsF (..))
import Semantic.Resolve.Context (Context, core, (!=.))
import qualified Semantic.Resolve.Core as Core (CoreF (..))
import qualified Semantic.Resolve.Go.Instance as Real (resolve)
import qualified Semantic.Stage as Semantic
import qualified Semantic.Tree.Instance as Real (Instance)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Position (Position)
import qualified Syntax.Tree.Constraint as Constraint
import qualified Syntax.Tree.Constraints as Constraints
import qualified Syntax.Tree.InstanceDeclarations as InstanceDeclarations
import qualified Syntax.Tree.TypePattern as TypePattern
import qualified Syntax.Tree.Declaration as Syntax
import Syntax.Tree.InstanceHead (InstanceHead (Head))
import qualified Syntax.Tree.InstanceHead as InstanceHead
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (ConstructorIdentifier, QualifiedConstructorIdentifier ((:=.)), Qualifiers (Local))

data DataInstance scope = DataInstance
  { classPosition :: !Position,
    dataIndex :: !Int,
    classIndex :: !(Type2.Index scope),
    instancex :: Real.Instance Identity Identity Normal Semantic.Resolve scope
  }

shrink :: DataInstance scope -> Real.Instance Identity Identity Normal Semantic.Resolve scope
shrink = instancex

prepare :: DataInstance scope -> (Int, Map (Type2.Index scope) (NonEmpty.NonEmpty (DataInstance scope)))
prepare instancex@DataInstance {dataIndex, classIndex} =
  (dataIndex, Map.singleton classIndex (NonEmpty.singleton instancex))

newtype Resolve = Resolve
  { typeIndexes :: Map ConstructorIdentifier Int
  }

resolve ::
  Context scope ->
  (ConstructorIdentifier -> Maybe Int) ->
  Syntax.Declaration Position ->
  [DataInstance scope]
resolve context lookup = \case
  Syntax.Instance
    { startPosition,
      prerequisites = prerequisites',
      classPosition,
      className,
      instanceHead = Head {typeName = Local :=. datax, parameters = written},
      instanceDefinition
    }
      | Just dataIndex <- lookup datax,
        valid ->
          let Type.Header {fields} Type.:@ Identity Type.Binding {index, methods} =
                context !=. classPosition :@ className
              classIndex = Type3.toType2 (illegalInstanceClass classPosition) index
              (parameters, prerequisites) = shortened classIndex instanceDefinition written prerequisites'
              -- A method can be given a definition if it can be seen: it
              -- came with the class, or it is in scope by its own name,
              -- with any qualifiers or with none.
              visible name = Set.member name fields || any (named name) (locals : Map.elems globals)
              Core.Core {globals, locals} = core context
              named name bindings = case Map.lookup name (Bindings.terms bindings) of
                Just (_ Term.:@ Identity Term.Binding {index = Term2.Method Method.Index {typeIndex}}) ->
                  typeIndex == classIndex
                _ -> False
              entry =
                DataInstance
                  { classPosition,
                    dataIndex,
                    classIndex,
                    instancex =
                      Real.resolve
                        context
                        startPosition
                        prerequisites
                        parameters
                        methods
                        visible
                        instanceDefinition
                  }
           in [entry]
      where
        valid
          | Local :=. classx <- className,
            Just _ <- lookup classx =
              False
          | otherwise = True
  _ -> []

-- |
-- The parameters and prerequisites of an instance that a clause asks for,
-- once its class is known. One of a class of types that take an argument
-- is of the type without its last parameter, and requires nothing of it.
shortened ::
  Type2.Index scope ->
  InstanceDeclarations.InstanceDeclarations Position ->
  Strict.Vector (TypePattern.TypePattern Position) ->
  Constraints.Constraints Position ->
  (Strict.Vector (TypePattern.TypePattern Position), Constraints.Constraints Position)
shortened classIndex declarations parameters prerequisites
  | InstanceDeclarations.DerivingClause <- declarations,
    classIndex `elem` [Type2.Functor, Type2.Foldable, Type2.Traversable],
    not (null parameters) =
      let lastName = TypePattern.name (Strict.Vector.last parameters)
          other = \case
            Constraint.Constraint {typeVariable = _ :@ name} -> name /= lastName
            Constraint.Equality {} -> True
       in ( Strict.Vector.init parameters,
            case prerequisites of
              Constraints.Constraints wanted
                | kept <- Strict.Vector.filter other wanted,
                  not (null kept) ->
                    Constraints.Constraints kept
              _ -> Constraints.Lazy
          )
  | otherwise = (parameters, prerequisites)
