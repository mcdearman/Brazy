{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Resolves an instance, whether written out or derived, given the class's
-- methods: its type parameters, its prerequisites, and each method's
-- definition, put in the class's order. A method left out, and every
-- method of a derived instance, is marked to be generated. Defining a name
-- that is not one of the class's visible methods is an error.
module Semantic.Resolve.Go.Instance where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (notClassMethod, patternInMethod)
import Order (orderListInt')
import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Constraints as Constraints
import qualified Semantic.Resolve.Go.Scheme as Scheme
import qualified Semantic.Resolve.Go.TypePattern as TypePattern
import qualified Semantic.Resolve.Temporary.Complete.Definition as Complete (Definition (..), resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Definition as Definition (merge)
import Semantic.Tree.Instance (Instance (..))
import Semantic.Tree.InstanceDefinition (InstanceDefinition (..))
import Semantic.Tree.InstanceDefinition2 (Annotation (..), Header (..), InstanceDefinition2 (..))
import Semantic.Tree.MethodConcrete (MethodConcrete (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Constraints as Syntax (Constraints)
import qualified Syntax.Tree.InstanceDeclaration as Syntax (InstanceDeclaration (..))
import qualified Syntax.Tree.InstanceDeclarations as Syntax (InstanceDeclarations (..))
import qualified Syntax.Tree.TypePattern as Syntax (TypePattern)
import Syntax.Variable (Variable)

resolve ::
  Context scope ->
  Position ->
  Syntax.Constraints Position ->
  Strict.Vector (Syntax.TypePattern Position) ->
  Map Variable Int ->
  (Variable -> Bool) ->
  Syntax.InstanceDeclarations Position ->
  Instance Identity Identity Normal Resolve scope
resolve
  context
  startPosition
  prerequisites
  parameters
  memberMethods
  visible
  declarations
    | parameters <- TypePattern.resolve <$> parameters,
      prerequisites <- Constraints.resolve context (Scheme.augmentWith parameters context) prerequisites =
        let members declarations = orderListInt' combine (length memberMethods) members
              where
                combine (member : members) = Definition $ Resolve $ Definition.merge (member :| members)
                combine [] = Generated Inferred
                members = map member $ toList declarations
                member = \case
                  Syntax.Definition {startPosition, leftHandSide, rightHandSide} ->
                    case Complete.resolve
                      (patternInMethod startPosition)
                      (Scheme.augmentWith parameters context)
                      leftHandSide
                      rightHandSide of
                      Complete.Definition _ name function
                        -- A method that the class's module did not export
                        -- cannot be given a definition, any more than it
                        -- can be used.
                        | Just index <- Map.lookup name memberMethods,
                          visible name ->
                            (index, function)
                        | otherwise -> notClassMethod startPosition
            header =
              Identity
                Header
                  { parameters,
                    prerequisites,
                    equated = Strict.Vector.empty,
                    needed = Inferred,
                    lenient = case declarations of
                      Syntax.DerivingClause -> True
                      _ -> False
                  }
            definition = case declarations of
              Syntax.InstanceDeclarations {declarations} ->
                Standard header
                  ::: (Identity . Identity)
                    InstanceDefinition
                      { evidence = Inferred,
                        members = members (toList declarations)
                      }
              Syntax.DerivingInstance -> derived
              Syntax.DerivingClause -> derived
            derived =
              DerivedInstance header
                ::: (Identity . Identity)
                  InstanceDefinition
                    { evidence = Inferred,
                      members = Strict.Vector.replicate (length memberMethods) $ Generated Inferred
                    }
         in Instance
              { startPosition,
                clause = case declarations of
                  Syntax.DerivingClause -> True
                  _ -> False,
                definition,
                prerequisites = Identity Inferred
              }
