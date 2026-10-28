-- |
-- @Functor@, @Foldable@ and @Traversable@, derived. Each goes through what
-- each constructor holds, and finds where the type's last parameter is in
-- it: not at all, the whole of it, or the last argument of a type that does
-- not otherwise mention it, which is gone through by that type's own
-- instance. What is done to a field is a function of its own, of the
-- function the method is given and then of the field.
module Semantic.Check.Derive.Functor
  ( inner,
    fmap,
    foldr,
    traverse,
  )
where

import {-# SOURCE #-} qualified Builtin.Applicative as Applicative
import {-# SOURCE #-} qualified Builtin.Foldable as Foldable
import {-# SOURCE #-} qualified Builtin.Functor as Functor
import {-# SOURCE #-} qualified Builtin.Traversable as Traversable
import Control.Monad.ST (ST)
import Core.Temporary.Definition (Definition (..))
import qualified Core.Temporary.Definition as Definition
import Core.Temporary.Function (Function (..))
import Core.Temporary.Pattern (Pattern (..))
import qualified Core.Temporary.Pattern as Pattern
import Core.Temporary.RightHandSide (RightHandSide (..))
import Core.Tree.Class (Class)
import qualified Core.Tree.Class as Class
import qualified Core.Tree.Constructor as Core.Constructor
import Core.Tree.Data (Data)
import qualified Core.Tree.Data as Core.Data
import qualified Core.Tree.Entry as Core.Entry
import Core.Tree.Evidence (Evidence, EvidenceF (..))
import Core.Tree.Expression (Expression)
import qualified Core.Tree.Expression as Expression
import Core.Tree.Forall (ForallOver (..))
import Core.Tree.Instanciation (Instanciation, InstanciationF (..))
import Core.Tree.Statements (Statements (Bottom))
import qualified Core.Tree.Type as Core
import Core.Tree.TypeLambda (TypeLambdaOver (..))
import Data.Foldable (toList)
import Data.List.Reverse (List (Nil))
import Data.Text.Lazy.Builder (fromString)
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Error (cannotDerive)
import Semantic.Check.ConstructorInstance (ConstructorInstance (..))
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.DataInstance (DataInstance (..))
import Semantic.Check.EntryInstance (EntryInstance (..))
import Semantic.Check.Simple.ConstructorInfo (ConstructorInfo)
import Semantic.Check.Simple.Data (instanciateApart)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Check.Temporary.Pattern as Apart (within)
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Evidence as Index.Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.MethodConcrete (Auto, MethodConcrete (Generated))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Prelude hiding (fmap, foldr, traverse)
import qualified Prelude

-- |
-- Where the last parameter is in what a constructor holds.
data Shape evidence
  = -- | It is not there.
    Fixed
  | -- | It is the whole of it.
    Whole
  | -- | It is the last argument of a type, whose instance it is gone
    -- through by: the evidence for that.
    Inside evidence (Shape evidence)

each :: (Applicative f) => (a -> f b) -> Shape a -> f (Shape b)
each f = \case
  Fixed -> pure Fixed
  Whole -> pure Whole
  Inside evidence rest -> Inside <$> f evidence <*> each f rest

shifted :: Shape (Evidence scope) -> Shape (Evidence (scope' ':+ scope))
shifted = \case
  Fixed -> Fixed
  Whole -> Whole
  Inside evidence rest -> Inside (shift evidence) (shifted rest)

-- |
-- Where a parameter is in a type that is in terms of the type's own
-- parameters, given by its place among them, with the type as it was
-- given; or why it cannot be gone through.
shape ::
  Position ->
  String ->
  Int ->
  Core.Type (Local ':+ scope) ->
  Unify.Type s scope' ->
  Shape (Unify.Type s scope')
shape position className parameter declared given
  | not (mentions parameter declared) = Fixed
  | Core.Variable (Local.Local index) <- declared, index == parameter = Whole
  | Core.Call function argument <- declared,
    Core.Call _ argument' <- given,
    Core.Call function' _ <- given,
    not (mentions parameter function) =
      Inside function' (shape position className parameter argument argument')
  | otherwise =
      cannotDerive position $
        fromString (className ++ ": the last parameter is in a type it cannot be reached in")

-- |
-- The types whose instances a type's instance goes through, and so
-- requires: the types the last parameter is the last argument of.
inner :: Int -> Core.Type (Local ':+ scope) -> [Core.Type (Local ':+ scope)]
inner parameter declared
  | Core.Call function argument <- declared,
    mentions parameter argument,
    not (mentions parameter function) =
      function : inner parameter argument
  | otherwise = []

mentions :: Int -> Core.Type (Local ':+ scope) -> Bool
mentions parameter = \case
  Core.Variable (Local.Local index) -> index == parameter
  Core.Call function argument -> mentions parameter function || mentions parameter argument
  Core.Function argument result -> mentions parameter argument || mentions parameter result
  Core.Type universe -> mentions parameter universe
  _ -> False

-- |
-- What each constructor holds, as the type has it in its own parameters.
declared :: Data scope -> [[Core.Type (Local ':+ scope)]]
declared Core.Data.Data {definition = Core.Data.Definition {constructors}} =
  [ [ Shift.map (Shift.Unshift $ error "a type variable that nothing binds") result
    | Core.Entry.Entry {entry = ForallOver {result}} <- toList entries
    ]
  | Core.Constructor.Constructor {entries} <- toList constructors
  ]

-- |
-- A constructor as what is derived sees it.
data Made scope = Made
  { constructor :: Constructor.Index scope,
    info :: ConstructorInfo scope,
    shapes :: [Shape (Evidence scope)]
  }

-- |
-- Each constructor, with where the last parameter is in what it holds and
-- the evidence that is needed to go through it, inside the instance.
made ::
  Context s (Local ':+ scope) ->
  Position ->
  String ->
  Type2.Index scope ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s [Made (Local ':+ scope)])
made context position className classx typeIndex datax = do
  datay@DataInstance {types, constructors} <- instanciateApart context position (shift datax)
  let parameter = length types - 1
      -- The instance's parameters are the type's, but for its last.
      unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence_ $ zipWith unify [0 ..] (take parameter (toList types))
  let generate index constructorx@ConstructorInstance {entries} fields = do
        if or [quantified | EntryInstance {quantified} <- toList entries]
          then cannotDerive position $ fromString (className ++ ": a constructor holds something with a forall of its own")
          else pure ()
        inside <- Apart.within context position datay (shift typeIndex) constructorx
        let held = [entry | EntryInstance {entry} <- toList entries]
            found = zipWith (shape position className parameter) fields held
        found <- Prelude.traverse (each (Unify.constrain inside position (shift classx))) found
        pure $ do
          shapes <- Prelude.traverse (each (Unify.solveEvidence position)) found
          info <- ConstructorInfo.solve $ ConstructorInstance.info constructorx
          pure
            Made
              { constructor = Constructor.Index {typeIndex = shift typeIndex, constructorIndex = index},
                info,
                shapes
              }
  generated <- sequence $ zipWith3 generate [0 ..] (toList constructors) (declared datax)
  pure $ sequence generated

-- |
-- A method of a class, at some evidence.
method :: Method.Index scope -> Class scope -> Evidence scope -> Instanciation scope -> Expression scope
method index classx evidence instanciation =
  Expression.Method
    { method = index,
      evidence,
      instanciation,
      methodInfo = Class.info classx
    }

-- |
-- What a constructor holds, matched with nothing more asked of it.
matching :: Made scope -> Pattern scope
matching Made {constructor, info, shapes} =
  Match
    { irrefutable = False,
      match =
        Pattern.Constructor
          { constructor,
            patterns = Strict.Vector.replicate (length shapes) Wildcard,
            constructorInfo = info
          }
    }

-- |
-- The method, of the definitions for each constructor, inside the method's
-- own type variables, and what is left when none matches.
generated ::
  ForallOver Core.TypeF Void (Local ':+ scope) ->
  [Definition (Local ':+ (Local ':+ scope))] ->
  Definition (Local ':+ (Local ':+ scope)) ->
  MethodConcrete Auto layout Check scope
generated ForallOver {parameters, constraints} definitions otherwise =
  Generated $
    Solved
      TypeLambdaOver
        { parameters,
          constraints,
          result = Definition.desugar $ Prelude.foldr (<>) otherwise definitions
        }

bottom :: Definition scope
bottom = Definition {definition = Plain {plain = Done {done = Expression.Join {statements = Bottom}}}}

-- |
-- > fmap f (C x1 ... xn) = C (m1 f x1) ... (mn f xn)
fmap ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ForallOver Core.TypeF Void (Local ':+ scope) ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
fmap context position typeIndex datax scheme = do
  constructors <- made context position "Functor" Type2.Functor typeIndex datax
  pure $ do
    constructors <- constructors
    pure $ generated scheme (map (definition . shift') constructors) bottom
  where
    shift' Made {constructor, info, shapes} =
      Made {constructor = shift constructor, info = shift info, shapes = map shifted shapes}
    definition constructorx@Made {constructor, info, shapes} =
      Definition
        { definition =
            Bound
              { patternx = Pattern.Wildcard,
                body =
                  Bound
                    { patternx = shift (matching constructorx),
                      body = Plain {plain = Done {done = built}}
                    }
              }
        }
      where
        built =
          Expression.simplifyConstructorExact
            (shift (shift constructor))
            (shift (shift info))
            ( Strict.Vector.fromList
                [ shift (shift (mapper fieldShape))
                    `Expression.call` shift Expression.patternVariable
                    `Expression.call` Expression.patternVariableAt' index
                | (index, fieldShape) <- zip [0 ..] shapes
                ]
            )
    -- \f x -> what f makes of x
    mapper :: Shape (Evidence scope') -> Expression scope'
    mapper = \case
      Fixed -> Expression.Lambda $ Expression.Lambda Expression.lambdaVariable
      Whole ->
        Expression.Lambda $
          Expression.Lambda $
            shift Expression.lambdaVariable `Expression.call` Expression.lambdaVariable
      Inside evidence rest ->
        Expression.Lambda $
          Expression.Lambda $
            method Method.fmap Functor.definition (shift (shift evidence)) Mono
              `Expression.call` (shift (shift (mapper rest)) `Expression.call` shift Expression.lambdaVariable)
              `Expression.call` Expression.lambdaVariable

-- |
-- > foldr f z (C x1 ... xn) = s1 f x1 (s2 f x2 (... (sn f xn z)))
foldr ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ForallOver Core.TypeF Void (Local ':+ scope) ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
foldr context position typeIndex datax scheme = do
  constructors <- made context position "Foldable" Type2.Foldable typeIndex datax
  pure $ do
    constructors <- constructors
    pure $ generated scheme (map (definition . shift') constructors) nothing
  where
    shift' Made {constructor, info, shapes} =
      Made {constructor = shift constructor, info = shift info, shapes = map shifted shapes}
    -- A type with no constructors has nothing to fold.
    nothing =
      Definition
        { definition =
            Bound
              { patternx = Pattern.Wildcard,
                body =
                  Bound
                    { patternx = Pattern.Wildcard,
                      body =
                        Bound
                          { patternx = Pattern.Wildcard,
                            body = Plain {plain = Done {done = shift Expression.patternVariable}}
                          }
                    }
              }
        }
    definition constructorx@Made {shapes} =
      Definition
        { definition =
            Bound
              { patternx = Pattern.Wildcard,
                body =
                  Bound
                    { patternx = Pattern.Wildcard,
                      body =
                        Bound
                          { patternx = shift (shift (matching constructorx)),
                            body = Plain {plain = Done {done = built}}
                          }
                    }
              }
        }
      where
        built =
          Prelude.foldr
            ( \(index, fieldShape) rest ->
                shift (shift (shift (step fieldShape)))
                  `Expression.call` shift (shift Expression.patternVariable)
                  `Expression.call` Expression.patternVariableAt' index
                  `Expression.call` rest
            )
            (shift Expression.patternVariable)
            (zip [0 ..] shapes)
    -- \f x rest -> rest, with x folded in front of it by f
    step :: Shape (Evidence scope') -> Expression scope'
    step = \case
      Fixed -> Expression.Lambda $ Expression.Lambda $ Expression.Lambda Expression.lambdaVariable
      Whole ->
        Expression.Lambda $
          Expression.Lambda $
            Expression.Lambda $
              shift (shift Expression.lambdaVariable)
                `Expression.call` shift Expression.lambdaVariable
                `Expression.call` Expression.lambdaVariable
      Inside evidence rest ->
        Expression.Lambda $
          Expression.Lambda $
            Expression.Lambda $
              method Method.foldr Foldable.definition (shift (shift (shift evidence))) Mono
                `Expression.call` ( Expression.Lambda $
                                      Expression.Lambda $
                                        shift (shift (shift (shift (shift (step rest)))))
                                          `Expression.call` shift (shift (shift (shift Expression.lambdaVariable)))
                                          `Expression.call` shift Expression.lambdaVariable
                                          `Expression.call` Expression.lambdaVariable
                                  )
                `Expression.call` Expression.lambdaVariable
                `Expression.call` shift Expression.lambdaVariable

-- |
-- > traverse f (C x1 ... xn) = pure C <*> t1 f x1 <*> ... <*> tn f xn
--
-- The applicative is the method's own: its evidence is what the method
-- assumes first.
traverse ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ForallOver Core.TypeF Void (Local ':+ scope) ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
traverse context position typeIndex datax scheme = do
  constructors <- made context position "Traversable" Type2.Traversable typeIndex datax
  pure $ do
    constructors <- constructors
    pure $ generated scheme (map (definition . shift') constructors) bottom
  where
    shift' Made {constructor, info, shapes} =
      Made {constructor = shift constructor, info = shift info, shapes = map shifted shapes}
    applicative :: Evidence (Local ':+ scope')
    applicative =
      Variable
        { variable = Index.Evidence.Index (Evidence0.Assumed 0),
          instanciation = Mono
        }
    definition constructorx@Made {constructor, info, shapes} =
      Definition
        { definition =
            Bound
              { patternx = Pattern.Wildcard,
                body =
                  Bound
                    { patternx = shift (matching constructorx),
                      body = Plain {plain = Done {done = built}}
                    }
              }
        }
      where
        given = shift (shift applicative)
        built =
          foldl
            ( \before (index, fieldShape) ->
                method Method.ap Applicative.definition given Mono
                  `Expression.call` before
                  `Expression.call` ( shift (shift (stepper applicative fieldShape))
                                        `Expression.call` shift Expression.patternVariable
                                        `Expression.call` Expression.patternVariableAt' index
                                    )
            )
            ( method Method.pure Applicative.definition given Mono
                `Expression.call` Expression.simplifyConstructor (shift (shift constructor)) (shift (shift info)) 0 Nil
            )
            (zip [0 ..] shapes)
    -- \f x -> x gone through by f, in the applicative
    stepper :: Evidence scope' -> Shape (Evidence scope') -> Expression scope'
    stepper given = \case
      Fixed ->
        Expression.Lambda $
          Expression.Lambda $
            method Method.pure Applicative.definition (shift (shift given)) Mono
              `Expression.call` Expression.lambdaVariable
      Whole ->
        Expression.Lambda $
          Expression.Lambda $
            shift Expression.lambdaVariable `Expression.call` Expression.lambdaVariable
      Inside evidence rest ->
        Expression.Lambda $
          Expression.Lambda $
            method
              Method.traverse
              Traversable.definition
              (shift (shift evidence))
              (Instanciation (Strict.Vector.singleton (shift (shift given))))
              `Expression.call` (shift (shift (stepper given rest)) `Expression.call` shift Expression.lambdaVariable)
              `Expression.call` Expression.lambdaVariable
