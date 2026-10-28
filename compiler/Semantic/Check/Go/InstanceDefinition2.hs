module Semantic.Check.Go.InstanceDefinition2 where

import Control.Monad (filterM)
import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import Core.Substitute (Category (Substitute), logicalType)
import qualified Core.Substitute as Substitute
import qualified Core.Tree.Class as Core.Class
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Substitute (substituteType)
import qualified Core.Tree.Constraint as Core (Constraint, ConstraintF (..))
import qualified Core.Tree.Constraint as Core.Constraint (argument)
import qualified Core.Tree.Constraints as Core.Constraints (ConstraintsF (..), Prerequisites (..), simplify)
import qualified Core.Tree.Constructor as Core.Constructor (ConstructorF (..), Shape (..))
import qualified Core.Tree.Data as Core.Data (Data (..), DefinitionF (..))
import qualified Core.Tree.Entry as Core.Entry (EntryF (..))
import qualified Core.Tree.Data as Core (Data)
import qualified Core.Tree.Evidence as Core (Evidence)
import qualified Core.Tree.Evidence as Core.Evidence
import Core.Tree.EvidenceSet (EvidenceSet (..))
import Core.Tree.Forall (ForallOver)
import qualified Core.Tree.Forall as Core (ForallOver (..))
import qualified Core.Tree.Instanciation as Core (InstanciationF (Instanciation, Mono))
import qualified Core.Tree.Instanciation as Core.Instanciation
import Core.Tree.Type ((#), (-#>))
import qualified Core.Tree.Type as Core (Type, TypeF, typeWith, universe)
import qualified Core.Tree.Type as Core.Type
import Core.Tree.TypeDeclaration (assumeClass, assumeData)
import qualified Core.Tree.TypeDeclarationExtra as Extra
import qualified Core.Tree.TypeLambda as Core (TypeLambdaOver (..))
import Core.Type.Functor (shiftLogical)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.Functor.Identity (Identity (..))
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Error (cannotDerive, cyclicalTypeChecking, unsupportedFeatureEqualityConstraints)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context (..))
import Semantic.Check.LocalBinding (Equality (Equality))
import qualified Semantic.Check.Context as Context
import qualified Semantic.Check.Derive.Bounded as Bounded
import qualified Semantic.Check.Derive.Enum as Enum
import qualified Semantic.Check.Derive.Eq as Eq
import qualified Semantic.Check.Derive.Functor as Functor
import qualified Semantic.Check.Derive.Ix as Ix
import qualified Semantic.Check.Derive.Read as Read
import qualified Semantic.Check.Derive.Ord as Ord
import qualified Semantic.Check.Derive.Show as Show
import Semantic.Check.Go.Definition4 (Solve (..))
import qualified Semantic.Check.Go.Scheme as Scheme
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Simple.Scheme as Core.Scheme
import qualified Semantic.Check.Temporary.Constraints as Unsolved.Constraints (check, solve)
import qualified Semantic.Check.Temporary.Definition as Definition
import qualified Semantic.Check.Temporary.Scheme as Unsolved (augment)
import qualified Semantic.Check.Temporary.Type as Unsolved.Type (check, solve)
import qualified Semantic.Check.Temporary.TypePattern as Unsolved (TypePattern (..), solve)
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Table.Type as Table.Type
import qualified Semantic.Index.Type as Type (Index)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Go.Type as Type (label)
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Implicit (Implicit (Resolve))
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Combinators.Inferred as Inferred (get)
import qualified Semantic.Tree.Constraint as Semantic (Constraint (..))
import Semantic.Tree.Constraints (Constraints (Constraints, None))
import Semantic.Tree.InstanceDefinition (InstanceDefinition (..))
import Semantic.Tree.InstanceDefinition2 (Annotation (..), Header (..), InstanceDefinition2 (..))
import Semantic.Tree.MethodConcrete (Auto, MethodConcrete (..))
import qualified Semantic.Tree.Type as Type (Synonym (..), Type (..))
import Semantic.Tree.TypePattern (TypePattern (..))
import qualified Semantic.Unify as Unify
import qualified Semantic.Unify.Builtin as Unify.Builtin (constrain)
import Syntax.Position (Position)
import qualified Syntax.Printer as Printer
import qualified Syntax.Tree.Type as Syntax.Type
import Prelude hiding (head)

data Key scope
  = Data
      { index1 :: !(Type2.Index scope),
        head1 :: !(Type.Index scope)
      }
  | Class
      { index2 :: !(Type.Index scope),
        head2 :: !(Type2.Index scope)
      }

keyIndex :: Key scope -> Type2.Index scope
keyIndex = \case
  Data {index1} -> index1
  Class {index2} -> Type2.Index index2

keyHead :: Key scope -> Type2.Index scope
keyHead = \case
  Data {head1} -> Type2.Index head1
  Class {head2} -> head2

-- |
-- What an instance of a class for a type requires, when nothing says: when
-- it is asked for in a @deriving@ clause. It requires what it takes to have
-- the class for everything the type holds. That is found by going through
-- what the type holds: where that is a type variable of the type's, given
-- anything or nothing, the class is required of it; where it is another
-- type, what the instance for that type requires is gone through in turn;
-- and the type itself is passed over, since what it requires is what is
-- being found.
--
-- Nothing is found for a type with a constructor that says what type it
-- makes, whose types are in variables of its own.
requirements ::
  forall s scope.
  Context s (Local ':+ scope) ->
  Type2.Index scope ->
  Type2.Index scope ->
  Core.Data.Data scope ->
  ST s (Maybe [Core.Constraint scope])
requirements Context {typeEnvironment} derived self datax
  | indexed datax = pure Nothing
  | otherwise = do
      found <- within [(shift derived, shift self)] (shift derived) (held datax)
      pure (Just (once [] (reverse found)))
  where
    -- A class of types that take an argument is had for everything the
    -- type holds by having it for the types its last parameter is the last
    -- argument of.
    functorial = derived `elem` [Type2.Functor, Type2.Foldable, Type2.Traversable]
    indexed :: Core.Data.Data scope' -> Bool
    indexed Core.Data.Data {definition = Core.Data.Definition {constructors}} = any telling constructors
    telling Core.Constructor.Constructor {shape} = case shape of
      Core.Constructor.Ordinary -> False
      Core.Constructor.Indexed {} -> True
    -- The types of what a type holds, in the type's own parameters.
    held :: Core.Data.Data scope' -> [Core.Type (Local ':+ scope')]
    held Core.Data.Data {parameters = typeParameters, definition = Core.Data.Definition {constructors}} =
      [ found
      | Core.Constructor.Constructor {entries} <- toList constructors,
        Core.Entry.Entry {entry = Core.ForallOver {parameters, constraints = Core.Constraints.None, result}} <- toList entries,
        null parameters,
        let typex = Shift.map (Shift.Unshift $ error "a type variable that nothing binds") result,
        found <-
          if functorial
            then Functor.inner (length typeParameters - 1) typex
            else [typex]
      ]
    -- Each thing that is required, once, and the last time it was found.
    -- There are thousands for a type that holds many types, most of them
    -- the same, and going through them is not as deep as they are many:
    -- what has been kept is looked at before the next is gone on to.
    once kept = \case
      [] -> kept
      constraint : rest
        | any (same constraint) kept -> once kept rest
        | otherwise -> once (constraint : kept) rest
    same
      Core.Constraint {classx = class1, head = head1, arguments = arguments1}
      Core.Constraint {classx = class2, head = head2, arguments = arguments2} =
        class1 == class2 && head1 == head2 && arguments1 == arguments2
    spine typex arguments = case typex of
      Core.Type.Call function argument -> spine function (argument : arguments)
      _ -> (typex, arguments)
    -- What having a class for each of some types requires. The pairs of a
    -- class and a type are the instances that are being worked out: one of
    -- them requires nothing more than is found already.
    within ::
      [(Type2.Index (Local ':+ scope), Type2.Index (Local ':+ scope))] ->
      Type2.Index (Local ':+ scope) ->
      [Core.Type (Local ':+ scope)] ->
      ST s [Core.Constraint scope]
    within visiting asked types = concat <$> traverse (simplify visiting asked) types
    simplify ::
      [(Type2.Index (Local ':+ scope), Type2.Index (Local ':+ scope))] ->
      Type2.Index (Local ':+ scope) ->
      Core.Type (Local ':+ scope) ->
      ST s [Core.Constraint scope]
    simplify visiting asked typex = case spine typex [] of
      (Core.Type.Variable (Local.Local head), arguments) ->
        pure [Core.Constraint {classx = Type2.unlocal asked, head, arguments = Strict.Vector.fromList arguments}]
      (Core.Type.Constructor head, arguments)
        | (asked, head) `elem` visiting -> pure []
        | Just TypeBinding.Requires {clause, prerequisites} <- instanceOf asked head -> do
            -- An instance that a deriving clause asks for requires what
            -- its own type holds requires, and that is gone through here
            -- rather than asked of it: two types may each hold the other.
            other <- if clause then content head else pure Nothing
            case other of
              Just datax | not (indexed datax) ->
                within
                  ((asked, head) : visiting)
                  asked
                  (substituteType (Vector.fromList arguments) <$> held datax)
              _ -> do
                Core.Constraints.Prerequisites {required} <- prerequisites
                case required of
                  Core.Constraints.None -> pure []
                  Core.Constraints.Constraints wanted ->
                    fmap concat $ for (toList wanted) $ \constraint@Core.Constraint {classx = next} ->
                      simplify
                        visiting
                        next
                        (substituteType (Vector.fromList arguments) (Core.Constraint.argument constraint))
        | otherwise -> do
            -- An instance that is built in says what it requires by asking
            -- for it.
            collected <- newSTRef []
            let unused :: Unify.Evidence s (Local ':+ scope)
                unused =
                  Core.Evidence.Variable
                    { variable = Evidence.Direct asked head,
                      instanciation = Core.Instanciation.Mono
                    }
                record next typex = do
                  more <- simplify visiting next typex
                  modifySTRef collected (++ more)
                  pure unused
            _ <- Unify.Builtin.constrain (pure unused) record asked head arguments
            readSTRef collected
      _ -> pure []
    -- The declaration of a type, if it is one a program declares.
    content :: Type2.Index (Local ':+ scope) -> ST s (Maybe (Core.Data.Data (Local ':+ scope)))
    content = \case
      Type2.Index index -> Just . assumeData <$> TypeBinding.content (typeEnvironment Table.Type.! index)
      _ -> pure Nothing
    instanceOf asked head
      | Type2.Index index <- asked,
        TypeBinding.TypeBinding {classInstances} <- typeEnvironment Table.Type.! index,
        Just instancex <- Map.lookup head classInstances =
          Just instancex
      | Type2.Index index <- head,
        TypeBinding.TypeBinding {dataInstances} <- typeEnvironment Table.Type.! index,
        Just instancex <- Map.lookup asked dataInstances =
          Just instancex
      | otherwise = Nothing

-- |
-- A parameter that an instance requires to be a type, by its place, with
-- the type. The parameter may be on either side.
equality ::
  Type.Type Position Resolve (Local ':+ scope) ->
  Type.Type Position Resolve (Local ':+ scope) ->
  Position ->
  (Int, Type.Type Position Resolve (Local ':+ scope))
equality left right startPosition = case (left, right) of
  (Type.Variable {variable = Local.Local index}, typex) -> (index, typex)
  (typex, Type.Variable {variable = Local.Local index}) -> (index, typex)
  _ -> unsupportedFeatureEqualityConstraints startPosition

checkHeader ::
  Position ->
  Key scope ->
  (f (ST s) -> Context s scope) ->
  Header Resolve scope ->
  Topological.Formula f s (Header Check scope)
checkHeader position key information Header {parameters, prerequisites, lenient} =
  Topological.Formula
    { cycle = cyclicalTypeChecking position,
      run = \declarations -> do
        let context = information declarations
            outer = context
            fresh TypePattern {name, position} = do
              level <- Unify.fresh Core.universe
              typex <- Unify.fresh (Core.typeWith level)
              pure
                Unsolved.TypePattern
                  { name,
                    typex,
                    position
                  }
        parameters <- traverse fresh parameters
        context <- pure $ Unsolved.augment parameters context
        indexKind <- Context.lookupKind position context (shift $ keyIndex key)
        headKind <- Context.lookupKind position context (shift $ keyHead key)
        universe <- Unify.fresh Core.universe
        target <- Unify.fresh (Core.typeWith universe)
        let types = [shiftLogical typex | Unsolved.TypePattern {typex} <- toList parameters]
            headKind' = foldr (-#>) target types
        Unify.unify context position headKind' headKind
        Unify.unify context position (target -#> Core.Type.Constraint) indexKind

        -- An instance from a deriving clause requires the class of each
        -- parameter, but only of those it can be asked of: the ones whose
        -- kind can be the kind the class takes. A parameter that stands
        -- for a stage or a scope, or for a type that takes arguments, is
        -- left out.
        prerequisites <- case prerequisites of
          Constraints wanted | lenient -> do
            let fits = \case
                  Semantic.Constraint {head} ->
                    Unify.alike (shiftLogical (Unsolved.typex (parameters Strict.Vector.! head))) target
                  Semantic.Equality {} -> pure True
            kept <- filterM fits (toList wanted)
            pure $ case kept of
              [] -> None
              _ -> Constraints (Strict.Vector.fromList kept)
          _ -> pure prerequisites
        -- A parameter that is required to be a type is taken out of what
        -- is required: it needs no evidence. It has to be a parameter that
        -- is said to be a type, on either side.
        let (classes, equalities) = case prerequisites of
              None -> (None, [])
              Constraints wanted ->
                ( case [constraint | constraint@Semantic.Constraint {} <- toList wanted] of
                    [] -> None
                    classes -> Constraints (Strict.Vector.fromList classes),
                  [equality left right startPosition | Semantic.Equality {startPosition, left, right} <- toList wanted]
                )
        prerequisites <- Unsolved.Constraints.check context classes
        equated <- for equalities $ \(index, typex) -> do
          let kind = shiftLogical (Unsolved.typex (parameters Strict.Vector.! index))
          typex <- Unsolved.Type.check context kind typex
          pure (index, typex)
        parameters <- Unify.runSolve $ traverse Unsolved.solve parameters
        prerequisites <- Unify.runSolve $ Unsolved.Constraints.solve context prerequisites
        equated <- Unify.runSolve $ for equated $ \(index, typex) -> do
          typex <- Unsolved.Type.solve context typex
          pure (index, typex)
        -- An instance from a deriving clause requires what it takes to
        -- have the class for everything the type holds, if that can be
        -- found.
        found <-
          if lenient
            then do
              let Context {typeEnvironment} = outer
              datax <- do
                let get index = assumeData <$> TypeBinding.content (typeEnvironment Table.Type.! index)
                Builtin.index pure get (keyHead key)
              requirements context (keyIndex key) (keyHead key) datax
            else pure Nothing
        let needed = case found of
              Just [] -> Core.Constraints.None
              Just constraints -> Core.Constraints.Constraints (Strict.Vector.fromList constraints)
              Nothing -> Core.Constraints.simplify prerequisites
        pure
          Header
            { parameters,
              prerequisites,
              equated = Strict.Vector.fromList equated,
              needed = Solved needed,
              lenient
            }
    }

data Source origin scope where
  Manual :: Source origin scope
  -- `Core.Data` may be undefined. This, however, should never happen for
  -- `deriving` clauses that are not orphans.
  Derive ::
    !(Type2.Index scope) ->
    !(Type2.Index scope) ->
    Core.Data scope ->
    Source Auto scope

data Method s scope = Method
  { position :: Position,
    context :: Context s (Local ':+ scope),
    base :: Core.Type (Local ':+ scope),
    self :: Core.Evidence (Local ':+ scope),
    extra :: Unify.Solve s (ClassExtra scope)
  }

checkMethod ::
  Method s scope ->
  Source origin scope ->
  Int ->
  ForallOver Core.TypeF Void (Local ':+ scope) ->
  MethodConcrete origin Group Resolve (Local ':+ scope) ->
  ST s (Unify.Solve s (MethodConcrete origin Group Check scope))
checkMethod Method {position, context} Manual _ scheme (Definition (Resolve member)) = do
  let Core.ForallOver {parameters, constraints, result} = scheme
  result <- pure $ logicalType result
  context <- Core.Scheme.augmentForall position scheme Mask.Runtime context
  definition <- Definition.check context result member
  pure $ do
    result <- Definition.solve definition
    pure $
      Definition $
        Implicit.Check
          Core.TypeLambdaOver
            { parameters,
              constraints,
              result
            }
checkMethod Method {context, position} (Derive Type2.Eq typeIndex datax) index _ _
  | Method.Equal <- toEnum index = Eq.equal context position typeIndex datax
checkMethod Method {context, position} (Derive Type2.Ord typeIndex datax) index _ _
  | Method.Compare <- toEnum index = Ord.compare context position typeIndex datax
checkMethod Method {context, position} (Derive Type2.Enum typeIndex datax) index _ _
  | Method.FromEnum <- toEnum index = Enum.fromEnum context position typeIndex datax
checkMethod Method {context, position} (Derive Type2.Enum typeIndex datax) index _ _
  | Method.ToEnum <- toEnum index = Enum.toEnum context position typeIndex datax
checkMethod Method {context, position, self} (Derive Type2.Enum typeIndex datax) index _ _
  | Method.EnumFrom <- toEnum index = Enum.enumFrom context position self typeIndex datax
checkMethod Method {context, position, self} (Derive Type2.Enum typeIndex datax) index _ _
  | Method.EnumFromThen <- toEnum index = Enum.enumFromThen context position self typeIndex datax
checkMethod Method {context, position} (Derive Type2.Bounded typeIndex datax) index _ _ =
  case toEnum index of
    Method.MinBound -> Bounded.minBound context position typeIndex datax
    Method.MaxBound -> Bounded.maxBound context position typeIndex datax
checkMethod Method {context, position} (Derive Type2.Show typeIndex datax) index _ _
  | Method.ShowsPrec <- toEnum index = Show.showsPrec context position typeIndex datax
checkMethod Method {context, position} (Derive Type2.Ix typeIndex datax) index _ _
  | Just derived <- case toEnum index of
      Method.IxRange -> Just Ix.range
      Method.IxIndex -> Just Ix.index
      Method.IxInRange -> Just Ix.inRange
      Method.IxRangeSize -> Nothing =
      derived context position typeIndex datax
checkMethod Method {context, position} (Derive Type2.Read typeIndex datax) index _ _
  | Method.ReadsPrec <- toEnum index = Read.readsPrec context position typeIndex datax
checkMethod Method {context, position} (Derive Type2.Functor typeIndex datax) index scheme _
  | Method.Fmap <- toEnum index = Functor.fmap context position typeIndex datax scheme
checkMethod Method {context, position} (Derive Type2.Foldable typeIndex datax) index scheme _
  | Method.Foldr <- toEnum index = Functor.foldr context position typeIndex datax scheme
checkMethod Method {context, position} (Derive Type2.Traversable typeIndex datax) index scheme _
  | Method.Traverse <- toEnum index = Functor.traverse context position typeIndex datax scheme
checkMethod Method {context, position, base, self, extra} source index scheme Generated {} = do
  case source of
    Manual -> pure ()
    Derive classx datax _ -> case classx of
      Type2.Eq -> pure ()
      Type2.Ord -> pure ()
      Type2.Enum -> pure ()
      Type2.Show -> pure ()
      Type2.Functor -> pure ()
      Type2.Foldable -> pure ()
      Type2.Traversable -> pure ()
      Type2.Ix -> pure ()
      Type2.Read -> pure ()
      _ ->
        let labelContext = Context.label context
            resolve =
              Type.Call
                { startPosition = (),
                  function =
                    Type.Constructor
                      { startPosition = (),
                        constructorPosition = (),
                        constructor = shift classx,
                        synonym = Type.NoSynonym
                      },
                  argument =
                    Type.Constructor
                      { startPosition = (),
                        constructorPosition = (),
                        constructor = shift datax,
                        synonym = Type.NoSynonym
                      }
                }
            syntax = Type.label labelContext resolve
            text = Printer.build $ Syntax.Type.print syntax
         in cannotDerive position text
  pure $ do
    let Core.ForallOver {parameters, constraints} = scheme
    defaultx <- do
      ClassExtra {defaults} <- extra
      pure $
        Core.TypeLambdaOver
          { parameters,
            constraints,
            result = defaults Strict.Vector.! index
          }
    let typeReplacements = Substitute.Types $ Vector.singleton base
        evidenceReplacements = Substitute.Evidences $ Vector.singleton self
        category = Substitute Shift.Shift typeReplacements evidenceReplacements
    pure $ Generated $ Solved $ Substitute.map category defaultx

-- |
-- A method of an instance, as it is checked: inside the type variables of
-- the method's own type. Where it was written, the type variables in scope
-- were the instance's parameters, and those are what its types still mean:
-- the method's own variables come between, and it has no names for them.
inside :: MethodConcrete origin Group Resolve scope -> MethodConcrete origin Group Resolve (Local ':+ scope)
inside = \case
  Definition definition -> Definition (shift definition)
  generated@Generated {} -> shift generated

checkBody ::
  (Monad solve) =>
  Position ->
  Key scope ->
  Solve solve logical s scope ->
  (declarations (ST s) -> InstanceDefinition2 solve' (ST s) Group Check scope) ->
  (declarations (ST s) -> Context s scope) ->
  (Context s scope -> Type2.Index scope -> Type2.Index scope -> ST s (Source origin scope)) ->
  Strict.Vector (MethodConcrete origin Group Resolve scope) ->
  Topological.Formula declarations s (solve (InstanceDefinition origin Group Check scope))
checkBody position key Solve {solve} reflection information lookup members =
  Topological.Formula
    { cycle = cyclicalTypeChecking position,
      run = \declarations -> do
        let context@Context {typeEnvironment} = information declarations
            index = keyIndex key
            head = keyHead key
        Header {parameters, needed, equated} <- case reflection declarations of
          Standard header ::: _ -> header
          DerivedInstance header ::: _ -> header
        let prerequisites = Inferred.get needed
        Core.Class.Class {constraints, definition = Core.Class.Definition {methods}} <- do
          let get index = assumeClass <$> TypeBinding.content (typeEnvironment Table.Type.! index)
          Builtin.index pure get index
        extra <- do
          let get index = do
                extra <- TypeBinding.extra (typeEnvironment Table.Type.! index)
                pure (Extra.assumeClass <$> extra)
          Builtin.index (pure . pure) get index
        source <- lookup context index head
        let base = foldl (#) (shift $ Core.Type.Constructor head) variables
              where
                variables = [Core.Type.Variable $ Local.Local i | i <- [0 .. length parameters - 1]]
            self = Core.Evidence.Variable {variable = shift variable, instanciation}
              where
                variable = case key of
                  Data {index1, head1} -> Evidence.Direct index1 (Type2.Index head1)
                  Class {index2, head2} -> Evidence.Direct (Type2.Index index2) head2
                instanciation = case prerequisites of
                  Core.Constraints.None -> Core.Mono
                  Core.Constraints.Constraints prerequisites -> Core.Instanciation $ Strict.Vector.fromList $ do
                    i <- [0 .. length prerequisites - 1]
                    pure
                      Core.Evidence.Variable
                        { variable = Evidence.Index (Evidence0.Assumed i),
                          instanciation = Core.Instanciation.Mono
                        }
        context <- Scheme.augmentNeeding position parameters prerequisites Mask.Runtime context
        -- Inside the instance, a parameter that is required to be a type
        -- is known to be it.
        context <- pure $ case context of
          Context {equalities} ->
            let known (index, typex) = Equality (Local.Local index) (logicalType (Core.Type.simplify typex))
             in context {equalities = fmap known (toList equated) ++ equalities}
        {-
          instances where the constraints contain the variable in the non head part
          should not kind check, so we should be able to safely ignore them
          For example:
          > instance (F (a a)) => G (H a)
        -}
        methods <-
          pure $
            let replacements = Substitute.Types $ Vector.singleton base
                category = Substitute.Over $ Substitute Shift.Shift replacements (error "no evidence")
                substitute Core.ForallOver {parameters, constraints, result} =
                  Core.ForallOver
                    { parameters,
                      constraints,
                      result = Substitute.map category result
                    }
             in substitute <$> methods
        evidence <- for constraints $
          \Core.Constraint {classx, arguments} -> case reverse (toList arguments) of
            -- What the class requires of the type given every type that
            -- has the same class. The evidence is a function of the
            -- evidence for that type: it is found for a type nothing is
            -- known of but that it has the class.
            Core.Type.Constructor Type2.Every : before -> do
              let given =
                    Core.Constraints.Constraints $
                      Strict.Vector.singleton
                        Core.Constraint {classx = shift classx, head = 0, arguments = Strict.Vector.empty}
                  kind = Core.Type.Type Core.Type.Small
              inner <- Core.Scheme.augment position (Strict.Vector.singleton kind) given Mask.Runtime context
              let parameter = foldl (#) (shift base) (shift <$> reverse before) # Core.Type.Variable (Local.Local 0)
              evidence <- Unify.constrain inner position (shift (shift classx)) (logicalType parameter)
              body <- Unify.runSolve $ Unify.solveEvidence position evidence
              pure Core.Evidence.Abstract {count = 1, body}
            _ -> do
              let parameter = foldl (#) base arguments
              evidence <- Unify.constrain context position (shift classx) (logicalType parameter)
              Unify.runSolve $ Unify.solveEvidence position evidence
        let arguments =
              Method
                { position,
                  context,
                  base,
                  self,
                  extra
                }
        members <- Strict.Vector.izipWithM (checkMethod arguments source) methods (inside <$> members)
        members <- solve $ sequence members
        pure $ do
          members <- members
          pure
            InstanceDefinition
              { evidence = Solved $ EvidenceSet $ evidence,
                members
              }
    }

check ::
  (Monad solve) =>
  Position ->
  Key scope ->
  Solve solve logical s scope ->
  (declarations (ST s) -> InstanceDefinition2 solve' (ST s) Group Check scope) ->
  (declarations (ST s) -> Context s scope) ->
  InstanceDefinition2 Identity Identity Group Resolve scope ->
  InstanceDefinition2 solve (Topological.Formula declarations s) Group Check scope
check position key solve reflection information = \case
  Standard (Identity header) ::: Identity (Identity InstanceDefinition {members}) ->
    Standard (checkHeader position key information header)
      ::: checkBody position key solve reflection information lookup members
    where
      lookup _ _ _ = pure Manual
  DerivedInstance (Identity header) ::: Identity (Identity InstanceDefinition {members}) ->
    DerivedInstance (checkHeader position key information header)
      ::: checkBody position key solve reflection information lookup members
    where
      lookup Context {typeEnvironment} index (Type2.Index head) = do
        datax <- assumeData <$> TypeBinding.content (typeEnvironment Table.Type.! head)
        pure $ Derive index (Type2.Index head) datax
      lookup _ index head = pure $ Derive index head $ error "not user defined data"

solve ::
  InstanceDefinition2 (Unify.Solve s) Identity Group Check scope ->
  Unify.Solve s (InstanceDefinition2 Identity Identity Group Check scope)
solve (annotation ::: Identity definition) = do
  definition <- definition
  pure $ annotation ::: Identity (Identity definition)
