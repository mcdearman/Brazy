module Semantic.Check.Go.Expression where

import qualified Core.Tree.Type as Levity (lazy)
import Control.Monad (when, zipWithM)
import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import qualified Core.Tree.Constructor (ConstructorF (..))
import qualified Core.Tree.Data (Data (..), DefinitionF (..))
import qualified Core.Tree.Entry (quantified)
import Core.Tree.Constraints (ConstraintsF (Value, levity))
import Core.Tree.Forall (ForallOver (..))
import Core.Tree.Instanciation (InstanciationF (Mono, levity))
import qualified Core.Tree.Data as Data
import qualified Core.Tree.Forall as Forall
import Core.Tree.Type ((#), (-#>))
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Type (lazy)
import Core.Tree.TypeDeclaration (assumeData)
import qualified Core.Tree.TypeLambda as Simple
import Core.Type.Functor (shiftLogical)
import qualified Core.Type.Functor as Core.Functor (map)
import Data.Foldable (toList)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict.Vector
import Error (badRunSTCall, mismatchedConstructorArguments)
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import Semantic.Check.Context (Context (..))
import Semantic.Check.Instanciate.Data (DataInstance (DataInstance))
import qualified Semantic.Check.Instanciate.Data as DataInstance
import Semantic.Check.Instanciate.Entry (EntryInstance (..))
import qualified Semantic.Check.Instanciate.Entry as EntryInstance (entry)
import qualified Semantic.Check.Go.Alternative as Alternative
import qualified Semantic.Check.Go.CallHead as CallHead
import qualified Semantic.Check.Go.Comprehension as Comprehension
import qualified Semantic.Check.Go.Declarations as Declarations
import qualified Semantic.Check.Go.Definition4 as Definition4
import qualified Semantic.Check.Go.Do as Do
import qualified Semantic.Check.Go.Lambda as Lambda
import qualified Semantic.Check.Go.RightHandSide as RightHandSide
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Simple.Data as Simple.Data
import Semantic.Check.Scheme (instanciate)
import qualified Semantic.Check.Scheme as Rigid (augment)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Check.Temporary.Pattern as Pattern
import qualified Semantic.Check.Temporary.UpdateInfo as UpdateInfo
import qualified Semantic.Check.Binding.Term as TermBinding (TermBinding (..), Type (..))
import qualified Semantic.Check.TypeAnnotation as Annotation
import qualified Semantic.Check.Binding.Type as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Local as Local (Index (Local))
import qualified Semantic.Index.Table.Term as Term ((!))
import qualified Semantic.Index.Table.Type as Type ((!))
import qualified Semantic.Index.Term as Term.Index (Index)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import Semantic.Shift (shift)
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Tree.CallHead as Head (CallHead (..))
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.Expression (Explicit (..))
import qualified Semantic.Tree.Expression as Semantic (Expression (..), Field (..))
import qualified Semantic.Tree.Expression as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Prelude hiding (Bool (False, True))
import qualified Prelude

-- |
-- What an expression is, once what is worked out of it while it is checked
-- is known.
type Solve s scope = Unify.Solve s (Solved.Expression Group Check scope)

-- |
-- Check an expression that is to be something a constructor holds.
fill :: Context s scope -> EntryInstance s scope -> Semantic.Expression Group Resolve scope -> ST s (Solve s scope)
fill context EntryInstance {quantified = Prelude.False, entry} expression = check context entry expression
fill context EntryInstance {position, scheme} expression = generalize context position scheme expression

-- |
-- Check an expression against a type with a @forall@ of its own. The
-- expression has to have the type whatever the variables of the @forall@
-- are, so they are type variables nothing is known of while it is checked,
-- but for what the type says is required of them.
generalize ::
  Context s scope ->
  Position ->
  Unify.Forall s scope ->
  Semantic.Expression Group Resolve scope ->
  ST s (Solve s scope)
generalize context position ForallOver {parameters, constraints, result} expression = do
  parameters <- Unify.runSolve $ traverse (Unify.solve context position) parameters
  constraints <- Unify.runSolve $ Unify.solve context position constraints
  inner <- Rigid.augment position parameters constraints Mask.Runtime context
  body <- check inner (Core.Functor.map shift result) (shift expression)
  pure $ do
    result <- body
    pure Solved.Generalized {expression = Known Simple.TypeLambdaOver {parameters, constraints, result}}

-- |
-- A constructor and what it is given, if an expression is that.
construction ::
  Semantic.Expression Group Resolve scope ->
  [Semantic.Expression Group Resolve scope] ->
  Maybe (Position, Constructor.Index scope, [Semantic.Expression Group Resolve scope])
construction expression arguments = case expression of
  Semantic.Call {function, argument} -> construction function (argument : arguments)
  Semantic.CallHead {callHead = Head.Constructor {constructorPosition, constructor}, arguments = given} ->
    Just (constructorPosition, constructor, toList given ++ arguments)
  _ -> Nothing

-- |
-- Check a constructor together with what it is given, if it holds
-- something of a type with a @forall@. Such a constructor is not a function
-- that can be given anything: what it is given there is checked against
-- that type.
--
-- The last thing it is given may be given through a function, if the
-- function can do nothing but apply: if its type is
-- @(a -> b) -> a -> b@ for any @a@ and @b@. The function is still called,
-- since it may differ from another of that type in what it evaluates.
holding ::
  Context s scope ->
  Unify.Type s scope ->
  Position ->
  Constructor.Index scope ->
  [Semantic.Expression Group Resolve scope] ->
  Maybe (Position, Term.Index.Index scope) ->
  ST s (Maybe (Solve s scope))
holding
  context@Context {typeEnvironment, termEnvironment}
  resultType
  constructorPosition
  constructor@(Constructor.Index typeIndex constructorIndex)
  arguments
  through = do
    datax <- do
      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
      Builtin.index pure get typeIndex
    let Core.Tree.Data.Data {definition = Core.Tree.Data.Definition {constructors = declared}} = datax
        Core.Tree.Constructor.Constructor {entries = held} = declared Strict.Vector.! constructorIndex
    applies <- case through of
      Nothing -> pure Prelude.True
      Just (_, variable) -> do
        let TermBinding.TermBinding binding = termEnvironment Term.! variable
        applying <$> binding
    if any Core.Tree.Entry.quantified held && applies
      then do
        DataInstance {constructors} <-
          Simple.Data.instanciate ConstructorInstance.Build context constructorPosition datax
        let instancex = constructors Strict.Vector.! constructorIndex
            entries = toList (ConstructorInstance.entries instancex)
            base = DataInstance.constructed instancex typeIndex
            (filled, rest) = splitAt (length arguments) entries
        when (length arguments > length entries) $ mismatchedConstructorArguments constructorPosition
        ConstructorInstance.mark context instancex
        Unify.unify context constructorPosition resultType (foldr ((-#>) . EntryInstance.entry) base rest)
        arguments <- zipWithM (fill context) filled arguments
        constructorInfo <- ConstructorInstance.built context instancex
        let made given = do
              callHead <- CallHead.constructor context constructorPosition constructor constructorInfo
              given <- sequence given
              pure Solved.CallHead {callHead, arguments = Strict.Vector.fromList given}
        pure $ Just $ case (through, reverse arguments) of
          (Just (variablePosition, variable), final : before) -> do
            callHead <- CallHead.variable context variablePosition variable Mono {levity = Levity.lazy}
            made <- made (reverse before)
            final <- final
            pure Solved.CallHead {callHead, arguments = Strict.Vector.fromList [made, final]}
          _ -> made arguments
      else pure Nothing

-- |
-- Whether a function can do nothing but apply: whether its type is
-- @(a -> b) -> a -> b@ for any @a@ and @b@.
applying :: TermBinding.Type s scope -> Prelude.Bool
applying = \case
  TermBinding.Rigid
    ForallOver
      { parameters,
        constraints = Value {},
        result =
          Core.Function
            (Core.Function (Core.Variable from) (Core.Variable to))
            (Core.Function (Core.Variable from') (Core.Variable to'))
      } -> length parameters == 2 && from == from' && to == to' && from /= to
  _ -> Prelude.False

check :: Context s scope -> Unify.Type s scope -> Semantic.Expression Group Resolve scope -> ST s (Solve s scope)
check context@Context {termEnvironment} typex call@Semantic.CallHead {callHead, arguments} = do
  special <- case (callHead, toList arguments) of
    -- An action given to runST through a function that only applies, as in
    -- @runST $ do ...@. Running the action is all that is left of it: what
    -- such a function could evaluate first is the action, and running it
    -- evaluates that.
    (Head.Variable {variable}, [Semantic.RunSTAlone {startPosition}, imperative]) -> do
      let TermBinding.TermBinding binding = termEnvironment Term.! variable
      applies <- applying <$> binding
      if applies
        then Just <$> check context typex Semantic.RunST {startPosition, imperative}
        else badRunSTCall startPosition
    -- A constructor given the last thing it is given through a function
    -- that only applies, as in @C $ x@.
    (Head.Variable {variablePosition, variable}, [left, right])
      | Just (position, constructor, given) <- construction left [] ->
          holding context typex position constructor (given ++ [right]) (Just (variablePosition, variable))
    _
      | Just (position, constructor, given) <- construction call [] ->
          holding context typex position constructor given Nothing
      | otherwise -> pure Nothing
  case special of
    Just expression -> pure expression
    Nothing -> do
      types <- sequence $ Strict.Vector.replicate (length arguments) $ Unify.fresh Core.typex
      let function = foldr (-#>) typex types
      callHead <- CallHead.check context function callHead
      arguments <- sequence $ Strict.Vector.zipWith (check context) types arguments
      pure $ do
        callHead <- callHead
        arguments <- sequence arguments
        pure Solved.CallHead {callHead, arguments}
check
  context@Context {typeEnvironment}
  typex
  Semantic.Record
    { constructorPosition,
      constructor,
      fields
    } =
    do
      let Constructor.Index typeIndex constructorIndex = constructor
      datax <- do
        let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
        Builtin.index pure get typeIndex
      DataInstance {constructors} <-
        Simple.Data.instanciate ConstructorInstance.Build context constructorPosition datax
      let instancex = constructors Strict.Vector.! constructorIndex
          base = DataInstance.constructed instancex typeIndex
          entries = ConstructorInstance.entries instancex
          lookup index = fill context (entries Strict.Vector.! index)
      ConstructorInstance.mark context instancex
      Unify.unify context constructorPosition typex base
      constructorInfo <- ConstructorInstance.built context instancex
      let checkField Semantic.Field {index, field} = do
            field <- lookup index field
            pure $ do
              field <- field
              pure Solved.Field {index, field}
      fields <- traverse checkField fields
      pure $ do
        fields <- sequence fields
        constructorInfo <- ConstructorInfo.solve context constructorInfo
        pure
          Solved.Record
            { constructorPosition,
              constructor,
              constructorInfo = Solved constructorInfo,
              fields
            }
check context typex Semantic.List {startPosition, items} = do
  inner <- Unify.fresh Core.typex
  Unify.unify context startPosition typex (Core.list inner)
  items <- traverse (check context inner) items
  pure $ do
    items <- sequence items
    pure Solved.List {startPosition, items}
check context resultType call@Semantic.Call {function, argument} = do
  special <- case call of
    _
      | Just (position, constructor, arguments) <- construction call [] ->
          holding context resultType position constructor arguments Nothing
      | otherwise -> pure Nothing
  case special of
    Just expression -> pure expression
    Nothing -> do
      argumentType <- Unify.fresh Core.typex
      function <- check context (argumentType -#> resultType) function
      argument <- check context argumentType argument
      pure $ do
        function <- function
        argument <- argument
        pure Solved.Call {function, argument}
check context typex Semantic.Let {declarations, letBody} = do
  context Declarations.:& declarations <- Declarations.check context declarations
  letBody <- check context (shiftLogical typex) letBody
  pure $ do
    declarations <- declarations
    letBody <- letBody
    pure Solved.Let {declarations, letBody}
check context typex Semantic.If {condition, thenx, elsex} = do
  condition <- check context Core.bool condition
  thenx <- check context typex thenx
  elsex <- check context typex elsex
  pure $ do
    condition <- condition
    thenx <- thenx
    elsex <- elsex
    pure Solved.If {condition, thenx, elsex}
check context typex Semantic.MultiwayIf {branches} = do
  branches <- traverse (RightHandSide.check context typex) branches
  pure $ do
    branches <- sequence branches
    pure Solved.MultiwayIf {branches}
check context typex Semantic.Integer {startPosition, integer} = do
  evidence <- Unify.constrain context startPosition Type2.Num typex
  pure $ do
    evidence <- Unify.solveEvidence context startPosition evidence
    pure Solved.Integer {startPosition, integer, evidence = Solved evidence}
check context typex Semantic.Float {startPosition, float} = do
  evidence <- Unify.constrain context startPosition Type2.Fractional typex
  pure $ do
    evidence <- Unify.solveEvidence context startPosition evidence
    pure Solved.Float {startPosition, float, evidence = Solved evidence}
check context typex Semantic.String {startPosition, string} = do
  Unify.unify context startPosition typex (Core.list Core.char)
  pure $ pure Solved.String {startPosition, string}
check context typex Semantic.Character {startPosition, character} = do
  Unify.unify context startPosition typex Core.char
  pure $ pure Solved.Character {startPosition, character}
check context typex Semantic.Tuple {startPosition, elements} = do
  items <- for elements $ \element -> do
    typex <- Unify.fresh Core.typex
    element <- check context typex element
    pure (typex, element)
  let (types, elements) = Strict.Vector.unzip items
  let target = foldl (#) (Core.tupling $ length elements) types
  Unify.unify context startPosition typex target
  pure $ do
    elements <- sequence elements
    pure Solved.Tuple {startPosition, elements}
check context typex Semantic.Comprehension {startPosition, statements} = do
  statements <- Comprehension.check context typex statements
  pure $ do
    statements <- statements
    pure Solved.Comprehension {startPosition, statements}
check
  context@Context {typeEnvironment}
  typex
  Semantic.Update
    { base,
      updatePosition,
      updateType,
      updates,
      permissive
    } = do
    datax <- do
      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
      Builtin.index pure get updateType
    instancex <- Simple.Data.instanciateMark context updatePosition datax
    let typex' = DataInstance.baseType instancex updateType
        updateInfo = DataInstance.updateInfo instancex
        checkSelect Semantic.Field {index, field} = do
          -- What is put in has the type that the value holds there.
          Unify.unify context updatePosition typex (DataInstance.selected instancex updateType index)
          field <- fill context (DataInstance.selectorEntry instancex index) field
          pure $ do
            field <- field
            pure Solved.Field {index, field}
    Unify.unify context updatePosition typex typex'
    base <- check context typex base
    updates <- sequence $ flip fmap updates checkSelect
    pure $ do
      base <- base
      updates <- sequence updates
      updateInfo <- UpdateInfo.solve context updateInfo
      pure
        Solved.Update
          { base,
            updatePosition,
            updateType,
            updates,
            updateInfo = Solved updateInfo,
            permissive
          }
check context typex Semantic.Case {startPosition, scrutinee, cases} = do
  binder <- Unify.fresh Core.typex
  scrutinee <- check context binder scrutinee
  cases <- traverse (Alternative.check context typex binder) cases
  pure $ do
    scrutinee <- scrutinee
    cases <- sequence cases
    pure Solved.Case {startPosition, scrutinee, cases}
check context typex Semantic.Do {startPosition, dox} = do
  dox <- Do.check context typex dox
  pure $ do
    dox <- dox
    pure Solved.Do {startPosition, dox}
check context typex Semantic.Lambda {startPosition, parameter, body} = do
  parameterType <- Unify.fresh Core.typex
  resultType <- Unify.fresh Core.typex
  -- What the function is to be is said before its parameter is looked at:
  -- matching on a constructor that says what type it makes tells nothing
  -- of a type that is not known yet.
  Unify.unify context startPosition typex (parameterType -#> resultType)
  parameter <- Pattern.check context Type.lazy parameterType parameter
  body <- Lambda.check (Pattern.augment parameter context) (shiftLogical resultType) body
  Pattern.sealed context parameter [parameterType, resultType]
  pure $ do
    parameter <- Pattern.solve context parameter
    body <- body
    pure Solved.Lambda {startPosition, parameter, body}
check context typex Semantic.LambdaCase {startPosition, cases} = do
  parameterType <- Unify.fresh Core.typex
  resultType <- Unify.fresh Core.typex
  Unify.unify context startPosition typex (parameterType -#> resultType)
  cases <- traverse (Alternative.check context resultType parameterType) cases
  pure $ do
    cases <- sequence cases
    pure Solved.LambdaCase {startPosition, cases}
check context typex Semantic.RightSection {left, operatorPosition, right} = do
  argumentType1 <- Unify.fresh Core.typex
  argumentType2 <- Unify.fresh Core.typex
  result <- Unify.fresh Core.typex
  let target = argumentType1 -#> argumentType2 -#> result
  left <- CallHead.check context target left
  right <- check context argumentType2 right
  Unify.unify context operatorPosition typex (argumentType1 -#> result)
  pure $ do
    left <- left
    right <- right
    pure Solved.RightSection {operatorPosition, left, right}
check context typex Semantic.LeftSection {operator, operatorPosition, operand} = do
  argumentType1 <- Unify.fresh Core.typex
  argumentType2 <- Unify.fresh Core.typex
  result <- Unify.fresh Core.typex
  let target = argumentType1 -#> argumentType2 -#> result
  operator <- CallHead.check context target operator
  operand <- check context argumentType1 operand
  Unify.unify context operatorPosition typex (argumentType2 -#> result)
  pure $ do
    operator <- operator
    operand <- operand
    pure Solved.LeftSection {operatorPosition, operator, operand}
check context typex Semantic.Annotation {expression = Explicit expression, operatorPosition, annotation} = do
  annotation <- Annotation.checkAnnotation context annotation
  let annotation' = Forall.simplify annotation
  expression <- Definition4.checkAnnotation context operatorPosition annotation $
    \context typex -> check context typex expression
  (typex', instanciation) <- instanciate context operatorPosition annotation'
  Unify.unify context operatorPosition typex typex'
  pure $ do
    expression <- expression
    instanciation <- Unify.solveEvidence context operatorPosition instanciation
    pure
      Solved.Annotation
        { operatorPosition,
          expression = Known expression,
          annotation,
          instanciation = Solved instanciation
        }
-- What is run has to be an action whatever its state is, so that nothing of
-- the state can be part of what it gives: its type is one with a forall,
--
-- > forall s. ST s a
--
-- for the type @a@ that is wanted.
check _ _ Semantic.RunSTAlone {startPosition} = badRunSTCall startPosition
check context typex Semantic.RunST {startPosition, imperative} = do
  let state = Core.Variable (Local.Local 0)
      scheme =
        ForallOver
          { parameters = Strict.Vector.singleton Core.typex,
            constraints = Value {levity = Levity.lazy},
            result = Core.Constructor Type2.ST # state # Shift0.map Shift0.Shift typex
          }
  imperative <- generalize context startPosition scheme imperative
  pure $ do
    imperative <- imperative
    pure Solved.RunST {startPosition, imperative}
-- Only the checker makes these.
check _ _ Semantic.Generalized {} = error "an expression the resolver does not make"
check context typex Semantic.Negate {startPosition, negative} = do
  evidence <- Unify.constrain context startPosition Type2.Num typex
  negative <- check context typex negative
  pure $ do
    evidence <- Unify.solveEvidence context startPosition evidence
    negative <- negative
    pure
      Solved.Negate
        { startPosition,
          evidence = Solved evidence,
          negative
        }
