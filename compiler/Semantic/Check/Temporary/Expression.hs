module Semantic.Check.Temporary.Expression where

import Control.Monad (when, zipWithM)
import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import qualified Core.Tree.Constructor (ConstructorF (..))
import qualified Core.Tree.Data (Data (..))
import qualified Core.Tree.Entry (quantified)
import Core.Tree.Constraints (ConstraintsF (None))
import Core.Tree.Forall (ForallOver (..))
import Core.Tree.Instanciation (InstanciationF (Mono))
import qualified Core.Tree.Forall as Forall
import Core.Tree.Type ((#), (-#>))
import qualified Core.Tree.Type as Core
import Core.Tree.TypeDeclaration (assumeData)
import qualified Core.Tree.TypeLambda as Simple
import Core.Type.Functor (shiftLogical)
import qualified Core.Type.Functor as Core.Functor (map)
import Data.Foldable (toList)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (badRunSTCall, mismatchedConstructorArguments)
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context (..))
import Semantic.Check.DataInstance (DataInstance (DataInstance))
import qualified Semantic.Check.DataInstance as DataInstance
import Semantic.Check.EntryInstance (EntryInstance (..))
import qualified Semantic.Check.EntryInstance as EntryInstance (entry)
import qualified Semantic.Check.Go.Declarations as Declarations
import qualified Semantic.Check.Go.Definition4 as Definition4
import Semantic.Check.Go.Scheme (Scheme)
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Simple.Data as Simple.Data
import Semantic.Check.Simple.Scheme (instanciate)
import qualified Semantic.Check.Simple.Scheme as Rigid (augment)
import Semantic.Check.Temporary.Alternative (Alternative)
import qualified Semantic.Check.Temporary.Alternative as Alternative
import Semantic.Check.Temporary.CallHead (CallHead)
import qualified Semantic.Check.Temporary.CallHead as CallHead
import Semantic.Check.Temporary.Comprehension (Comprehension)
import qualified Semantic.Check.Temporary.Comprehension as Comprehension
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import Semantic.Check.Temporary.Do (Do)
import qualified Semantic.Check.Temporary.Do as Do
import Semantic.Check.Temporary.ExpressionField (Field)
import qualified Semantic.Check.Temporary.ExpressionField as Field
import Semantic.Check.Temporary.Lambda (Lambda)
import qualified Semantic.Check.Temporary.Lambda as Lambda
import Semantic.Check.Temporary.Pattern (Pattern)
import qualified Semantic.Check.Temporary.Pattern as Pattern
import Semantic.Check.Temporary.RightHandSide (RightHandSide)
import qualified Semantic.Check.Temporary.RightHandSide as RightHandSide
import Semantic.Check.Temporary.Select (Select)
import qualified Semantic.Check.Temporary.Select as Select
import Semantic.Check.Temporary.UpdateInfo (UpdateInfo)
import qualified Semantic.Check.Temporary.UpdateInfo as UpdateInfo
import qualified Semantic.Check.TermBinding as TermBinding (TermBinding (..), Type (..))
import qualified Semantic.Check.TypeAnnotation as Annotation
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Local as Local (Index (Local))
import qualified Semantic.Index.Table.Term as Term ((!))
import qualified Semantic.Index.Table.Type as Type
import qualified Semantic.Index.Term as Term.Index (Index)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Shift (shift)
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Tree.CallHead as Head (CallHead (..))
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Declarations as Proper.Declarations
import Semantic.Tree.Expression (Explicit (..))
import qualified Semantic.Tree.Expression as Semantic (Expression (..))
import qualified Semantic.Tree.Expression as Solved
import qualified Semantic.Tree.Select as Semantic (Select (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.StringLiteral (StringLiteral)
import Prelude hiding (Bool (False, True))
import qualified Prelude

data Expression s scope
  = CallHead
      { callHead :: !(CallHead s scope)
      }
  | Integer
      { startPosition :: !Position,
        integer :: !Integer,
        evidence :: !(Unify.Evidence s scope)
      }
  | Float
      { startPosition :: !Position,
        float :: !Rational,
        evidence :: !(Unify.Evidence s scope)
      }
  | Character
      { startPosition :: !Position,
        character :: !Char
      }
  | String
      { startPosition :: !Position,
        string :: !StringLiteral
      }
  | Tuple
      { startPosition :: !Position,
        elements :: !(Strict.Vector2 (Expression s scope))
      }
  | List
      { startPosition :: !Position,
        items :: !(Strict.Vector (Expression s scope))
      }
  | Comprehension
      { startPosition :: !Position,
        statements :: !(Comprehension s scope)
      }
  | Record
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        constructorInfo :: !(ConstructorInfo s scope),
        fields :: !(Strict.Vector (Field s scope))
      }
  | Update
      { base :: !(Expression s scope),
        updatePosition :: !Position,
        updateType :: !(Type2.Index scope),
        updates :: !(Strict.Vector1 (Select s scope)),
        updateInfo :: !(UpdateInfo s scope),
        permissive :: !Prelude.Bool
      }
  | Call
      { function :: !(Expression s scope),
        argument :: !(Expression s scope)
      }
  | Let
      { declarations :: !(Unify.Solve s (Proper.Declarations.Local Group Check scope)),
        letBody :: !(Expression s (Scope.Declaration ':+ scope))
      }
  | If
      { conditionx :: !(Expression s scope),
        thenx :: !(Expression s scope),
        elsex :: !(Expression s scope)
      }
  | Case
      { startPosition :: !Position,
        scrutinee :: !(Expression s scope),
        cases :: !(Strict.Vector (Alternative s scope))
      }
  | Lambda
      { startPosition :: !Position,
        parameter :: !(Pattern s scope),
        body :: !(Lambda s (Scope.Pattern ':+ scope))
      }
  | LambdaCase
      { startPosition :: !Position,
        cases :: !(Strict.Vector (Alternative s scope))
      }
  | MultiwayIf
      { branches :: !(Strict.Vector1 (RightHandSide s scope))
      }
  | Do
      { startPosition :: !Position,
        dox :: !(Do s scope)
      }
  | RunST
      { startPosition :: !Position,
        imperative :: !(Expression s scope)
      }
  | -- | An expression of a type with a @forall@ of its own, for a
    -- constructor to hold.
    Generalized
      { expression :: !(Unify.Solve s (Simple.TypeLambdaOver (Solved.Expression Group Check) scope))
      }
  | Annotation
      { expression :: !(Unify.Solve s (Simple.TypeLambdaOver (Solved.Expression Group Check) scope)),
        operatorPosition :: !Position,
        annotation :: !(Scheme Position Check scope),
        instanciation :: !(Unify.Instanciation s scope)
      }
  | RightSection
      { operatorPosition :: !Position,
        left :: !(CallHead s scope),
        right :: !(Expression s scope)
      }
  | LeftSection
      { operatorPosition :: !Position,
        operator :: !(CallHead s scope),
        operand :: !(Expression s scope)
      }
  | Negate
      { startPosition :: !Position,
        evidence :: !(Unify.Evidence s scope),
        negative :: !(Expression s scope)
      }

-- |
-- Check an expression that is to be something a constructor holds.
fill :: Context s scope -> EntryInstance s scope -> Semantic.Expression Group Resolve scope -> ST s (Expression s scope)
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
  ST s (Expression s scope)
generalize context position ForallOver {parameters, constraints, result} expression = do
  parameters <- Unify.runSolve $ traverse (Unify.solve position) parameters
  constraints <- Unify.runSolve $ Unify.solve position constraints
  inner <- Rigid.augment position parameters constraints Mask.Runtime context
  body <- check inner (Core.Functor.map shift result) (shift expression)
  pure
    Generalized
      { expression = do
          result <- solve body
          pure Simple.TypeLambdaOver {parameters, constraints, result}
      }

-- |
-- A constructor and what it is given, if an expression is that.
construction ::
  Semantic.Expression Group Resolve scope ->
  [Semantic.Expression Group Resolve scope] ->
  Maybe (Position, Constructor.Index scope, [Semantic.Expression Group Resolve scope])
construction expression arguments = case expression of
  Semantic.Call {function, argument} -> construction function (argument : arguments)
  Semantic.CallHead {callHead = Head.Constructor {constructorPosition, constructor}} ->
    Just (constructorPosition, constructor, arguments)
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
  ST s (Maybe (Expression s scope))
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
    let Core.Tree.Data.Data {constructors = declared} = datax
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
        let callHead = CallHead.Constructor {constructorPosition, constructor, constructorInfo}
            made = CallHead {callHead}
        pure $ Just $ case (through, reverse arguments) of
          (Just (variablePosition, variable), final : before) ->
            let apply = CallHead.Variable {variablePosition, variable, instanciation = Mono}
             in Call (Call CallHead {callHead = apply} (foldl Call made (reverse before))) final
          _ -> foldl Call made arguments
      else pure Nothing

-- |
-- Whether a function can do nothing but apply: whether its type is
-- @(a -> b) -> a -> b@ for any @a@ and @b@.
applying :: TermBinding.Type s scope -> Prelude.Bool
applying = \case
  TermBinding.Rigid
    ForallOver
      { parameters,
        constraints = None,
        result =
          Core.Function
            (Core.Function (Core.Variable from) (Core.Variable to))
            (Core.Function (Core.Variable from') (Core.Variable to'))
      } -> length parameters == 2 && from == from' && to == to' && from /= to
  _ -> Prelude.False

check :: Context s scope -> Unify.Type s scope -> Semantic.Expression Group Resolve scope -> ST s (Expression s scope)
check context typex Semantic.CallHead {callHead} = do
  callHead <- CallHead.check context typex callHead
  pure CallHead {callHead}
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
      fields <- traverse (Field.check lookup) fields
      pure $ Record {constructorPosition, constructor, fields, constructorInfo}
check context typex Semantic.List {startPosition, items} = do
  inner <- Unify.fresh Core.typex
  Unify.unify context startPosition typex (Core.list inner)
  items <- traverse (check context inner) items
  pure List {startPosition, items}
check context@Context {termEnvironment} resultType call@Semantic.Call {function, argument} = do
  special <- case call of
    -- An action given to runST through a function that only applies, as in
    -- @runST $ do ...@. Running the action is all that is left of it: what
    -- such a function could evaluate first is the action, and running it
    -- evaluates that.
    Semantic.Call
      { function =
          Semantic.Call
            { function = Semantic.CallHead {callHead = Head.Variable {variable}},
              argument = Semantic.RunSTAlone {startPosition}
            },
        argument = imperative
      } -> do
        let TermBinding.TermBinding binding = termEnvironment Term.! variable
        applies <- applying <$> binding
        if applies
          then Just <$> check context resultType Semantic.RunST {startPosition, imperative}
          else badRunSTCall startPosition
    -- A constructor given the last thing it is given through a function
    -- that only applies, as in @C $ x@.
    Semantic.Call
      { function =
          Semantic.Call
            { function = Semantic.CallHead {callHead = Head.Variable {variablePosition, variable}},
              argument = left
            },
        argument = right
      }
        | Just (position, constructor, arguments) <- construction left [] ->
            holding context resultType position constructor (arguments ++ [right]) (Just (variablePosition, variable))
    _
      | Just (position, constructor, arguments) <- construction call [] ->
          holding context resultType position constructor arguments Nothing
      | otherwise -> pure Nothing
  case special of
    Just expression -> pure expression
    Nothing -> do
      argumentType <- Unify.fresh Core.typex
      function1 <- check context (argumentType -#> resultType) function
      argument <- check context argumentType argument
      pure (Call function1 argument)
check context typex Semantic.Let {declarations, letBody} = do
  context Declarations.:& declarations <- Declarations.check context declarations
  letBody <- check context (shiftLogical typex) letBody
  pure (Let declarations letBody)
check context typex Semantic.If {condition, thenx, elsex} = do
  condition <- check context Core.bool condition
  thenx <- check context typex thenx
  elsex <- check context typex elsex
  pure (If condition thenx elsex)
check context typex Semantic.MultiwayIf {branches} = do
  branches <- traverse (RightHandSide.check context typex) branches
  pure (MultiwayIf branches)
check context typex Semantic.Integer {startPosition, integer} = do
  evidence <- Unify.constrain context startPosition Type2.Num typex
  pure $ Integer {startPosition, integer, evidence}
check context typex Semantic.Float {startPosition, float} = do
  evidence <- Unify.constrain context startPosition Type2.Fractional typex
  pure $ Float {startPosition, float, evidence}
check context typex Semantic.String {startPosition, string} = do
  Unify.unify context startPosition typex (Core.list Core.char)
  pure $ String {startPosition, string}
check context typex Semantic.Character {startPosition, character} = do
  Unify.unify context startPosition typex Core.char
  pure $ Character {startPosition, character}
check context typex Semantic.Tuple {startPosition, elements} = do
  items <- for elements $ \element -> do
    typex <- Unify.fresh Core.typex
    element <- check context typex element
    pure (typex, element)
  let (types, elements) = Strict.Vector.unzip items
  let target = foldl (#) (Core.tupling $ length elements) types
  Unify.unify context startPosition typex target
  pure $ Tuple {startPosition, elements}
check context typex Semantic.Comprehension {startPosition, statements} = do
  statements <- Comprehension.check context typex statements
  pure Comprehension {startPosition, statements}
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
    Unify.unify context updatePosition typex typex'
    base <- check context typex base
    updates <- sequence $ flip fmap updates $ \update@Semantic.Select {pick} -> do
      -- What is put in has the type that the value holds there.
      Unify.unify context updatePosition typex (DataInstance.selected instancex updateType pick)
      Select.check (\pick -> fill context (DataInstance.selectorEntry instancex pick)) update
    let updateInfo = DataInstance.updateInfo instancex
    pure
      Update
        { base,
          updatePosition,
          updateType,
          updates,
          updateInfo,
          permissive
        }
check context typex Semantic.Case {startPosition, scrutinee, cases} = do
  binder <- Unify.fresh Core.typex
  scrutinee <- check context binder scrutinee
  cases <- traverse (Alternative.check context typex binder) cases
  pure Case {startPosition, scrutinee, cases}
check context typex Semantic.Do {startPosition, dox} = do
  dox <- Do.check context typex dox
  pure Do {startPosition, dox}
check context typex Semantic.Lambda {startPosition, parameter, body} = do
  parameterType <- Unify.fresh Core.typex
  resultType <- Unify.fresh Core.typex
  -- What the function is to be is said before its parameter is looked at:
  -- matching on a constructor that says what type it makes tells nothing
  -- of a type that is not known yet.
  Unify.unify context startPosition typex (parameterType -#> resultType)
  parameter <- Pattern.check context parameterType parameter
  body <- Lambda.check (Pattern.augment parameter context) (shiftLogical resultType) body
  Pattern.sealed context parameter [parameterType, resultType]
  pure Lambda {startPosition, parameter, body}
check context typex Semantic.LambdaCase {startPosition, cases} = do
  parameterType <- Unify.fresh Core.typex
  resultType <- Unify.fresh Core.typex
  Unify.unify context startPosition typex (parameterType -#> resultType)
  cases <- traverse (Alternative.check context resultType parameterType) cases
  pure LambdaCase {startPosition, cases}
check context typex Semantic.RightSection {left, operatorPosition, right} = do
  argumentType1 <- Unify.fresh Core.typex
  argumentType2 <- Unify.fresh Core.typex
  result <- Unify.fresh Core.typex
  let target = argumentType1 -#> argumentType2 -#> result
  left <- CallHead.check context target left
  right <- check context argumentType2 right
  Unify.unify context operatorPosition typex (argumentType1 -#> result)
  pure RightSection {operatorPosition, left, right}
check context typex Semantic.LeftSection {operator, operatorPosition, operand} = do
  argumentType1 <- Unify.fresh Core.typex
  argumentType2 <- Unify.fresh Core.typex
  result <- Unify.fresh Core.typex
  let target = argumentType1 -#> argumentType2 -#> result
  operator <- CallHead.check context target operator
  operand <- check context argumentType1 operand
  Unify.unify context operatorPosition typex (argumentType2 -#> result)
  pure LeftSection {operatorPosition, operator, operand}
check context typex Semantic.Annotation {expression = Explicit expression, operatorPosition, annotation} = do
  annotation <- Annotation.checkAnnotation context annotation
  let annotation' = Forall.simplify annotation
  expression <- Definition4.checkAnnotation context operatorPosition annotation $
    \context typex -> solve <$> check context typex expression
  (typex', instanciation) <- instanciate context operatorPosition annotation'
  Unify.unify context operatorPosition typex typex'
  pure Annotation {expression, operatorPosition, annotation, instanciation}
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
            constraints = None,
            result = Core.Constructor Type2.ST # state # Shift0.map Shift0.Shift typex
          }
  imperative <- generalize context startPosition scheme imperative
  pure RunST {startPosition, imperative}
-- Only the checker makes these.
check _ _ Semantic.Generalized {} = error "an expression the resolver does not make"
check context typex Semantic.Negate {startPosition, negative} = do
  evidence <- Unify.constrain context startPosition Type2.Num typex
  negative <- check context typex negative
  pure Negate {startPosition, evidence, negative}

solve :: Expression s scope -> Unify.Solve s (Solved.Expression Group Check scope)
solve = \case
  CallHead {callHead} -> do
    callHead <- CallHead.solve callHead
    pure Solved.CallHead {callHead}
  Record {constructorPosition, constructor, constructorInfo, fields} -> do
    fields <- traverse Field.solve fields
    constructorInfo <- ConstructorInfo.solve constructorInfo
    pure
      Solved.Record
        { constructorPosition,
          constructor,
          constructorInfo = Solved constructorInfo,
          fields
        }
  Update {base, updatePosition, updateType, updates, updateInfo, permissive} -> do
    base <- solve base
    updates <- traverse Select.solve updates
    updateInfo <- UpdateInfo.solve updateInfo
    pure
      Solved.Update
        { base,
          updatePosition,
          updateType,
          updates,
          updateInfo = Solved updateInfo,
          permissive
        }
  List {startPosition, items} -> do
    items <- traverse solve items
    pure $ Solved.List {startPosition, items}
  Comprehension {startPosition, statements} -> do
    statements <- Comprehension.solve statements
    pure Solved.Comprehension {startPosition, statements}
  Call function argument -> do
    function <- solve function
    argument <- solve argument
    pure $ Solved.Call {function, argument}
  Let declarations body -> do
    declarations <- declarations
    body <- solve body
    pure $ Solved.Let {declarations, letBody = body}
  If condition true false -> do
    condition <- solve condition
    true <- solve true
    false <- solve false
    pure $ Solved.If {condition, thenx = true, elsex = false}
  Case {startPosition, scrutinee, cases} -> do
    scrutinee <- solve scrutinee
    cases <- traverse Alternative.solve cases
    pure Solved.Case {startPosition, scrutinee, cases}
  Lambda {startPosition, parameter, body} -> do
    parameter <- Pattern.solve parameter
    body <- Lambda.solve body
    pure $ Solved.Lambda {startPosition, parameter, body}
  LambdaCase {startPosition, cases} -> do
    cases <- traverse Alternative.solve cases
    pure Solved.LambdaCase {startPosition, cases}
  MultiwayIf branches -> do
    branches <- traverse RightHandSide.solve branches
    pure $ Solved.MultiwayIf {branches}
  Integer {startPosition, integer, evidence} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    pure Solved.Integer {startPosition, integer, evidence = Solved evidence}
  Float {startPosition, float, evidence} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    pure Solved.Float {startPosition, float, evidence = Solved evidence}
  String {startPosition, string} -> pure $ Solved.String {startPosition, string}
  Character {startPosition, character} -> pure $ Solved.Character {startPosition, character}
  Tuple {startPosition, elements} -> do
    elements <- traverse solve elements
    pure $ Solved.Tuple {startPosition, elements}
  Do {startPosition, dox} -> do
    dox <- Do.solve dox
    pure Solved.Do {startPosition, dox}
  Generalized {expression} -> do
    expression <- expression
    pure Solved.Generalized {expression = Known expression}
  RunST {startPosition, imperative} -> do
    imperative <- solve imperative
    pure Solved.RunST {startPosition, imperative}
  Annotation {expression, operatorPosition, annotation, instanciation} -> do
    expression <- expression
    instanciation <- Unify.solveEvidence operatorPosition instanciation
    pure
      Solved.Annotation
        { operatorPosition,
          expression = Known expression,
          annotation,
          instanciation = Solved instanciation
        }
  RightSection {operatorPosition, left, right} -> do
    left <- CallHead.solve left
    right <- solve right
    pure Solved.RightSection {operatorPosition, left, right}
  LeftSection {operatorPosition, operator, operand} -> do
    operator <- CallHead.solve operator
    operand <- solve operand
    pure Solved.LeftSection {operatorPosition, operator, operand}
  Negate {startPosition, evidence, negative} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    negative <- solve negative
    pure
      Solved.Negate
        { startPosition,
          evidence = Solved evidence,
          negative
        }
