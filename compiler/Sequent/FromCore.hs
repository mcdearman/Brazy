-- |
-- Core to the sequent calculus.
--
-- This follows "Generate.Go", the Javascript generator, case for case, so
-- that the two agree on what a program means. Where the Javascript
-- generator builds an expression, this builds a statement that hands its
-- value to a consumer.
--
-- The whole program is translated at once, and only the definitions that
-- the one it starts at can reach are kept.
module Sequent.FromCore (generate) where

import Control.Monad (replicateM)
import Control.Monad.ST (ST, runST)
import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.ConstructorInfo (ConstructorInfo (..))
import Core.Tree.Declaration (Declaration (..))
import Core.Tree.Declarations (Declarations (..))
import Core.Tree.EntryInfo (EntryInfo (EntryInfo))
import qualified Core.Tree.EntryInfo
import Core.Tree.Evidence (Evidence)
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Core.Tree.EvidenceSet (EvidenceSet (..))
import Core.Tree.Expression (Expression)
import qualified Core.Tree.Expression as Core (Expression (..))
import Core.Tree.Instance (Instance (..))
import Core.Tree.Instanciation (Instanciation, InstanciationF (Instanciation))
import qualified Core.Tree.Instanciation as Instanciation
import Core.Tree.MethodConcrete (MethodConcrete (..))
import Core.Tree.MethodInfo (MethodInfo (..))
import qualified Core.Tree.Module as Core (Module (..))
import Core.Tree.Statements (Statements)
import qualified Core.Tree.Statements as Statements (Statements (..))
import Core.Tree.TypeLambda (TypeLambda, TypeLambdaOver (..))
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import qualified Generate.Binding.Evidence as Evidence (Binding (..))
import qualified Generate.Binding.Term as Term (Binding (..))
import qualified Generate.Binding.Type as Type
import Generate.Context (Context, fresh, singleBinding, (!-), (!=.), (!~))
import qualified Generate.Context as Context
import qualified Generate.Global as Global (Global)
import qualified Generate.GlobalType as GlobalType
import Generate.Go.Module (precontext)
import qualified Generate.Info as Info
import Generate.LocalType (LocalType (LocalType))
import qualified Generate.LocalType as LocalType
import Generate.Precontext (Precontext (Precontext))
import qualified Generate.Precontext as Precontext
import qualified Generate.Variable as Variable (Variable (..))
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Index.Evidence (Index (Direct))
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term as Term (Index)
import Semantic.Index.Type2 (Index (Index, Tuple))
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope
import Sequent.Primitive (Primitive (Field))
import qualified Sequent.Runtime as Runtime
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Literal (..),
    Producer (..),
    Program (..),
    Statement (..),
    Strategy (..),
    globals,
  )

-- |
-- The program that runs a definition, which must be an action.
generate :: Global.Global -> Vector Core.Module -> Program
generate start modules =
  Program
    { definitions = reach Map.empty (globals entry),
      entry
    }
  where
    entry = Cut (Global start) (Force (Apply [] (MuTilde ignored Halt)))
    ignored = Binder {name = pack "_", strategy = ByNeed}
    everything = Map.unions $ toList $ Vector.imap (moduleDefinitions (precontext modules)) modules
    find global
      | Just definition <- Runtime.definition global = definition
      | Just definition <- Map.lookup global everything = definition
      | otherwise = error $ "undefined global: " ++ Runtime.showGlobal global
    reach :: Map Global.Global Producer -> Set Global.Global -> Map Global.Global Producer
    reach known wanted = case Set.minView (Set.filter (`Map.notMember` known) wanted) of
      Nothing -> known
      Just (global, wanted) ->
        let definition = find global
         in reach (Map.insert global definition known) (wanted <> globals definition)

-- |
-- Every definition of a module. The map is lazy in its values, so only the
-- definitions that are looked at are translated.
moduleDefinitions :: Precontext -> Int -> Core.Module -> Map Global.Global Producer
moduleDefinitions
  pre@Precontext {terms = termNames, types = typeNames}
  moduleIndex
  Core.Module
    { name,
      declarations = Declarations {terms, classInstances, dataInstances}
    }
    | name == Runtime.hazy = Map.empty
    | otherwise = Map.fromList $ concat [termDefinitions, classDefinitions, dataDefinitions]
    where
      termDefinitions = do
        (termIndex, Declaration {definition}) <- zip [0 ..] (toList terms)
        let global = termNames Vector.! moduleIndex Vector.! termIndex
        pure (global, runST (globalDeclaration pre definition))
      classDefinitions = do
        (typeIndex, instances) <- zip [0 ..] (toList classInstances)
        (target, instancex) <- Map.toList instances
        let global = typeNames Vector.! moduleIndex Vector.! typeIndex `GlobalType.indexClass` target
        pure (global, runST (globalInstance pre instancex))
      dataDefinitions = do
        (typeIndex, instances) <- zip [0 ..] (toList dataInstances)
        (target, instancex) <- Map.toList instances
        let global = typeNames Vector.! moduleIndex Vector.! typeIndex `GlobalType.indexData` target
        pure (global, runST (globalInstance pre instancex))

globalDeclaration :: Precontext -> TypeLambda Scope.Global -> ST s Producer
globalDeclaration pre definition = do
  context <- Context.start pre
  declaration context definition

globalInstance :: Precontext -> Instance Scope.Global -> ST s Producer
globalInstance pre instancex = do
  context <- Context.start pre
  generateInstance context instancex

byValue, byNeed :: Text -> Binder
byValue name = Binder {name, strategy = ByValue}
byNeed name = Binder {name, strategy = ByNeed}

-- |
-- What a term variable is, and whether it is known to hold a value.
variable :: Context s scope -> Term.Index scope -> (Producer, Bool)
variable context index = case context !- index of
  Term.Binding {name = Variable.Local name, strict} -> (Variable name, strict)
  Term.Binding {name = Variable.Global global, strict} -> (Global global, strict)

-- |
-- Use a consumer more than once: if it is not a name, give it one.
named :: Context s scope -> Consumer -> (Consumer -> ST s Statement) -> ST s Statement
named context consumer continue = case consumer of
  Covariable {} -> continue consumer
  _ -> do
    name <- fresh context
    body <- continue (Covariable name)
    pure $ Cut (Mu name body) consumer

-- |
-- Use a variable or call a function of evidence.
implicit :: Context s scope -> Bool -> Producer -> Instanciation scope -> Consumer -> ST s Statement
implicit _ False producer Instanciation.Mono consumer = pure $ Cut producer (Force consumer)
implicit _ True producer Instanciation.Mono consumer = pure $ Cut producer consumer
implicit context _ producer (Instanciation arguments) consumer =
  generateEvidences context (toList arguments) $ \arguments ->
    pure $ Cut producer (Apply arguments consumer)

generateEvidences ::
  Context s scope ->
  [Evidence scope] ->
  ([Producer] -> ST s Statement) ->
  ST s Statement
generateEvidences context list continue = case list of
  [] -> continue []
  head : tail -> generateEvidence context head $ \head ->
    generateEvidences context tail $ \tail -> continue (head : tail)

-- |
-- Evaluate evidence. What the continuation is given is a value.
generateEvidence ::
  Context s scope ->
  Evidence scope ->
  (Producer -> ST s Statement) ->
  ST s Statement
generateEvidence context = \case
  Evidence.Variable {variable = Direct classx (Tuple number), instanciation}
    | Just global <- Runtime.tuple classx number -> \continue -> case instanciation of
        Instanciation arguments ->
          generateEvidences context (toList arguments) $ \arguments -> do
            name <- fresh context
            body <- continue (Variable name)
            pure $ Cut (Global global) (Apply arguments (MuTilde (byValue name) body))
        Instanciation.Mono -> error "bad tuple instance"
  Evidence.Variable {variable, instanciation} -> \continue -> do
    let fromBinding = \case
          Variable.Local name -> Variable name
          Variable.Global global -> Global global
        (producer, strict) = case variable of
          Evidence.Direct target (Index index)
            | Type.Binding {dataInstances} <- context !=. index,
              Just binding <- Map.lookup target dataInstances ->
                (fromBinding binding, False)
          Evidence.Direct (Index index) target
            | Type.Binding {classInstances} <- context !=. index,
              Just binding <- Map.lookup target classInstances ->
                (fromBinding binding, False)
          Evidence.Index index
            | Evidence.Binding name <- context !~ index ->
                (Variable name, True)
          Evidence.Direct classx typex -> (Global (Runtime.builtin classx typex), False)
    case instanciation of
      Instanciation.Mono
        | strict -> continue producer
        | otherwise -> do
            name <- fresh context
            body <- continue (Variable name)
            pure $ Cut producer (Force (MuTilde (byValue name) body))
      Instanciation arguments ->
        generateEvidences context (toList arguments) $ \arguments -> do
          name <- fresh context
          body <- continue (Variable name)
          pure $ Cut producer (Apply arguments (MuTilde (byValue name) body))
  Evidence.Super {base, index} -> \continue ->
    generateEvidence context base $ \base -> do
      name <- fresh context
      body <- continue (Variable name)
      pure $ Primitive (Field index) [base] (MuTilde (byValue name) body)

lambda :: Context s scope -> Expression (Scope.SimpleDeclaration ':+ scope) -> ST s Producer
lambda context body = do
  name <- fresh context
  result <- fresh context
  body <- evaluate (singleBinding name context) body (Covariable result)
  pure $ Function [byNeed name] result body

-- |
-- Something that stands for an expression without evaluating it, and that
-- other things in a binding group can refer to: a function or a thunk.
object :: Context s scope -> Expression scope -> ST s Producer
object context = \case
  Core.Lambda {body} -> lambda context body
  Core.Newtype {argument} -> object context argument
  expression -> do
    result <- fresh context
    body <- evaluate context expression (Covariable result)
    pure $ Thunk result body

-- |
-- Something that stands for an expression without evaluating it. Unlike
-- 'object', this can be a variable, a literal or a constructor.
delayed :: Context s scope -> Expression scope -> ST s Producer
delayed context = \case
  Core.Variable {variable = index, instanciation = Instanciation.Mono} ->
    pure $ fst $ variable context index
  Core.Constructor
    { constructor = Constructor.Index {constructorIndex},
      arguments,
      constructorInfo = ConstructorInfo {entries}
    }
      | all lazy entries -> do
          arguments <- traverse (delayed context) (toList arguments)
          pure $ Constructor constructorIndex arguments
  Core.Character {character} -> pure $ Literal $ Int $ fromIntegral $ ord character
  Core.Integer {integer} -> pure $ Literal $ Integer integer
  Core.Newtype {argument} -> delayed context argument
  expression -> object context expression
  where
    lazy EntryInfo {strict} = not (Info.strict strict)

-- |
-- Evaluate an expression. What the consumer is given is a value.
evaluate :: Context s scope -> Expression scope -> Consumer -> ST s Statement
evaluate context expression consumer = case expression of
  Core.Variable {variable = index, instanciation} -> do
    let (producer, strict) = variable context index
    implicit context strict producer instanciation consumer
  Core.Method
    { method = Method.Index {methodIndex},
      evidence = dictionary,
      instanciation,
      methodInfo = MethodInfo {constraintCount}
    } -> generateEvidence context dictionary $ \dictionary -> do
      name <- fresh context
      body <- implicit context False (Variable name) instanciation consumer
      pure $ Primitive (Field (methodIndex + constraintCount)) [dictionary] (MuTilde (byNeed name) body)
  Core.Selector
    { selector = Selector.Index {selectorIndex},
      argument,
      selectorInfo = EntryInfo {strict}
    } -> do
      name <- fresh context
      let field
            | Info.strict strict = consumer
            | otherwise = Force consumer
      evaluate context argument $
        MuTilde (byValue name) (Primitive (Field selectorIndex) [Variable name] field)
  Core.Constructor
    { constructor = Constructor.Index {constructorIndex},
      arguments,
      constructorInfo = ConstructorInfo {entries}
    } -> do
      let go [] continue = continue []
          go ((EntryInfo {strict}, argument) : rest) continue
            | Info.strict strict = do
                name <- fresh context
                body <- go rest (continue . (Variable name :))
                evaluate context argument (MuTilde (byValue name) body)
            | otherwise = do
                argument <- delayed context argument
                go rest (continue . (argument :))
      go (zip (toList entries) (toList arguments)) $ \fields ->
        pure $ Cut (Constructor constructorIndex fields) consumer
  Core.Integer {integer} -> pure $ Cut (Literal $ Integer integer) consumer
  Core.Character {character} ->
    pure $ Cut (Literal $ Int $ fromIntegral $ ord character) consumer
  Core.Let {declarations, letBody} -> do
    (context, bindings) <- generateDeclarations context declarations
    body <- evaluate context letBody consumer
    pure $ letRec bindings body
  Core.Lambda {body} -> do
    function <- lambda context body
    pure $ Cut function consumer
  Core.Call {function, argument} -> do
    argument <- delayed context argument
    evaluate context function (Apply [argument] consumer)
  Core.Join {statements} ->
    named context consumer $ \consumer -> attempt context statements consumer Abort
  Core.Newtype {argument} -> evaluate context argument consumer

letRec :: [(Text, Producer)] -> Statement -> Statement
letRec [] body = body
letRec bindings body = LetRec bindings body

-- |
-- Try the statements. The consumer is given the value of the first branch
-- whose patterns all match, and the failure statement runs if there is none.
-- The consumer must be a name and the failure statement must be small,
-- because both are copied into every branch.
attempt :: Context s scope -> Statements scope -> Consumer -> Statement -> ST s Statement
attempt context statements success failure = case statements of
  Statements.Done {done} -> evaluate context done success
  Statements.Bind
    { constructor = Constructor.Index {constructorIndex},
      constructorInfo = constructorInfo@ConstructorInfo {entries},
      check,
      thenx
    } -> do
      names <- replicateM (length entries) (fresh context)
      let binders = zipWith binder names (toList entries)
          binder name EntryInfo {strict}
            | Info.strict strict = byValue name
            | otherwise = byNeed name
      thenx <-
        attempt
          (Context.patternBindings (Strict.Vector.fromList names) constructorInfo context)
          thenx
          success
          failure
      evaluate context check $
        Case [Alternative constructorIndex binders thenx] (Just failure)
  Statements.Let {declarations, letBody} -> do
    (context, bindings) <- generateDeclarations context declarations
    body <- attempt context letBody success failure
    pure $ letRec bindings body
  Statements.LetOne {declaration = value, body} -> do
    value <- delayed context value
    name <- fresh context
    body <- attempt (singleBinding name context) body success failure
    pure $ Cut value (MuTilde (byNeed name) body)
  Statements.Branch {left, right} -> do
    right <- attempt context right success failure
    case right of
      Abort -> attempt context left success right
      Cut (Literal _) (Covariable _) -> attempt context left success right
      _ -> do
        -- The right branch is where the left one goes when it fails. Name it
        -- so that it is not copied.
        label <- fresh context
        ignored <- fresh context
        left <- attempt context left success (Cut (Literal $ Int 0) (Covariable label))
        pure $ Cut (Mu label left) (MuTilde (byValue ignored) right)
  Statements.Bottom -> pure failure

-- |
-- A declaration: a function of its evidence if it has constraints, and
-- otherwise a function or a thunk.
declaration :: Context s scope -> TypeLambda scope -> ST s Producer
declaration context TypeLambdaOver {constraints, result = expression} = do
  let count = case constraints of
        Constraints constraints -> length constraints
        None -> 0
  names <- Vector.replicateM count (fresh context)
  context <- pure $ Context.evidenceBindings names context
  case constraints of
    None -> object context expression
    Constraints {} -> do
      result <- fresh context
      body <- evaluate context expression (Covariable result)
      pure $ Function (byValue <$> toList names) result body

generateDeclarations ::
  Context s scope ->
  Declarations (Scope.Declaration ':+ scope) ->
  ST s (Context s (Scope.Declaration ':+ scope), [(Text, Producer)])
generateDeclarations context Declarations {terms, classInstances, dataInstances} = do
  variables <- Vector.replicateM (length terms) (fresh context)
  classVariables <- traverse (traverse $ const $ fresh context) classInstances
  dataVariables <- traverse (traverse $ const $ fresh context) dataInstances
  let instances = Vector.zipWith localType classVariables dataVariables
      localType classInstances dataInstances =
        LocalType {classInstances, dataInstances}
  context <- pure $ Context.localBindings variables instances context
  termBindings <- for (zip (toList variables) (toList terms)) $
    \(name, Declaration {definition}) -> do
      value <- declaration context definition
      pure (name, value)
  let instanceBindings variables instances =
        for (zip (toList variables) (toList instances)) $ \(names, instances) ->
          for (Map.toList instances) $ \(index, instancex) -> do
            value <- generateInstance context instancex
            pure (names Map.! index, value)
  classBindings <- instanceBindings classVariables classInstances
  dataBindings <- instanceBindings dataVariables dataInstances
  pure (context, termBindings ++ concat classBindings ++ concat dataBindings)

-- |
-- An instance: a constructed value of the evidence for its superclasses and
-- then its methods. It is a thunk, or a function of what it requires.
generateInstance :: Context s scope -> Instance scope -> ST s Producer
generateInstance context Instance {evidence = EvidenceSet supers, prerequisites, members} = do
  let count = case prerequisites of
        Constraints count -> length count
        None -> 0
  names <- Vector.replicateM count (fresh context)
  context <- pure $ Context.evidenceBindings names context
  result <- fresh context
  body <- generateEvidences context (toList supers) $ \supers -> do
    members <- for (toList members) $ \Definition {definition} ->
      declaration context definition
    pure $ Cut (Constructor 0 (supers ++ members)) (Covariable result)
  pure $ case prerequisites of
    None -> Thunk result body
    Constraints {} -> Function (byValue <$> toList names) result body
