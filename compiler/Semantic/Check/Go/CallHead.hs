module Semantic.Check.Go.CallHead where

import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import qualified Core.Tree.Class as Class
import Core.Tree.Class (EvidenceInfo (..))
import Core.Tree.Type ((-#>))
import Core.Tree.TypeDeclaration (assumeClass, assumeData)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Check.Instanciate.Class as ClassInstance
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import Semantic.Check.Context (Context (..))
import Semantic.Check.Instanciate.Data (DataInstance (..))
import qualified Semantic.Check.Instanciate.Data as DataInstance
import qualified Semantic.Check.Instanciate.Entry as EntryInstance (taken)
import qualified Semantic.Check.Simple.Data as Simple.Data
import Semantic.Check.Scheme (instanciate)
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Check.Temporary.SelectorInfo as SelectorInfo
import Semantic.Check.Binding.Term (TermBinding (..), Type (..))
import qualified Semantic.Check.Binding.Type as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Table.Term as Term ((!))
import qualified Semantic.Index.Table.Type as Type
import qualified Semantic.Index.Term as Term (Index)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.CallHead as Semantic
import qualified Semantic.Tree.CallHead as Solved
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.CallHead layout Resolve scope ->
  ST s (Unify.Solve s (Solved.CallHead layout' Check scope))
check context@Context {termEnvironment} typex Semantic.Variable {variablePosition, variable = index} = do
  let TermBinding binding = termEnvironment Term.! index
  binding >>= \case
    Wobbly scheme -> do
      (typex', instanciation) <- Unify.instanciate context variablePosition scheme
      Unify.unify context variablePosition typex typex'
      pure $ variable context variablePosition index instanciation
    Rigid scheme -> do
      (typex', instanciation) <- instanciate context variablePosition scheme
      Unify.unify context variablePosition typex typex'
      pure $ variable context variablePosition index instanciation
check context@Context {typeEnvironment} typex Semantic.Constructor {constructorPosition, constructor = index} =
  do
    let Constructor.Index typeIndex constructorIndex = index
    datax <- do
      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
      Builtin.index pure get typeIndex
    DataInstance {constructors} <-
      Simple.Data.instanciate ConstructorInstance.Build context constructorPosition datax
    let instancex = constructors Strict.Vector.! constructorIndex
        base = DataInstance.constructed instancex typeIndex
        typex' = ConstructorInstance.function instancex base
    ConstructorInstance.mark context instancex
    Unify.unify context constructorPosition typex typex'
    constructorInfo <- ConstructorInstance.built context instancex
    pure $ constructor context constructorPosition index constructorInfo
check
  context@Context {typeEnvironment}
  typex
  Semantic.Selector {selectorPosition, selector} = do
    let Selector.Index typeIndex selectorIndex = selector
    datax <- do
      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
      Builtin.index pure get typeIndex
    instancex <- Simple.Data.instanciateMark context selectorPosition datax
    let selectorInfo = DataInstance.selectorInfo instancex selectorIndex
    selected <- EntryInstance.taken context selectorPosition (DataInstance.selectorEntry instancex selectorIndex)
    let typex' = DataInstance.selected instancex typeIndex selectorIndex -#> selected
    Unify.unify context selectorPosition typex typex'
    pure $ do
      selectorInfo <- SelectorInfo.solve context selectorInfo
      pure
        Solved.Selector
          { selectorPosition,
            selector,
            selectorInfo = Solved selectorInfo
          }
check
  context@Context {typeEnvironment}
  typex
  Semantic.Method {methodPosition, method} = do
    case method of
      Method.Index typeIndex methodIndex -> do
        classx <- do
          let get index = assumeClass <$> TypeBinding.content (typeEnvironment Type.! index)
          Builtin.index pure get typeIndex
        instancex <- ClassInstance.instanciate context methodPosition typeIndex classx
        let function = ClassInstance.methodFunction instancex methodIndex
            methodInfo = ClassInstance.info instancex
            EvidenceInfo evidence = Class.evidence instancex
        (typex', instanciation) <- Unify.instanciate context methodPosition function
        Unify.unify context methodPosition typex typex'
        pure $ do
          evidence <- Unify.solveEvidence context methodPosition evidence
          instanciation <- Unify.solveEvidence context methodPosition instanciation
          pure
            Solved.Method
              { methodPosition,
                method,
                evidence = Solved evidence,
                instanciation = Solved instanciation,
                methodInfo = Solved methodInfo
              }

-- | A variable, used as the type it is given.
variable ::
  Context s scope ->
  Position ->
  Term.Index scope ->
  Unify.Instanciation s scope ->
  Unify.Solve s (Solved.CallHead layout Check scope)
variable context variablePosition variable instanciation = do
  instanciation <- Unify.solveEvidence context variablePosition instanciation
  pure
    Solved.Variable
      { variablePosition,
        variable,
        instanciation = Solved instanciation
      }

-- | A constructor, made into what it makes.
constructor ::
  Context s scope ->
  Position ->
  Constructor.Index scope ->
  ConstructorInfo s scope ->
  Unify.Solve s (Solved.CallHead layout Check scope)
constructor context constructorPosition constructor constructorInfo = do
  constructorInfo <- ConstructorInfo.solve context constructorInfo
  pure
    Solved.Constructor
      { constructorPosition,
        constructor,
        constructorInfo = Solved constructorInfo
      }
