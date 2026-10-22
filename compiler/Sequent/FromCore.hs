-- |
-- Core to the sequent calculus.
--
-- This was written after the Javascript generator there used to be, case
-- for case, so that the two agreed on what a program means. Where that
-- built an expression, this builds a statement that hands its value to a
-- consumer.
--
-- The whole program is translated at once, and only the definitions that
-- the one it starts at can reach are kept.
module Sequent.FromCore (generate, complete, definitionOf, Unit (..), Unshared (..), unit, reached, start) where

import Control.Monad (replicateM)
import Control.Monad.ST (ST, runST)
import qualified Core.Foreign as Foreign
import Core.Tree.Constraints (ConstraintsF (..))
import Core.Tree.ConstructorInfo (ConstructorInfo (..))
import Core.Tree.Declaration (Declaration (..))
import Core.Tree.Declarations (Declarations (..))
import qualified Core.Tree.Declarations as Declarations (none)
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
import Data.Ratio ((%))
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text (takeWhile)
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import qualified Generate.Binding.Evidence as Evidence (Binding (..))
import qualified Generate.Binding.Term as Term (Binding (..))
import qualified Generate.Binding.Type as Type
import Generate.Context (Context, fresh, singleBinding, (!-), (!=.), (!~))
import qualified Generate.Context as Context
import qualified Generate.Global as Global (Global (..))
import qualified Generate.GlobalType as GlobalType
import qualified Generate.Info as Info
import qualified Generate.Mangle as Mangle (mangle)
import Generate.LocalType (LocalType (LocalType))
import qualified Generate.LocalType as LocalType
import Generate.Precontext (Precontext (Precontext), precontext)
import qualified Generate.Precontext as Precontext
import qualified Generate.Variable as Variable (Variable (..))
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Index.Evidence (Index (Direct))
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term as Term (Index)
import Semantic.Index.Type2 (Index (Index, Tuple))
import qualified Semantic.Index.Type2 as Type2 (Index (..))
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope
import Sequent.Primitive (Primitive (Export, Field))
import Sequent.Borrow (Lending, lending)
import Sequent.Hoist (Unshared (..), hoist)
import Sequent.Inline (copy, inline, isCopy, specialize)
import qualified Sequent.Uncurry as Uncurry (rewrite, shape, split, whole)
import qualified Sequent.Runtime as Runtime
import Sequent.Simplify (simplify)
import Sequent.Strict (demand, strengthen, valued)
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
    int63,
    separator,
  )

-- |
-- The program that runs a definition, which must be an action.
generate :: Global.Global -> Vector Core.Module -> Program
generate entry modules = fst (complete entry modules)

-- |
-- The same, with which parameters each definition only reads, for where
-- native code is generated: see "Sequent.Borrow".
complete :: Global.Global -> Vector Core.Module -> (Program, Global.Global -> Maybe Lending)
complete entry modules =
  ( Program
      { definitions = reach Map.empty (globals (start entry) <> Set.fromList (exportsOf modules [0 .. Vector.length modules - 1])),
        entry = start entry
      },
    lends
  )
  where
    (family, lends) = tablesOf modules
    find global
      | Just found <- family (top global) >>= Map.lookup global = found
      | otherwise = error $ "undefined global: " ++ Runtime.showGlobal global
    reach :: Map Global.Global Producer -> Set Global.Global -> Map Global.Global Producer
    reach known wanted = case Set.minView (Set.filter (`Map.notMember` known) wanted) of
      Nothing -> known
      Just (global, wanted) ->
        let found = find global
         in reach (Map.insert global found known) (wanted <> globals found)

-- |
-- The definitions of some of the modules that C calls by a name, as foreign
-- exports say. Nothing in the program may use them, and they are kept.
exportsOf :: Vector Core.Module -> [Int] -> [Global.Global]
exportsOf modules indexes = do
  let Precontext {terms = termNames} = precontext modules
  index <- indexes
  let Core.Module {name, declarations = Declarations {terms}} = modules Vector.! index
  -- The module of primitives has only what they are said to be.
  if name == Runtime.hazy then [] else pure ()
  (termIndex, Declaration {definition = TypeLambdaOver {result = Core.ForeignExport {}}}) <- zip [0 ..] (toList terms)
  pure (termNames Vector.! index Vector.! termIndex)

-- |
-- What a name is defined as, anywhere in a program. Given the modules alone
-- this is a table, which is worked out once and is lazy in what it holds.
definitionOf :: Vector Core.Module -> Global.Global -> Producer
definitionOf modules = find
  where
    family = familyOf modules
    find global
      | Just found <- family (top global) >>= Map.lookup global = found
      | otherwise = error $ "undefined global: " ++ Runtime.showGlobal global

-- |
-- The definition at the top of a module that a definition came out of.
-- Its name is the first part of theirs.
top :: Global.Global -> Global.Global
top global@Global.Global {name} = global {Global.name = Text.takeWhile (/= separator) name}

-- |
-- A definition at the top of a module as it is compiled: itself, the
-- functions that were taken out of it, and the entries of each that take
-- every argument together. They are all called by its name and then their
-- own, so any of them is found through it.
--
-- There is a table for each module, by the module's name. A module's table
-- is only made when something of the module is asked for: making it has the
-- module checked, and most of a program's packages are never used.
familyOf :: Vector Core.Module -> Global.Global -> Maybe (Map Global.Global Producer)
familyOf modules = fst (tablesOf modules)


tablesOf ::
  Vector Core.Module ->
  (Global.Global -> Maybe (Map Global.Global Producer), Global.Global -> Maybe Lending)
tablesOf modules = (members, borrowed)
  where
    Tables {members, borrowed} = tables modules

-- |
-- What is worked out of some modules for the native pipeline, each a table
-- that is lazy in what it holds.
data Tables = Tables
  { -- | A definition at the top of a module as it is compiled, with what was
    -- taken out of it.
    members :: Global.Global -> Maybe (Map Global.Global Producer),
    -- | Which parameters a definition only reads.
    borrowed :: Global.Global -> Maybe Lending,
    -- | The values of a definition at the top of a module that are no
    -- longer shared with themselves: see "Sequent.Hoist".
    lost :: Global.Global -> [Unshared],
    -- | How many arguments a definition takes at once, if it is a function.
    taken :: Global.Global -> Maybe Int
  }

tables :: Vector Core.Module -> Tables
tables modules = Tables {members = family, borrowed = lends, lost = unshared, taken}
  where
    -- A definition is asked as it is in the end, when a call of it with
    -- every argument has been sent to the entry that takes them together.
    taken global = case family (top global) >>= Map.lookup global of
      Just (Function parameters _ _) -> Just (length parameters)
      _ -> Nothing
    pre = precontext modules
    -- What each definition is as it comes from Core.
    written = Map.fromList $ do
      (index, modulex@Core.Module {name}) <- zip [0 ..] (toList modules)
      pure (name, moduleDefinitions pre index modulex)
    -- With the functions inside it taken out.
    lifted global found =
      let (outer, inner, _) = hoist global found
       in Map.insert global outer inner
    -- The values of a definition that are no longer shared with themselves.
    unshared global@Global.Global {path} =
      case Map.lookup path written >>= Map.lookup global of
        Just found -> let (_, _, lost) = hoist global found in lost
        Nothing -> []
    hoisted = Map.map (Map.mapWithKey lifted) written
    liftedOf global@Global.Global {path}
      | Just found <- Runtime.definition global = Just (lifted global found)
      | otherwise = Map.lookup path hoisted >>= Map.lookup global
    -- How many arguments a definition takes, which is asked of it as it is
    -- before any of its calls are changed.
    known global = liftedOf (top global) >>= Map.lookup global >>= Uncurry.shape
    -- With the calls that have every argument sent to the entry for them.
    finish members = Map.unions $ do
      (global, found) <- Map.toList members
      let (outer, entry) = Uncurry.split global found
      pure $
        Map.fromList $
          (global, simplify (Uncurry.rewrite known outer))
            : [(name, simplify (Uncurry.rewrite known inner)) | Just (name, inner) <- [entry]]
    finished = Map.map (Map.map finish) hoisted
    familyIn table global@Global.Global {path}
      | Just found <- Runtime.definition global = Just (finish (lifted global found))
      | otherwise = Map.lookup path table >>= Map.lookup global
    memberIn table global = familyIn table (top global) >>= Map.lookup global
    -- With the small functions each calls put where they are called, which
    -- is done a few times over: each time reaches one call further.
    -- The fields of the instances that are known are read as what they
    -- are, before and after, and the calls that then give every argument
    -- to a definition that is known are sent to the entry for them. A
    -- function that calls itself and is given an instance that is known, or
    -- a function written where it is called, is copied for it: see
    -- 'copy'. A copy is a definition of the module it is made in, and of no
    -- other family.
    improve before time =
      let look = memberIn before
          improved global found =
            simplify . Uncurry.rewrite known . specialize look . simplify $
              inline time look global (specialize look found)
          improving definitions =
            let made = Map.mapWithKey (\top members -> copy time look top (Map.mapWithKey improved members)) definitions
                copies = Map.unions (map snd (Map.elems made))
             in Map.union (Map.map fst made) (Map.mapWithKey Map.singleton copies)
       in Map.map improving before
    inlined = foldl improve finished [1 .. 4]
    -- Whether calling a definition with every argument runs an action,
    -- which it does when one of the functions it is made of takes none.
    acts global = maybe False (elem 0) (known (Uncurry.whole global))
    -- With what is certain to be evaluated evaluated at once. This is done
    -- twice over as well: what a function is certain to force is worked out
    -- from what the functions it calls were found to force the time before.
    strict (before, others) time =
      let demands = Map.map (Map.map (Map.mapWithKey (demand others acts))) before
          demanded global@Global.Global {path}
            | Just _ <- Runtime.definition (top global) =
                memberIn before global >>= demand (const Nothing) acts global
            | otherwise = Map.lookup path demands >>= Map.lookup (top global) >>= Map.lookup global >>= id
          stronger found = simplify (strengthen time demanded acts found)
       in (Map.map (Map.map (Map.map stronger)) before, demanded)
    (strengthened, demanded) = foldl strict (inlined, const Nothing) [1, 2 :: Int]
    -- An entry that takes every argument is only got to by a call that
    -- names its definition, and each of those now evaluates first what the
    -- entry is certain to force. So the entry takes those as values.
    settle global found
      | Uncurry.whole global /= global,
        Just wanted <- demanded global,
        or wanted =
          simplify (strengthen 4 demanded acts (valued wanted found))
      | otherwise = found
    settled = Map.map (Map.map (Map.mapWithKey settle)) strengthened
    -- The definitions that are written in the sequent calculus call the
    -- others too, and have to call them the same way.
    family global@Global.Global {path}
      | Just found <- Runtime.definition global =
          let stronger = simplify . strengthen 3 demanded acts
           in Just (Map.mapWithKey settle (Map.map stronger (finish (lifted global found))))
      | otherwise = Map.lookup path settled >>= Map.lookup global
    -- Which parameters each only reads. This is also done twice over, so
    -- that a function that passes a parameter on to one that only reads it
    -- is found to only read it too.
    reading others =
      let table = Map.map (Map.map (Map.mapWithKey (lending others))) settled
       in \global@Global.Global {path} ->
            if null (Runtime.definition (top global))
              then Map.lookup path table >>= Map.lookup (top global) >>= Map.lookup global >>= id
              else family (top global) >>= Map.lookup global >>= lending (const Nothing) global
    lends = reading (reading (const Nothing))

-- |
-- The statement that runs an action and ends the program.
start :: Global.Global -> Statement
start entry = Cut (Global entry) (Force (Apply [] (MuTilde ignored Halt)))
  where
    ignored = Binder {name = pack "_", strategy = ByNeed}

-- |
-- Some modules compiled by themselves, to be linked with others later.
data Unit = Unit
  { -- | Every definition of the modules.
    own :: Map Global.Global Producer,
    -- | The definitions that are made where they are used, which another
    -- unit may have made as well.
    shared :: Map Global.Global Producer,
    -- | The values in the unit's own definitions that are no longer shared
    -- with themselves, to be warned of.
    unshared :: [Unshared],
    -- | Which parameters a definition only reads, whether it is this
    -- unit's or another's: see "Sequent.Borrow". A unit that calls a
    -- definition and the unit that has it are told the same.
    lends :: Global.Global -> Maybe Lending,
    -- | How many arguments a definition takes at once, if it is a function,
    -- whether it is this unit's or another's. A call that gives that many
    -- can go to the definition's code, and has no need to ask the
    -- definition where its code is.
    arities :: Global.Global -> Maybe Int,
    -- | The unit's own definitions that C calls by a name, which are kept
    -- in a program though nothing in it uses them.
    exports :: [Global.Global]
  }

-- |
-- A unit that is a program, with only what the program gets to from where
-- it starts. Nothing outside a program uses what is in it, so the rest is
-- never run: most of it is instances that are derived and never asked for.
reached :: Statement -> Unit -> Unit
reached entry unit@Unit {own, shared, exports} =
  unit {own = Map.restrictKeys own wanted, shared = Map.restrictKeys shared wanted}
  where
    wanted = go Set.empty (Set.toList (globals entry) ++ exports)
    go seen = \case
      [] -> seen
      global : rest
        | Set.member global seen -> go seen rest
        | Just found <- Map.lookup global own -> go (Set.insert global seen) (Set.toList (globals found) ++ rest)
        | Just found <- Map.lookup global shared -> go (Set.insert global seen) (Set.toList (globals found) ++ rest)
        -- Some other unit's.
        | otherwise -> go (Set.insert global seen) rest

-- |
-- The modules at some places among all of the modules, as a unit. What they
-- use of other modules is left as names.
unit :: [Int] -> Vector Core.Module -> Unit
unit indexes modules =
  Unit
    { own,
      shared = reach Map.empty (globals (Map.elems own)),
      unshared = concatMap lost names,
      lends,
      arities,
      exports = exportsOf modules indexes
    }
  where
    pre = precontext modules
    Tables {members = family, borrowed = lends, lost, taken = arities} = tables modules
    -- The names of every definition at the top of the unit's modules.
    names = do
      index <- indexes
      let modulex@Core.Module {name} = modules Vector.! index
      -- The module of primitives is written out in the sequent calculus.
      if name == Runtime.hazy
        then Map.keys Runtime.fixed
        else Map.keys (moduleDefinitions pre index modulex)
    own = Map.unions $ do
      name <- names
      case family name of
        Just members -> [members]
        Nothing -> error $ "undefined global: " ++ Runtime.showGlobal name
    reach :: Map Global.Global Producer -> Set Global.Global -> Map Global.Global Producer
    reach known wanted = case Set.minView (Set.filter (`Map.notMember` known) wanted) of
      Nothing -> known
      Just (global, wanted)
        -- A copy of a function is made wherever it is used too.
        | not (null (Runtime.generated (top global))) || isCopy global,
          Just members <- family (top global) ->
            reach (Map.union members known) (wanted <> globals (Map.elems members))
        | otherwise -> reach known wanted

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
      -- An instance, and its methods as definitions of their own if it
      -- needs nothing.
      instanceDefinitions global instancex@Instance {prerequisites, members} = case prerequisites of
        None ->
          (global, runST (globalInstance pre (Just global) instancex)) : do
            (index, method) <- zip [0 ..] (toList members)
            pure (Runtime.member global index, runST (globalMember pre method))
        Constraints {} -> [(global, runST (globalInstance pre Nothing instancex))]
      termDefinitions = do
        (termIndex, Declaration {definition}) <- zip [0 ..] (toList terms)
        let global = termNames Vector.! moduleIndex Vector.! termIndex
        pure (global, runST (globalDeclaration pre global definition))
      classDefinitions = do
        (typeIndex, instances) <- zip [0 ..] (toList classInstances)
        (target, instancex) <- Map.toList instances
        let global = typeNames Vector.! moduleIndex Vector.! typeIndex `GlobalType.indexClass` target
        instanceDefinitions global instancex
      dataDefinitions = do
        (typeIndex, instances) <- zip [0 ..] (toList dataInstances)
        (target, instancex) <- Map.toList instances
        let global = typeNames Vector.! moduleIndex Vector.! typeIndex `GlobalType.indexData` target
        instanceDefinitions global instancex

globalDeclaration :: Precontext -> Global.Global -> TypeLambda Scope.Global -> ST s Producer
globalDeclaration pre global definition = do
  context <- Context.start pre
  case definition of
    TypeLambdaOver {constraints = None, result = Core.ForeignExport {foreignCall, exported}} ->
      exportOf (Context.evidenceBindings Vector.empty context) global foreignCall exported
    _ -> declaration context definition

-- |
-- What C calls by a name, as a foreign export says: a function of the
-- values C gives, which applies what is exported to them, runs that if it
-- is an action, leaves the result for C and stops. C calls its code, which
-- comes back to C when it stops.
exportOf :: Context s scope -> Global.Global -> Foreign.Call -> Expression scope -> ST s Producer
exportOf context global call@Foreign.Call {arguments, action} exported = do
  names <- traverse (const (fresh context)) arguments
  result <- fresh context
  marker <- fresh context
  body <- evaluate context exported (foldr (\name -> Apply [Variable name]) (Runtime.callbackDone action) names)
  pure $
    Function (byValue <$> names) result $
      Primitive (Export call global) [] (MuTilde (byValue marker) body)

globalInstance :: Precontext -> Maybe Global.Global -> Instance Scope.Global -> ST s Producer
globalInstance pre lifted instancex = do
  context <- Context.start pre
  generateInstance context lifted instancex

-- |
-- A method of an instance at the top of a module, as a definition of its
-- own. The instance must need nothing, so that the method needs nothing but
-- what it says it does.
globalMember :: Precontext -> MethodConcrete Scope.Global -> ST s Producer
globalMember pre Definition {definition} = do
  context <- Context.start pre
  declaration (Context.evidenceBindings Vector.empty context) definition

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
-- What the evidence a name stands for is, and whether it is known to hold a
-- value: an instance, or evidence that was passed in.
instanceOf :: Context s scope -> Evidence.Index scope -> (Producer, Bool)
instanceOf context = \case
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
  where
    fromBinding = \case
      Variable.Local name -> Variable name
      Variable.Global global -> Global global

-- |
-- The method of an instance, when the instance is known where the method is
-- used: it is an instance at the top of a module that needs nothing, whose
-- methods are definitions of their own. The instance is then not looked at.
knownMethod :: Context s scope -> Evidence scope -> Int -> Maybe Producer
knownMethod context dictionary index = case dictionary of
  Evidence.Variable {variable = Direct _ (Tuple _)} -> Nothing
  Evidence.Variable {variable = variable@Direct {}, instanciation = Instanciation.Mono}
    | (Global global, _) <- instanceOf context variable -> Just (Global (Runtime.member global index))
  _ -> Nothing

-- |
-- A number written in the program, when its type is known to be one whose
-- numbers the program can hold as they are. Otherwise a number is an
-- @Integer@ that is turned into its type as the program runs.
number :: Expression scope -> Maybe Literal
number = \case
  Core.Call
    { function =
        Core.Method
          { method = Method.Index {typeIndex = Type2.Num, methodIndex},
            evidence = Evidence.Variable {variable = Direct Type2.Num target, instanciation = Instanciation.Mono}
          },
      argument = Core.Integer {integer}
    }
      | methodIndex == fromInteger' -> case target of
          -- As the instance has it, an Int keeps the low 63 bits.
          Type2.Int -> Just (Int (int63 integer))
          Type2.Integer -> Just (Integer integer)
          Type2.Double -> Just (Double (fromInteger integer))
          Type2.Float -> Just (Double (realToFrac (fromInteger integer :: Float)))
          _ -> Nothing
  -- A number written with a point, at Double or Float, is the number.
  Core.Call
    { function =
        Core.Method
          { method = Method.Index {typeIndex = Type2.Fractional, methodIndex},
            evidence = Evidence.Variable {variable = Direct Type2.Fractional target, instanciation = Instanciation.Mono}
          },
      argument = Core.Constructor {arguments}
    }
      | methodIndex == fromRational',
        [Core.Integer {integer = top}, Core.Integer {integer = bottom}] <- toList arguments,
        bottom /= 0 -> case target of
          Type2.Double -> Just (Double (fromRational (top % bottom)))
          Type2.Float -> Just (Double (realToFrac (fromRational (top % bottom) :: Float)))
          _ -> Nothing
  _ -> Nothing
  where
    Method.Index {methodIndex = fromInteger'} = Method.fromInteger
    Method.Index {methodIndex = fromRational'} = Method.fromRational

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
    let (producer, strict) = instanceOf context variable
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
  -- What a value holds is like anything else it holds: not evaluated yet,
  -- unless what named it says it is.
  Evidence.Abstract {count, body} -> \continue -> do
    names <- Vector.replicateM count (fresh context)
    result <- fresh context
    body <- generateEvidence (Context.evidenceBindings names context) body $ \evidence ->
      pure $ Cut evidence (Covariable result)
    name <- fresh context
    rest <- continue (Variable name)
    pure $ Cut (Function (byValue <$> toList names) result body) (MuTilde (byValue name) rest)
  Evidence.Applied {function, arguments} -> \continue ->
    generateEvidence context function $ \function ->
      generateEvidences context arguments $ \arguments -> do
        name <- fresh context
        body <- continue (Variable name)
        pure $ Cut function (Apply arguments (MuTilde (byValue name) body))
  Evidence.Held {term} -> \continue -> do
    let (producer, strict) = variable context term
    if strict
      then continue producer
      else do
        name <- fresh context
        body <- continue (Variable name)
        pure $ Cut producer (Force (MuTilde (byValue name) body))

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
  -- A definition with nothing before its @=@ comes from the check as the
  -- one way out of a match that matches nothing, inside the definitions of
  -- a @where@ that has none. What it is is what is inside. A function
  -- written so was a thunk that gave the function: one that called itself
  -- referred to itself, and was made again at every call (see
  -- "Sequent.Hoist"), where written with its arguments it was not.
  Core.Join {statements = Statements.Done {done}} -> object context done
  Core.Join {statements = Statements.Let {declarations, letBody = Statements.Done {done}}}
    | Declarations.none declarations -> do
        (context, _) <- generateDeclarations context declarations
        object context done
  expression -> do
    result <- fresh context
    body <- evaluate context expression (Covariable result)
    pure $ Thunk result body

-- |
-- Something that stands for an expression without evaluating it. Unlike
-- 'object', this can be a variable, a literal or a constructor.
delayed :: Context s scope -> Expression scope -> ST s Producer
delayed context = \case
  expression | Just literal <- number expression -> pure (Literal literal)
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
  _ | Just literal <- number expression -> pure $ Cut (Literal literal) consumer
  Core.Method
    { method = Method.Index {methodIndex},
      evidence = dictionary,
      instanciation
    }
      | Just method <- knownMethod context dictionary methodIndex ->
          implicit context False method instanciation consumer
  Core.Variable {variable = index, instanciation} -> do
    let (producer, strict) = variable context index
    case instanciation of
      -- What a newtype wrapped is evaluated to the function of evidence
      -- that it is.
      Instanciation arguments
        | Term.Binding {delayed = True} <- context !- index ->
            generateEvidences context (toList arguments) $ \arguments ->
              pure $ Cut producer (Force (Apply arguments consumer))
      _ -> implicit context strict producer instanciation consumer
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
    case argument of
      Variable _ -> evaluate context function (Apply [argument] consumer)
      -- The argument is made before the function is evaluated. Making it
      -- evaluates nothing, so the order does not matter to the program, but
      -- it lets a value that was just taken apart be built on in place
      -- before control goes anywhere else.
      _ -> do
        name <- fresh context
        body <- evaluate context function (Apply [Variable name] consumer)
        pure $ Cut argument (MuTilde (byNeed name) body)
  Core.Join {statements} ->
    named context consumer $ \consumer -> attempt context statements consumer Abort
  Core.Newtype {argument} -> evaluate context argument consumer
  Core.Dictionary {evidence} -> generateEvidence context evidence $ \dictionary ->
    pure $ Cut dictionary consumer
  Core.Helper {helperName} -> pure $ Cut (Global (Runtime.helperGlobal (unpack helperName))) (Force consumer)
  Core.Foreign {foreignCall} -> pure $ Cut (Runtime.foreignCall foreignCall) consumer
  Core.ForeignExport {} -> error "a foreign export is only at the top of a module"
  -- An action is a function of nothing, and what it gives may be yet to be
  -- evaluated.
  Core.Run {imperative} -> evaluate context imperative (Apply [] (Force consumer))
  Core.Generalized {definition = definition@TypeLambdaOver {constraints, result}} -> case constraints of
    None -> evaluate (Context.evidenceBindings Vector.empty context) result consumer
    Constraints {} -> do
      function <- declaration context definition
      pure $ Cut function consumer

letRec :: [(Text, Producer)] -> Statement -> Statement
letRec [] body = body
letRec bindings body = LetRec bindings body

-- |
-- Try the statements. The consumer is given the value of the first branch
-- whose patterns all match, and the failure statement runs if there is none.
-- The consumer must be a name and the failure statement must be small,
-- because both are copied into every branch.
attempt :: Context s scope -> Statements scope -> Consumer -> Statement -> ST s Statement
attempt context statements = match (clauses context statements)

-- |
-- A clause: some statements that are tried as a whole, with the names that
-- are in scope for them.
data Clause s where
  Clause :: Context s scope -> Statements scope -> Clause s

-- |
-- The clauses that some statements are, in the order they are tried.
clauses :: Context s scope -> Statements scope -> [Clause s]
clauses context = \case
  Statements.Branch {left, right} -> clauses context left ++ clauses context right
  statements -> [Clause context statements]

-- |
-- Try some clauses in turn, as a tree of tests in which nothing is looked
-- at twice.
--
-- The clauses at the front that each start by testing the same variable
-- are one test of it, with an alternative for each constructor they test it
-- against. The value is then looked at once, and no clause holds it while
-- another is tried, which is what lets it be taken apart in place.
--
-- Clauses that test it against the same constructor share an alternative,
-- and what each goes on to do is tried in turn inside it, in the same way.
-- So a second clause that starts as the first did does not start again: its
-- next test, of another variable or of something inside the constructor,
-- is put together with the first clause's next test. The fields of the
-- constructor have the same names for all of them, which is what lets
-- tests of a field be seen to be tests of the same thing.
--
-- A clause whose test fails without looking at anything else can be passed
-- over, so putting together clauses that were not next to each other
-- changes nothing that is evaluated or the order it is evaluated in.
match :: [Clause s] -> Consumer -> Statement -> ST s Statement
match [] _ failure = pure failure
match (first@(Clause context statements) : later) success failure = case heading first of
  Just (Head name strict _ _ _ _) -> do
    let (same, rest) = headed name (first : later)
        tags = nub [tag | Head _ _ tag _ _ _ <- same]
    rest <- match rest success failure
    joined context rest $ \failure -> do
      alternatives <- for tags $ \tag -> do
        let chosen = [head | head@(Head _ _ other _ _ _) <- same, other == tag]
            entries = concatMap fields (take 1 chosen)
        names <- replicateM (length entries) (fresh context)
        body <- match (concatMap (onward names) chosen) success failure
        pure $ Alternative tag (zipWith binder names entries) body
      let test = Case alternatives (Just failure)
      pure $ Cut (Variable name) (if strict then test else Force test)
  Nothing -> do
    rest <- match later success failure
    joined context rest $ \failure -> attemptOne context statements success failure
  where
    binder name strict
      | strict = byValue name
      | otherwise = byNeed name

heading :: Clause s -> Maybe (Head s)
heading (Clause context statements) = peel context statements

-- |
-- The clauses at the front that start by testing a variable, and the rest.
headed :: Text -> [Clause s] -> ([Head s], [Clause s])
headed name = \case
  clause : later
    | Just head@(Head other _ _ _ _ _) <- heading clause,
      other == name ->
        let (more, rest) = headed name later
         in (head : more, rest)
  rest -> ([], rest)

-- |
-- Whether each field of the constructor a clause tests for holds a value.
fields :: Head s -> [Bool]
fields (Head _ _ _ ConstructorInfo {entries} _ _) = [Info.strict strict | EntryInfo {strict} <- toList entries]

-- |
-- What a clause goes on to do when its test has matched, given the names of
-- the constructor's fields.
onward :: [Text] -> Head s -> [Clause s]
onward names (Head _ _ _ info inner thenx) =
  clauses (Context.patternBindings (Strict.Vector.fromList names) info inner) thenx

-- |
-- The first thing a clause does, when that is to test a variable against a
-- constructor: the variable and whether it holds a value, the constructor,
-- and what follows if it matches, with the names that are in scope there.
data Head s where
  Head ::
    !Text ->
    !Bool ->
    !Int ->
    ConstructorInfo scope ->
    Context s scope ->
    Statements (Scope.SimplePattern ':+ scope) ->
    Head s

-- |
-- A clause's first test. On the way to it a clause may give other names to
-- variables, which are followed.
peel :: Context s scope -> Statements scope -> Maybe (Head s)
peel context = \case
  Statements.LetOne
    { declaration = Core.Variable {variable = index, instanciation = Instanciation.Mono},
      body
    }
      | (Variable name, _) <- variable context index -> peel (singleBinding name context) body
  Statements.Bind
    { constructor = Constructor.Index {constructorIndex},
      constructorInfo,
      check = Core.Variable {variable = index, instanciation = Instanciation.Mono},
      thenx
    }
      | (Variable name, strict) <- variable context index ->
          Just (Head name strict constructorIndex constructorInfo context thenx)
  _ -> Nothing

-- |
-- Use a statement in more than one place. If it is not small it is named,
-- as a consumer, and what is used is a jump to it.
joined :: Context s scope -> Statement -> (Statement -> ST s Statement) -> ST s Statement
joined context statement continue = case statement of
  Abort -> continue statement
  Cut (Literal _) (Covariable _) -> continue statement
  _ -> do
    label <- fresh context
    ignored <- fresh context
    body <- continue (Cut (Literal $ Int 0) (Covariable label))
    pure $ Cut (Mu label body) (MuTilde (byValue ignored) statement)

attemptOne :: Context s scope -> Statements scope -> Consumer -> Statement -> ST s Statement
attemptOne context statements success failure = case statements of
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
    case value of
      -- Another name for a variable is the variable.
      Variable name -> attempt (singleBinding name context) body success failure
      _ -> do
        name <- fresh context
        body <- attempt (singleBinding name context) body success failure
        pure $ Cut value (MuTilde (byNeed name) body)
  Statements.Branch {left, right} -> do
    -- The right branch is where the left one goes when it fails.
    right <- attempt context right success failure
    joined context right $ \right -> attempt context left success right
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
  -- A local definition is called what the program calls it, with a name of
  -- the compiler's after it to keep it apart from any other of that name.
  variables <- for terms $ \Declaration {name} -> do
    unique <- fresh context
    pure $ Mangle.mangle name <> pack "%" <> unique
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
            value <- generateInstance context Nothing instancex
            pure (names Map.! index, value)
  classBindings <- instanceBindings classVariables classInstances
  dataBindings <- instanceBindings dataVariables dataInstances
  pure (context, termBindings ++ concat classBindings ++ concat dataBindings)

-- |
-- An instance: a constructed value of the evidence for its superclasses and
-- then its methods. It is a thunk, or a function of what it requires.
generateInstance :: Context s scope -> Maybe Global.Global -> Instance scope -> ST s Producer
generateInstance context lifted Instance {evidence = EvidenceSet supers, prerequisites, members} = do
  let count = case prerequisites of
        Constraints count -> length count
        None -> 0
  names <- Vector.replicateM count (fresh context)
  context <- pure $ Context.evidenceBindings names context
  result <- fresh context
  body <- generateEvidences context (toList supers) $ \supers -> do
    members <- case lifted of
      -- The methods are definitions of their own, and are only named here.
      Just global -> pure [Global (Runtime.member global index) | index <- [0 .. length members - 1]]
      Nothing -> for (toList members) $ \Definition {definition} ->
        declaration context definition
    pure $ Cut (Constructor 0 (supers ++ members)) (Covariable result)
  pure $ case prerequisites of
    None -> Thunk result body
    Constraints {} -> Function (byValue <$> toList names) result body
