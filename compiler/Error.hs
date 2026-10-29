-- |
-- The errors the compiler reports in a program it is given. Each kind of
-- error is a function that throws a 'HazyError': its kind ('Type'), where
-- it is, other places it is about, and a message. 'fail' prints an error
-- and exits; 'allow' lets a run that expects a kind of error
-- (@--debug-fail@) pass when that kind is thrown. The names of what the
-- lexer and parser expect to find are here too.
module Error
  ( HazyError (..),
    Type,
    types,
    report,
    fail,
    allow,
    digit,
    octal,
    hexadecimal,
    integer,
    decimal,
    string,
    pathCharacter,
    endOfFile,
    nonSymbol,
    newline,
    character,
    stringCharacter,
    whitespace,
    whitespaceNoLine,
    variableIdentifier,
    qualifiedVariableIdentifier,
    constructorIdentifier,
    qualifiedConstructorIdentifier,
    variableSymbol,
    qualifiedVariableSymbol,
    constructorSymbol,
    qualifiedConstructorSymbol,
    typeOperator,
    moduleIdentifier,
    braceIdentionError,
    parenIdentionError,
    bracketIdentionError,
    pragmaIdentionError,
    Position,
    locate,
    expected,
    missingMethodEntry,
    missingVariableEntry,
    missingConstructorDeclaration,
    missingTypeDeclaration,
    duplicateVariableEntries,
    duplicateConstructorEntries,
    duplicateTypeEntries,
    duplicateMethodEntries,
    duplicateFixityEntries,
    duplicateAnnotationEntries,
    duplicateFieldEntries,
    duplicateModuleEntries,
    duplicateUnorderedEntries,
    mismatchedModuleNames,
    moduleNotFound,
    moduleNotInScope,
    variableNotInScope,
    typeNotInScope,
    constructorNotInScope,
    fieldNotInScope,
    cannotTurnoffExtension,
    mismatchSelectors,
    unstableImports,
    unstableShadowing,
    orderDependentUsage,
    cyclicalImports,
    unificationError,
    nonUniqueConstraints,
    constraintError,
    escapingType,
    refiningBinding,
    illegalInstanceClass,
    illegalInstanceData,
    illegalFixity,
    constraintNonLocal,
    illegalConstraint,
    mismatchSelectorTypes,
    badRunSTCall,
    overlappingInstances,
    patternInMethod,
    patternInInstance,
    notClassMethod,
    instanceForNonClass,
    unneededFieldQualification,
    occurenceError,
    cyclicalTypeChecking,
    mismatchedConstructorArguments,
    universeMustBeSmall,
    uncheckable,
    invalidNewtype,
    maskError,
    orphanInstance,
    improperBindingGroup,
    cannotDerive,
    derivingRequired,
    partialSynonym,
    derivingNonEnum,
    strictDeclaration,
    unsupportedFeatureRunST,
    unsupportedFeatureConstraintedTypeDefaulting,
    unsupportedFeatureStrictFunctions,
    unsupportedFeaturePolymorphicComponents,
    unsupportedFeatureGADTs,
    unsupportedFeatureEqualityConstraints,
    unsupportedForeignType,
    unsupportedForeignEntity,
  )
where

import Control.Exception (Exception, throw)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Lazy as Lazy.Text (toStrict)
import Data.Text.Lazy.Builder (Builder, fromString, fromText, toLazyText)
import qualified Diagnostics
import Syntax.ParserCombinator
  ( Position,
    column,
    file,
    line,
  )
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stdout, utf8)
import Prelude hiding (fail)

-- |
-- An error in the program being compiled: its kind, where it is, the other
-- places it is about, and what to say. Errors are thrown from pure code and
-- caught by whoever started the compiler.
data HazyError = HazyError !Type !Position [Position] Lazy.Text
  deriving (Show)

instance Exception HazyError

-- |
-- An error as the diagnostics library takes it.
report :: HazyError -> Diagnostics.Report
report (HazyError typex position others message) =
  Diagnostics.Report
    { Diagnostics.code = Just (Text.pack (show typex)),
      Diagnostics.message = Lazy.Text.toStrict message,
      Diagnostics.location = location position,
      Diagnostics.related = [(location other, Text.pack "also here") | other <- others]
    }
  where
    location position =
      Diagnostics.Location
        { Diagnostics.file = file position,
          Diagnostics.line = line position,
          Diagnostics.column = column position
        }

fail :: HazyError -> IO ()
fail error = do
  let reports = [report error]
  sources <- Diagnostics.load reports
  hSetEncoding stdout utf8
  Diagnostics.print stdout sources reports
  exitFailure

allow :: Type -> HazyError -> IO ()
allow typex' error@(HazyError typex _ _ _)
  | typex == typex' = pure ()
  | otherwise = throw error

digit = fromString "digit"

octal = fromString "octal"

hexadecimal = fromString "hexadecimal"

integer = fromString "integer"

decimal = fromString "decimal"

string = fromString "string"

pathCharacter = fromString "path character"

endOfFile = fromString "end of file"

nonSymbol = fromString "non symbol"

newline = fromString "newline"

character = fromString "character"

stringCharacter = fromString "string character"

whitespace = fromString "whitespace"

whitespaceNoLine = fromString "non-line whitespace"

annotation = fromString "annotation"

variable = fromString "variable"

fixity = fromString "fixity"

variableIdentifier = fromString "variable identifier"

qualifiedVariableIdentifier = fromString "qualified variable identifier"

constructor = fromString "constructor"

constructorIdentifier = fromString "constructor identifier"

qualifiedConstructorIdentifier = fromString "qualified constructor identifier"

variableSymbol = fromString "variable symbol"

qualifiedVariableSymbol = fromString "qualified variable symbol"

constructorSymbol = fromString "constructor symbol"

qualifiedConstructorSymbol = fromString "qualified constructor symbol"

typeOperator = fromString "type operator"

moduleIdentifier = fromString "module identifier"

field = fromString "field"

method = fromString "method"

typex = fromString "type"

braceIdentionError = fromString "brace indentation error"

parenIdentionError = fromString "parenthesis indentation error"

bracketIdentionError = fromString "bracket indentation error"

pragmaIdentionError = fromString "bracket indentation error"

modulex = fromString "module"

languagePragma = fromString "language pragma"

unorderedPragma = fromString "unordered pragma"

locate :: Position -> Builder
locate position =
  mconcat
    [ fromText (file position),
      fromString ":",
      fromString (show (line position)),
      fromString ":",
      fromString (show (column position))
    ]

data Type
  = Expected
  | MissingVariableDeclaration
  | MissingMethodDeclaration
  | MissingConstructorDeclaration
  | MissingTypeDeclaration
  | DuplicateVariableEntries
  | DuplicateFixityEntries
  | DuplicateAnnotationEntries
  | DuplicateConstructorEntries
  | DuplicateMethodEntries
  | DuplicateFieldEntries
  | DuplicateTypeEntries
  | DuplicateModuleEntries
  | DuplicateUnordereds
  | MismatchModuleNames
  | ModuleNotFound
  | ModuleNotInScope
  | VariableNotInScope
  | TypeNotInScope
  | ConstructorNotInScope
  | FieldNotInScope
  | CannotTurnOffExtension
  | MismatchSelectors
  | UnstableImport
  | UnstableShadowing
  | OrderDependentUsage
  | CyclicalImports
  | UnificationError
  | NonUniqueConstraints
  | ConstraintError
  | EscapingType
  | RefiningBinding
  | IllegalInstanceClass
  | IllegalInstanceData
  | IllegalFixity
  | RecordSyntaxOnBuiltin
  | MismatchedSelectorTypes
  | ConstraintNonLocal
  | IllegalConstraint
  | BadRunSTCall
  | OverlappingInstances
  | PatternInMethod
  | PatternInInstance
  | NotClassMethod
  | InstanceForNonClass
  | UnneededFieldQualification
  | OccurenceError
  | CyclicalTypeChecking
  | MismatchedConstructorArguments
  | UniverseMustBeSmall
  | Uncheckable
  | InvalidNewtype
  | Mismask
  | OrphanInstance
  | ImproperBindingGroup
  | UnsupportedFeature
  | CannotDerive
  | PartialSynonym
  | DerivingNonEnum
  | StrictDeclaration
  deriving (Show, Eq, Enum, Bounded)

types :: Map String Type
types = Map.fromList [(show error, error) | error <- [minBound .. maxBound]]

errorAt :: Type -> Position -> Builder -> a
errorAt typex position = errorAlso typex position []

-- |
-- An error that is about other places as well.
errorAlso :: Type -> Position -> [Position] -> Builder -> a
errorAlso typex position others message = throw $ HazyError typex position others (toLazyText message)

expected :: Position -> [Builder] -> Builder -> a
expected position expect found = errorAt Expected position (mconcat builders)
  where
    builders =
      [ fromString "expected ",
        many,
        mconcat tokens,
        fromString " but found ",
        found
      ]
    tokens = intersperse (fromString ", ") expect
    many = case expect of
      [_] -> mempty
      _ -> fromString "one of "

missingDeclaration :: Type -> Builder -> Position -> a
missingDeclaration typex brand position =
  errorAt typex position $
    mconcat
      [ fromString "missing ",
        brand,
        fromString " declaration"
      ]

missingVariableEntry = missingDeclaration MissingVariableDeclaration variable

missingMethodEntry = missingDeclaration MissingMethodDeclaration method

missingConstructorDeclaration = missingDeclaration MissingConstructorDeclaration constructor

missingTypeDeclaration = missingDeclaration MissingTypeDeclaration typex

duplicateEntries :: Type -> Builder -> [Position] -> a
duplicateEntries typex brand entries =
  errorAlso typex (head entries) (tail entries) (mconcat builders)
  where
    builders =
      [ fromString "duplicate ",
        brand,
        fromString " declarations at ",
        mconcat $ intersperse (fromString ", ") (locate <$> tail entries)
      ]

duplicateVariableEntries = duplicateEntries DuplicateVariableEntries variable

duplicateFixityEntries = duplicateEntries DuplicateFixityEntries fixity

duplicateAnnotationEntries = duplicateEntries DuplicateAnnotationEntries annotation

duplicateConstructorEntries = duplicateEntries DuplicateConstructorEntries constructor

duplicateMethodEntries = duplicateEntries DuplicateMethodEntries method

duplicateFieldEntries = duplicateEntries DuplicateFieldEntries field

duplicateTypeEntries = duplicateEntries DuplicateTypeEntries typex

duplicateModuleEntries = duplicateEntries DuplicateModuleEntries modulex

duplicateUnorderedEntries = duplicateEntries DuplicateUnordereds unorderedPragma

mismatchedModuleNames :: Position -> a
mismatchedModuleNames position = errorAt MismatchModuleNames position message
  where
    message = fromString "module name doesn't match file name"

moduleNotFound :: Position -> a
moduleNotFound position = errorAt ModuleNotFound position message
  where
    message = fromString "module not found"

notInScope :: Type -> Builder -> Position -> a
notInScope typex brand position = errorAt typex position (mconcat builders)
  where
    builders =
      [ brand,
        fromString " not in scope"
      ]

moduleNotInScope = notInScope ModuleNotInScope modulex

variableNotInScope = notInScope VariableNotInScope variable

typeNotInScope = notInScope TypeNotInScope typex

constructorNotInScope = notInScope ConstructorNotInScope constructor

fieldNotInScope = notInScope FieldNotInScope field

cannotTurnoffExtension :: Position -> Builder -> a
cannotTurnoffExtension position extension =
  errorAt CannotTurnOffExtension position (mconcat builders)
  where
    builders =
      [ fromString "cannot turn off ",
        extension
      ]

mismatchSelectors :: Position -> a
mismatchSelectors position = errorAt MismatchSelectors position $ fromString "mismatched selectors"

unstableImports :: [Position] -> a
unstableImports positions = errorAlso UnstableImport (head positions) (tail positions) $ mconcat builders
  where
    builders =
      [ fromString "at most one import may be unstable: ",
        mconcat $ intersperse (fromString ", ") $ map locate (tail positions)
      ]

unstableShadowing :: Position -> a
unstableShadowing position =
  errorAt UnstableShadowing position $ fromString "local unstable imports are not supported"

orderDependentUsage :: Position -> a
orderDependentUsage position =
  errorAt OrderDependentUsage position $ fromString "order dependent usage of record constructor"

cyclicalImports :: Position -> a
cyclicalImports position =
  errorAt CyclicalImports position $ fromString "unresolvable cyclical imports"

unificationError :: Position -> Builder -> Builder -> a
unificationError position left right =
  errorAt UnificationError position $ mconcat builders
  where
    builders =
      [ fromString "unification error: couldn't match `",
        left,
        fromString "` with `",
        right,
        fromString "`"
      ]

nonUniqueConstraints :: Position -> a
nonUniqueConstraints position =
  errorAt NonUniqueConstraints position $
    fromString
      "constraints include non-unique typeclass variable pair"

constraintError :: Position -> Builder -> a
constraintError position satify =
  errorAt ConstraintError position $ mconcat builders
  where
    builders =
      [ fromString "constraint error: unable to satify `",
        satify,
        fromString "`"
      ]

escapingType :: Position -> Builder -> a
escapingType position typex =
  errorAt EscapingType position $ mconcat builders
  where
    builders =
      [ fromString "escaping type: `",
        typex,
        fromString "`"
      ]

refiningBinding :: Position -> a
refiningBinding position =
  errorAt RefiningBinding position $
    fromString "a binding cannot match a constructor that hides a type or says what type it makes"

illegalInstanceClass :: Position -> a
illegalInstanceClass position =
  errorAt IllegalInstanceClass position $ fromString "illegal instance class"

illegalInstanceData :: Position -> a
illegalInstanceData position =
  errorAt IllegalInstanceData position $ fromString "illegal instance data"

illegalFixity :: Position -> a
illegalFixity position =
  errorAt IllegalFixity position $ fromString "illegal fixity"

constraintNonLocal position =
  errorAt ConstraintNonLocal position $ fromString "constraint with non local variable"

illegalConstraint :: Position -> a
illegalConstraint position =
  errorAt IllegalConstraint position $ fromString "illegal constraint"

mismatchSelectorTypes :: Position -> a
mismatchSelectorTypes position =
  errorAt MismatchedSelectorTypes position $ fromString "mismatched selector types"

badRunSTCall :: Position -> b
badRunSTCall position =
  errorAt BadRunSTCall position $ fromString "bad runST call"

overlappingInstances :: [Position] -> a
overlappingInstances positions =
  errorAlso OverlappingInstances (head positions) (tail positions) (mconcat builders)
  where
    builders =
      [ fromString "overlapping instance definitions:",
        mconcat $ intersperse (fromString ", ") (locate <$> tail positions)
      ]

patternInMethod :: Position -> a
patternInMethod position = errorAt PatternInMethod position $ fromString "pattern in method definition"

patternInInstance :: Position -> a
patternInInstance position = errorAt PatternInInstance position $ fromString "pattern in instance definition"

notClassMethod :: Position -> a
notClassMethod position = errorAt NotClassMethod position $ fromString "instance definition not a class method"

instanceForNonClass :: Position -> a
instanceForNonClass position = errorAt InstanceForNonClass position $ fromString "instance declaration for non-class"

unneededFieldQualification :: Position -> a
unneededFieldQualification position =
  errorAt UnneededFieldQualification position $ fromString "unneed field qualification"

occurenceError :: Position -> Builder -> Builder -> a
occurenceError position infinite term =
  errorAt OccurenceError position $ mconcat builders
  where
    builders =
      [ fromString "occurance error: `",
        infinite,
        fromString "` appears in `",
        term,
        fromString "`"
      ]

cyclicalTypeChecking :: Position -> a
cyclicalTypeChecking position =
  errorAt CyclicalTypeChecking position $ fromString "cyclical type checking"

universeMustBeSmall :: Position -> a
universeMustBeSmall position = errorAt UniverseMustBeSmall position $ fromString "universe must be small"

uncheckable :: Position -> a
uncheckable position = errorAt Uncheckable position $ fromString "not typecheckable"

mismatchedConstructorArguments :: Position -> a
mismatchedConstructorArguments position =
  errorAt MismatchedConstructorArguments position $ fromString "mismatched number of constructor arguments"

invalidNewtype :: Position -> a
invalidNewtype position = errorAt InvalidNewtype position $ fromString "Invalid newtype definition"

maskError :: Position -> Builder -> a
maskError position typex = errorAt Mismask position $ mconcat builders
  where
    builders =
      [ fromString "masking error: `",
        typex,
        fromString "` needs to have a precise type"
      ]

orphanInstance :: Position -> a
orphanInstance position = errorAt OrphanInstance position $ fromString "Orphan instance"

improperBindingGroup :: Position -> a
improperBindingGroup position = errorAt ImproperBindingGroup position $ fromString "improper binding group"

cannotDerive :: Position -> Builder -> a
cannotDerive position instancex = errorAt CannotDerive position $ mconcat builders
  where
    builders =
      [ fromString "cannot derive: ",
        instancex
      ]

derivingRequired :: Position -> a
derivingRequired position =
  errorAt CannotDerive position $
    fromString "cannot derive an instance for a type with a constructor that requires something of its types"

partialSynonym :: Position -> a
partialSynonym position = errorAt PartialSynonym position $ fromString "not enough type synonym arguments"

derivingNonEnum :: Position -> a
derivingNonEnum position = errorAt DerivingNonEnum position $ fromString "data type is not an enum"

strictDeclaration :: Position -> a
strictDeclaration position = errorAt StrictDeclaration position $ fromString "declarations cannot be strict"

strictFunctions = fromString "strict functions"

gadts = fromString "GADTs"

polymorphicComponents = fromString "PolymorphicComponents"

constraintedTypeDefaulting = fromString "constrained type defaulting"

runST = fromString "runST"

equalityConstraints = fromString "equality constraints"

unsupportedFeature :: Builder -> Position -> a
unsupportedFeature feature position =
  errorAt UnsupportedFeature position $ fromString "unsupported feature: " <> feature

unsupportedFeatureRunST :: Position -> a
unsupportedFeatureRunST = unsupportedFeature runST

unsupportedFeatureConstraintedTypeDefaulting :: Position -> a
unsupportedFeatureConstraintedTypeDefaulting =
  unsupportedFeature constraintedTypeDefaulting

unsupportedFeatureStrictFunctions :: Position -> a
unsupportedFeatureStrictFunctions =
  unsupportedFeature strictFunctions

unsupportedFeaturePolymorphicComponents :: Position -> a
unsupportedFeaturePolymorphicComponents =
  unsupportedFeature polymorphicComponents

unsupportedFeatureGADTs :: Position -> a
unsupportedFeatureGADTs = unsupportedFeature gadts


-- | A foreign function of a type that is not passed to C.
unsupportedForeignType :: Position -> a
unsupportedForeignType =
  unsupportedFeature
    (fromString "a foreign type other than a number, Char, Bool, an address and (), in IO or not, with nothing required of its variables")

-- | A foreign import of an address, or of something not named by a C name.
unsupportedForeignEntity :: Position -> a
unsupportedForeignEntity = unsupportedFeature (fromString "a foreign import of other than a C function by name")

unsupportedFeatureEqualityConstraints :: Position -> a
unsupportedFeatureEqualityConstraints = unsupportedFeature equalityConstraints
