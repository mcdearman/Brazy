-- |
-- Instances that are derived by writing them out.
--
-- The classes the compiler has built in are derived where instances are
-- checked: it makes the methods itself, out of what it knows of the type.
-- The classes here are derived as a programmer would write them. The
-- instance is written as text, of the names a program has from the Prelude,
-- and read as any instance is. So a class of the library can be derived,
-- which the compiler knows nothing of, and whatever is wrong with what is
-- derived is said the way it is of anything written.
--
-- What is written uses @fmap@, @foldr@, @traverse@, @pure@, @(\<*\>)@,
-- @readsPrec@, @readParen@ and @lex@ by those names, so a module that
-- derives one of these has to have them from the Prelude.
module Syntax.Tree.Derived (written) where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as Lazy (unpack)
import Data.Text.Lazy.Builder (fromString, toLazyText)
import qualified Data.Vector.Strict as Strict (Vector)
import Error (cannotDerive)
import Syntax.Extensions (hazy)
import Syntax.FreeVariables (freeTypeVariables)
import qualified Syntax.Parser as Parser (betweenBraces, parse)
import Syntax.Position (Position)
import Syntax.Tree.Constructor (Constructor)
import qualified Syntax.Tree.Constructor as Constructor (Constructor (..))
import Syntax.Tree.Data (Data)
import qualified Syntax.Tree.Data as Data (Data (..))
import qualified Syntax.Tree.Entry as Entry (Entry (..))
import qualified Syntax.Tree.Field as Field (Field (..))
import Syntax.Tree.InstanceDeclarations (InstanceDeclarations)
import qualified Syntax.Tree.InstanceDeclarations as InstanceDeclarations (parse)
import Syntax.Tree.Marked (Marked ((:@)))
import qualified Syntax.Tree.Scheme as Scheme (Scheme (..))
import Syntax.Tree.Type (Type)
import qualified Syntax.Tree.Type as Type (Type (..))
import Syntax.Tree.TypePattern (TypePattern)
import qualified Syntax.Tree.TypePattern as TypePattern (TypePattern (..))
import Syntax.Variable (VariableIdentifier, printLiteral')

-- |
-- A constructor as the instances see it: how it is written before what it
-- holds, the names of its fields if it has them, and the type of each thing
-- it holds.
data Made = Made
  { name :: String,
    fields :: Maybe [String],
    held :: [Type Position]
  }

-- |
-- How the last parameter of a type is found in the type of something a
-- constructor holds.
data Found
  = -- | It is not there.
    Fixed
  | -- | It is the whole type.
    Whole
  | -- | It is found in the last argument of a type that does not mention it
    -- otherwise: a list of it, say.
    Inside Found

-- |
-- The instance of a class for a type, written out, if the class is one of
-- those that are derived that way. With it is whether the instance is of
-- the type without its last parameter, as an instance of @Functor@ is.
written ::
  Position ->
  Text ->
  Strict.Vector (TypePattern Position) ->
  Data Position ->
  Maybe (Bool, InstanceDeclarations Position)
written position className parameters datax = case unpack className of
  "Functor" -> Just (True, instancex (map functor (shaped constructors)))
  "Foldable" -> Just (True, instancex (map foldable (shaped constructors) ++ [nothing]))
  "Traversable" -> Just (True, instancex (map traversable (shaped constructors)))
  "Read" -> Just (False, instancex [readable constructors])
  _ -> Nothing
  where
    refuse :: String -> a
    refuse why = cannotDerive position (fromString (unpack className ++ ": " ++ why))
    constructors = case datax of
      Data.ADT {constructors} -> map made (toList constructors)
      Data.GADT {} -> refuse "the type says what its constructors make"
    instancex clauses =
      Parser.parse
        hazy
        (const <$> Parser.betweenBraces InstanceDeclarations.parse)
        (pack "an instance that was derived")
        (pack ("where { " ++ intercalate " ; " clauses ++ " }"))
    -- A fold of a type with no constructors, so that the instance has the
    -- method whatever the type.
    nothing = "foldr _ z _ = z"
    parameter = case reverse (toList parameters) of
      TypePattern.TypePattern {name} : _ -> name
      [] -> refuse "the type has no parameter"
    shaped = map (\constructor@Made {held} -> (constructor, map (found parameter) held))
    found :: VariableIdentifier -> Type Position -> Found
    found variable typex
      | not (mentions variable typex) = Fixed
      | otherwise = case typex of
          Type.Variable {} -> Whole
          Type.List {element} -> Inside (found variable element)
          Type.Call {function, argument}
            | not (mentions variable function) -> Inside (found variable argument)
          _ -> refuse "the last parameter is in a type it cannot be reached in"
    mentions variable typex = variable `elem` [mentioned | _ :@ mentioned <- freeTypeVariables typex]

-- |
-- What the instances need of a constructor.
made :: Constructor Position -> Made
made = \case
  Constructor.Constructor {constructor = _ :@ name, entries} ->
    Made {name = literal name, fields = Nothing, held = map entry (toList entries)}
  Constructor.Infix {left, constructor = _ :@ name, right} ->
    Made {name = literal name, fields = Nothing, held = [entry left, entry right]}
  Constructor.Record {constructor = _ :@ name, fields} ->
    Made
      { name = literal name,
        fields = Just [literal field | (field, _) <- named],
        held = map snd named
      }
    where
      named = do
        Field.Field {names, entry = held} <- toList fields
        _ :@ field <- toList names
        pure (field, entry held)
  where
    literal variable = Lazy.unpack (toLazyText (printLiteral' variable))
    entry held = case Entry.entry held of
      Scheme.Explicit {result} -> result
      Scheme.Implicit {result} -> result

-- | The names of what a constructor holds, one after another.
variables :: Made -> [String]
variables Made {held} = ["x" ++ show index | index <- [1 .. length held]]

-- | A constructor and what it holds, as a pattern.
matched :: Made -> String
matched constructor = "(" ++ built constructor (variables constructor) ++ ")"

-- |
-- A constructor of some things. One with fields is given them by name: a
-- type may ask that its fields are never gone by their order.
built :: Made -> [String] -> String
built Made {name, fields} values = case fields of
  Nothing -> unwords (name : values)
  Just names -> name ++ " {" ++ intercalate ", " (zipWith (\field value -> field ++ " = " ++ value) names values) ++ "}"

-- | A constructor as a function of what it holds.
building :: Made -> String
building constructor@Made {name, fields} = case fields of
  Nothing -> name
  Just _
    | null (variables constructor) -> built constructor []
    | otherwise -> "(\\" ++ unwords (variables constructor) ++ " -> " ++ built constructor (variables constructor) ++ ")"

functor :: (Made, [Found]) -> String
functor (constructor, shapes) =
  "fmap f " ++ matched constructor ++ " = " ++ built constructor (zipWith (go (1 :: Int)) shapes (variables constructor))
  where
    go depth shape variable = case shape of
      Fixed -> variable
      Whole -> "(f " ++ variable ++ ")"
      Inside inner ->
        let each = "y" ++ show depth
         in "(fmap (\\" ++ each ++ " -> " ++ go (depth + 1) inner each ++ ") " ++ variable ++ ")"

-- A fold is written as its steps in a list, each a function of what the
-- rest folds to, and not as one step inside another: what is written is no
-- deeper for a constructor that holds a great many things.
foldable :: (Made, [Found]) -> String
foldable (constructor, shapes) =
  "foldr f z " ++ matched constructor ++ " = foldr (\\step rest -> step rest) z [" ++ intercalate ", " steps ++ "]"
  where
    steps = [step (1 :: Int) shape variable | (shape, variable) <- zip shapes (variables constructor), found shape]
    found = \case
      Fixed -> False
      _ -> True
    step depth shape variable = case shape of
      Fixed -> "id"
      Whole -> "f " ++ variable
      Inside inner ->
        let each = "y" ++ show depth
            after = "r" ++ show depth
         in "\\" ++ after ++ " -> foldr (\\" ++ each ++ " -> " ++ step (depth + 1) inner each ++ ") " ++ after ++ " " ++ variable

traversable :: (Made, [Found]) -> String
traversable (constructor, shapes) =
  "traverse f " ++ matched constructor ++ " = " ++ concat (("pure " ++ building constructor) : map (" <*> " ++) (zipWith (go (1 :: Int)) shapes (variables constructor)))
  where
    go depth shape variable = case shape of
      Fixed -> "pure " ++ variable
      Whole -> "f " ++ variable
      Inside inner ->
        let each = "y" ++ show depth
         in "traverse (\\" ++ each ++ " -> " ++ go (depth + 1) inner each ++ ") " ++ variable

-- |
-- A value is read as it is shown: a constructor and then what it holds, in
-- brackets where it is an argument, or a constructor and its fields by name.
readable :: [Made] -> String
readable = \case
  [] -> "readsPrec _ _ = []"
  constructors -> "readsPrec d r = " ++ intercalate " ++ " (map one constructors)
  where
    one constructor@Made {name, fields, held}
      | take 1 name == "(" = cannotRead
      | otherwise = case fields of
          Nothing
            | null held -> reader "False" (name, "t0") [token name "s" "t0"]
            | otherwise ->
                reader
                  "(d > 10)"
                  (built constructor (variables constructor), "t" ++ show (length held))
                  ( token name "s" "t0"
                      : [ "(" ++ variable ++ ", t" ++ show index ++ ") <- readsPrec 11 t" ++ show (index - 1)
                          | (index, variable) <- zip [1 :: Int ..] (variables constructor)
                        ]
                  )
          Just names
            | any ((== "(") . take 1) names -> cannotRead
            | otherwise ->
                reader
                  "(d > 11)"
                  (built constructor (variables constructor), "end")
                  ( [token name "s" "t0", token "{" "t0" "u0"]
                      ++ concat
                        [ [ token field ("u" ++ show (index - 1)) ("a" ++ show index),
                            token "=" ("a" ++ show index) ("b" ++ show index),
                            "(" ++ variable ++ ", c" ++ show index ++ ") <- readsPrec 0 b" ++ show index
                          ]
                            ++ [token "," ("c" ++ show index) ("u" ++ show index) | index < length names]
                          | (index, field, variable) <- zip3 [1 :: Int ..] names (variables constructor)
                        ]
                      ++ [token "}" (if null names then "u0" else "c" ++ show (length names)) "end"]
                  )
    -- One alternative: what is made and what is left, of the steps that get
    -- there, in brackets if the first of these says so.
    reader brackets (value, rest) steps =
      "readParen " ++ brackets ++ " (\\s -> [(" ++ value ++ ", " ++ rest ++ ") | " ++ intercalate ", " steps ++ "]) r"
    -- A word that has to come next.
    token word from to = "(" ++ show word ++ ", " ++ to ++ ") <- lex " ++ from
    cannotRead = error "a constructor or a field that is an operator is not read by a derived instance"
