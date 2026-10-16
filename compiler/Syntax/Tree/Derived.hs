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
-- derives one of these has to have them from the Prelude; and an @Ix@ uses
-- @range@, @index@, @inRange@ and @rangeSize@, which come with the class.
module Syntax.Tree.Derived (written) where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as Lazy (unpack)
import Data.Text.Lazy.Builder (fromString, toLazyText)
import Error (cannotDerive)
import Syntax.Extensions (hazy)
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
import Syntax.Variable (printLiteral')

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
-- The instance of a class for a type, written out, if the class is one of
-- those that are derived that way.
written ::
  Position ->
  Text ->
  Data Position ->
  Maybe (InstanceDeclarations Position)
written position className datax = case unpack className of
  "Read" -> Just (instancex [readable constructors])
  "Ix"
    | not (null constructors) && all (\Made {held} -> null held) constructors ->
        Just (instancex (enumerated constructors))
    | [constructor] <- constructors -> Just (instancex (single constructor))
    | otherwise -> refuse "only a type of constructors that hold nothing, or of one constructor, is"
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

-- |
-- The places of a type whose constructors hold nothing are the constructors
-- in the order they are written, as the report has it. Each is numbered by
-- a match, written again where it is wanted.
enumerated :: [Made] -> [String]
enumerated constructors =
  [ "range (a, b) = [x | x <- [" ++ intercalate ", " names ++ "], inRange (a, b) x]",
    "index (a, b) x = if inRange (a, b) x then "
      ++ number "x"
      ++ " - "
      ++ number "a"
      ++ " else error \"Ix.index: index out of range\"",
    "inRange (a, b) x = " ++ number "x" ++ " >= " ++ number "a" ++ " && " ++ number "x" ++ " <= " ++ number "b"
  ]
  where
    names = [name | Made {name} <- constructors]
    number value =
      "(case "
        ++ value
        ++ " of { "
        ++ intercalate " ; " [name ++ " -> " ++ show place | (name, place) <- zip names [0 :: Int ..]]
        ++ " } :: Int)"

-- |
-- The places of a type of one constructor are those of what it holds, the
-- last changing fastest, as a tuple's are.
single :: Made -> [String]
single constructor@Made {held = things} =
  [ "range (" ++ lower ++ ", " ++ upper ++ ") = " ++ ranged,
    "index (" ++ lower ++ ", " ++ upper ++ ") " ++ at ++ " = if " ++ inside ++ " then " ++ place ++ " else error \"Ix.index: index out of range\"",
    "inRange (" ++ lower ++ ", " ++ upper ++ ") " ++ at ++ " = " ++ inside
  ]
  where
    count = length things
    named prefix = [prefix ++ show index | index <- [1 .. count]]
    pattern values = "(" ++ built constructor values ++ ")"
    lower = pattern (named "l")
    upper = pattern (named "u")
    at = pattern (named "i")
    bounds index = "(l" ++ show index ++ ", u" ++ show index ++ ")"
    ranged
      | count == 0 = "[" ++ built constructor [] ++ "]"
      | otherwise =
          "["
            ++ built constructor (named "i")
            ++ " | "
            ++ intercalate ", " ["i" ++ show index ++ " <- range " ++ bounds index | index <- [1 .. count]]
            ++ "]"
    inside
      | count == 0 = "True"
      | otherwise = intercalate " && " ["inRange " ++ bounds index ++ " i" ++ show index | index <- [1 .. count]]
    place =
      foldl
        (\before index -> "(" ++ before ++ ") * rangeSize " ++ bounds index ++ " + index " ++ bounds index ++ " i" ++ show index)
        "0"
        [1 .. count]

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
