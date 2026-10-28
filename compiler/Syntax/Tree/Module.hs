{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for modules
module Syntax.Tree.Module (Module (..), assumeName, parse) where

import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import Error (mismatchedModuleNames)
import Syntax.Extensions (Extensions)
import Syntax.Lexer (FullQualifiers (..), Qualifiers (..), constructorIdentifier, variableIdentifier)
import Syntax.Parser (Parser, position, token, (<|>))
import Syntax.Position (Position)
import Syntax.Tree.Declarations (Declarations)
import qualified Syntax.Tree.Declarations as Declarations
import Syntax.Tree.Exports (Exports (..))
import qualified Syntax.Tree.Exports as Exports
import qualified Syntax.Tree.ExportSymbol as ExportSymbol
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (QualifiedVariable (..), Variable (..))
import qualified Syntax.Variable as Variable
import Verbose as Null (Debug)
import qualified Verbose

data Module position = Module
  { extensions :: Extensions,
    modulePosition :: position,
    name :: !FullQualifiers,
    exports :: Exports,
    declarations :: Declarations position
  }
  deriving (Show)

assumeName :: FullQualifiers -> Module Position -> Module Position
assumeName name' ~Module {extensions, modulePosition, name, exports, declarations} =
  Module
    { name = name',
      extensions = seq valid extensions,
      modulePosition = seq valid modulePosition,
      exports = seq valid exports,
      declarations = seq valid declarations
    }
  where
    valid
      | name == name' = ()
      | otherwise = mismatchedModuleNames modulePosition

parse :: (Debug verbose) => Parser (Extensions -> verbose (Module Position))
parse = headed <|> headerless
  where
    headed = modulex <$> position <*> (token "module" *> Variable.parse) <*> Exports.parse <*> body
    -- A module that is its body alone is @Main@, and gives @main@.
    headerless = do
      modulePosition <- position
      declarations <- Declarations.parse
      pure $ modulex modulePosition mainModule (mainExports modulePosition) declarations
    mainModule = Local :.. constructorIdentifier (pack "Main")
    mainExports modulePosition =
      Exports
        { exports =
            Strict.Vector.singleton
              ExportSymbol.Definition
                { variable = modulePosition :@ (Local :- VariableIdentifier (variableIdentifier (pack "main")))
                }
        }
    modulex modulePosition name exports declarations extensions =
      Verbose.parsing
        (Variable.print' name)
        Module
          { extensions,
            modulePosition,
            name,
            exports,
            declarations
          }
    body = token "where" *> Declarations.parse
