{-# LANGUAGE_HAZY UnorderedRecords #-}
-- |
-- A package the compiler is given: its directory, the extensions its
-- modules are read with, and its modules. 'load' reads the @package@ file,
-- which lists the extensions, a @;@, and the path of each module; a
-- module's source is read from the @header@ directory when it is needed.
module Package (Package (..), Module (..), load) where

import Control.Monad (void)
import Data.Char (isUpper)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as Text
import Data.Text.Lazy.Builder (fromString)
import Data.Traversable (for)
import Syntax.Lexer (FullQualifiers (..), Toggle, legalChar)
import qualified Syntax.Lexer as Lexer
import Syntax.ParserCombinator
  ( Parser,
    Stream,
    char,
    many,
    parse,
    satify,
    some,
    startStream,
    string,
    (<|>),
  )
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), hSetEncoding, openFile, utf8)
import System.IO.Unsafe (unsafeInterleaveIO)

data Module = Module
  { name :: !Text,
    path :: !FullQualifiers,
    header :: Text
  }
  deriving (Show)

data Package = Package
  { -- | The directory the package is in.
    root :: FilePath,
    extensions :: [Toggle],
    modules :: [Module]
  }
  deriving (Show)

data Path
  = Root !Text
  | !Text :</> !Path
  deriving (Show)

infixr 5 :</>

path' :: Parser Stream Path
path' = do
  head <- satify (fromString "Upper case") isUpper
  tail <- many $ satify (fromString "Legal character") legalChar
  let name = pack (head : tail)
      file = do
        string (pack ".hs")
        pure (Root name)
      folder = do
        char '/'
        subpath <- path'
        pure (name :</> subpath)
  file <|> folder

haskell :: Path -> FilePath
haskell = \case
  Root name -> unpack name ++ ".hs"
  root :</> subpath -> unpack root </> haskell subpath

qualifiers :: Path -> FullQualifiers
qualifiers = go Lexer.Local
  where
    go prefix (Root name) = prefix Lexer.:.. Lexer.constructorIdentifier name
    go prefix (root :</> subpath) = go (prefix Lexer.:. Lexer.constructorIdentifier root) subpath

data Meta = Meta
  { toggles :: [Toggle],
    paths :: [Path]
  }

white :: Parser Stream ()
white = do
  void $ some (char ' ' <|> char '\n' <|> char '\r')

meta :: Parser Stream Meta
meta = do
  toggles <- Lexer.toggles <|> pure []
  char ';'
  paths <- many (white *> path')
  pure Meta {toggles, paths}

load :: FilePath -> IO Package
load root = do
  let package = root </> "package"
  raw <- Text.readFile package
  let Meta {toggles, paths} = parse meta $ startStream (pack package) raw
  modules <- for paths $ \path -> do
    let haskellName = root </> "header" </> haskell path
    header <- unsafeInterleaveIO $ do
      handle <- openFile haskellName ReadMode
      hSetEncoding handle utf8
      Text.hGetContents handle
    pure
      Module
        { name = pack haskellName,
          path = qualifiers path,
          header
        }
  pure
    Package
      { root,
        extensions = toggles,
        modules
      }
