-- |
-- The builtin tuple types, one for each size. Unlike the other builtins
-- they are built directly in Core, not from source: a single constructor,
-- named @Tuple@ and the size (as @Tuple2@), with one lazy field for each
-- type parameter.
module Builtin.Tuple where

import Core.Instanciate (Store (Empty))
import Core.Tree.Constructor (ConstructorF (..), Shape (..))
import Core.Tree.Data (Data (Data), DefinitionF (..), TypesInfo (TypesNormal))
import qualified Core.Tree.Data as Data
import Core.Tree.Entry (EntryF (..))
import qualified Core.Tree.Forall as Forall (mono)
import qualified Core.Tree.Type as Type
import Data.Foldable (Foldable (toList))
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Local as Local
import Semantic.Tree.Constructor (Syntax (..))
import Syntax.Lexer (constructorIdentifier)
import qualified Syntax.Tree.Brand as Brand
import qualified Syntax.Variable as Variable

definition :: Int -> Data scope
definition n =
  Data
    { parameters = Strict.Vector.replicate n Type.typex,
      definition =
        Definition
          { position = Empty,
            types = TypesNormal,
            constructors = Strict.Vector.fromList $ toList set,
            selectors = Strict.Vector.empty,
            brand = Brand.Boxed
          }
    }
  where
    set = map go [minBound .. maxBound]
    go Constructor.Tuple =
      Constructor
        { brand = Empty,
          entries = Strict.Vector.generate n $ \n ->
            Entry
              { position = Empty,
                entry = Forall.mono Type.lazy (Type.Variable (Local.Local n)),
                levity = Type.lazy
              },
          syntax = Standard,
          shape = Ordinary,
          name = Variable.ConstructorIdentifier $ constructorIdentifier $ pack $ "Tuple" ++ show n
        }
