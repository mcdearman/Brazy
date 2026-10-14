module Generate.Precontext where

import Core.Tree.Declarations (Declarations (..))
import qualified Core.Tree.Declaration as Term (Declaration (..))
import qualified Core.Tree.Module as Core (Module (..))
import qualified Core.Tree.TypeDeclaration as Type (TypeDeclaration (..))
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Generate.Global (Global (Global))
import qualified Generate.Global as Global
import Generate.GlobalType (GlobalType (GlobalType))
import qualified Generate.GlobalType as GlobalType
import qualified Generate.Mangle as Mangle
import qualified Semantic.Index.Type as Type (Index (..))
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Scope as Scope
import Syntax.Variable (FullyQualifiedConstructorIdentifier ((:.=.)))

data Precontext = Precontext
  { terms :: !(Vector (Vector Global)),
    types :: !(Vector (Vector GlobalType))
  }

-- |
-- What every definition and every instance of some modules is called.
precontext :: Vector Core.Module -> Precontext
precontext modules =
  Precontext
    { terms = names <$> modules,
      types = instanceNames <$> modules
    }
  where
    names Core.Module {name = path, declarations = Declarations {terms}} = generate <$> terms
      where
        generate Term.Declaration {name} =
          Global
            { path,
              name = Mangle.mangle name
            }
    instanceNames
      Core.Module
        { name = path,
          declarations = Declarations {types, classInstances, dataInstances}
        } = Vector.zipWith3 generate types classInstances dataInstances
        where
          global name = Global {path, name}
          generate Type.TypeDeclaration {name} classInstances dataInstances =
            GlobalType
              { classInstances =
                  let go key _ = global $ single Mangle.Class key
                   in Map.mapWithKey go classInstances,
                dataInstances =
                  let go key _ = global $ single Mangle.Data key
                   in Map.mapWithKey go dataInstances
              }
            where
              single :: Mangle.Brand -> Type2.Index Scope.Global -> Text
              single brand = Mangle.mangleInstance target brand name
              target :: Type.Index Scope.Global -> FullyQualifiedConstructorIdentifier
              target (Type.Global global local) = targetPath :.=. targetName
                where
                  Core.Module
                    { name = targetPath,
                      declarations = Declarations {types}
                    } = modules Vector.! global
                  Type.TypeDeclaration {name = targetName} = types Vector.! local
