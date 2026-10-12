module Semantic.Check.Simple.Constructor where

import Control.Monad.ST (ST)
import Core.Substitute (substituteType)
import Core.Tree.Constructor (Asked (..), Constructor (..), Shape (..))
import qualified Core.Tree.Type as Core
import qualified Data.Vector.Strict as Strict
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict.Vector
import Core.Tree.Type (TypeF (Variable))
import Data.Foldable (toList)
import Data.Maybe (catMaybes, isNothing)
import Semantic.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance), Use (..))
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import qualified Semantic.Check.EntryInstance as EntryInstance
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

instanciate ::
  Use ->
  Position ->
  Brand ->
  Strict.Vector (Unify.Type s scope) ->
  Constructor (Local ':+ scope) ->
  ST s (ConstructorInstance s scope)
instanciate use position brand fresh Constructor {name, syntax, entries, shape} = case shape of
  Ordinary ->
    pure
      ConstructorInstance
        { position,
          brand,
          name,
          syntax,
          entries = EntryInstance.instanciate position fresh <$> entries,
          result = fresh,
          indexed = False,
          refined = False <$ fresh,
          hidden = [],
          asked = [],
          equated = []
        }
  -- To make a value, the constructor's own variables are unknowns, of
  -- kinds that are not known either: what they are is found from how the
  -- constructor is used.
  --
  -- Where a value is taken apart, a variable is an unknown only if the type
  -- that is matched says what it is: if the constructor gives it to the
  -- type it makes, bare, as in @C :: a -> T a@. It is then what a parameter
  -- of the type is for a constructor declared the other way. Any other
  -- stands for a type that nothing outside the match can say, and stands
  -- for itself alone.
  Indexed {variables, asked, equated, arguments} -> do
    let places = settling [] (toList arguments)
        settled = catMaybes places
        open index = case use of
          Build -> True
          Match -> index `elem` settled
    own <- for (Strict.Vector.fromList [0 .. variables - 1]) $ \index -> do
      level <- Unify.fresh Core.universe
      kind <- Unify.fresh (Core.typeWith level)
      if open index then Unify.fresh kind else Unify.skolem kind
    pure
      ConstructorInstance
        { position,
          brand,
          name,
          syntax,
          entries = EntryInstance.instanciate position own <$> entries,
          result = substituteType (Strict.Vector.toLazy own) <$> arguments,
          indexed = True,
          refined = Strict.Vector.fromList (isNothing <$> places),
          hidden = [typex | (index, typex) <- zip [0 ..] (toList own), not (open index)],
          asked =
            [ (Type2.unlocal classx, substituteType (Strict.Vector.toLazy own) subject)
            | Asked {classx, subject} <- toList asked
            ],
          equated =
            [ (substituteType (Strict.Vector.toLazy own) left, substituteType (Strict.Vector.toLazy own) right)
            | (left, right) <- toList equated
            ]
        }
  where
    -- For each argument of the type that is made, the variable it settles,
    -- if it is one that no argument before it settles.
    settling :: [Int] -> [Core.Type (Local ':+ scope)] -> [Maybe Int]
    settling seen = \case
      [] -> []
      Variable (Local.Local index) : rest
        | index `notElem` seen -> Just index : settling (index : seen) rest
      _ : rest -> Nothing : settling seen rest
