-- |
-- What a function of C is given and gives, as C has them, worked out from
-- the type written for it.
module Core.Foreign
  ( Marshal (..),
    Call (..),
    Target (..),
    call,
    callThrough,
  )
where

import Control.Monad.ST (ST)
import Core.Marshal (Call (..), Marshal (..), Target (..))
import Core.Substitute (substituteType)
import qualified Core.Tree.Constraints as Constraints (ConstraintsF (..))
import Core.Tree.Constructor (ConstructorF (Constructor, entries))
import Core.Tree.Data (Data (Data, brand, constructors))
import Core.Tree.Entry (EntryF (Entry, entry))
import Core.Tree.Forall (ForallOver (..))
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Void (Void)
import Error (unsupportedForeignType)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import Syntax.Position (Position)
import qualified Syntax.Tree.Brand as Brand

-- | A call, where the types are only the ones C is given as they are.
call :: Position -> Target -> Text -> ForallOver TypeF Void scope -> Call
call position target symbol typex = runIdentity (callWith (const (pure Nothing)) position target symbol typex)

-- |
-- A call, where a newtype of a type C is given is given as what it holds,
-- as the report has it. What each type is is asked of the definitions of
-- the types.
callThrough ::
  (Type2.Index scope -> ST s (Data scope)) ->
  Position ->
  Target ->
  Text ->
  ForallOver TypeF Void scope ->
  ST s Call
callThrough definition = callWith unwrap
  where
    unwrap typex = case spine typex [] of
      (Type.Constructor (Type2.Index index), arguments)
        | Just found <- Shift.partialUnshift Nothing (Type2.Index index) -> do
            Data {brand, constructors} <- definition found
            pure $ case (brand, toList constructors) of
              (Brand.Newtype, [Constructor {entries}])
                | [Entry {entry = ForallOver {parameters, result}}] <- toList entries,
                  null parameters ->
                    Just (held arguments result)
              _ -> Nothing
      _ -> pure Nothing
    spine (Type.Call function argument) arguments = spine function (argument : arguments)
    spine function arguments = (function, arguments)
    -- What the newtype holds, with its parameters made what it is given.
    held ::
      [TypeF Void (Local ':+ scope)] ->
      TypeF Void (Local ':+ (Local ':+ scope)) ->
      TypeF Void (Local ':+ scope)
    held arguments result =
      substituteType (Vector.fromList arguments) $
        Shift.map (Shift.Over Shift.Shift) (substituteType Vector.empty result)

-- A type may have variables, as the type an address is of; but what is
-- required of them would be given to the function, which C does not take.
callWith ::
  (Monad m) =>
  (TypeF Void (Local ':+ scope) -> m (Maybe (TypeF Void (Local ':+ scope)))) ->
  Position ->
  Target ->
  Text ->
  ForallOver TypeF Void scope ->
  m Call
callWith unwrap position target symbol ForallOver {constraints, result = typex}
  | Constraints.Constraints required <- constraints,
    not (null required) =
      unsupportedForeignType position
  | otherwise = do
      (arguments, result, action) <- go (maybe typex id wrapped)
      -- A function called by its name is given something, or is an action;
      -- an address is an address, and nothing else; and a function called
      -- by its address is given that first.
      let fits = case target of
            Static -> not (null arguments) || action
            Address -> null arguments && not action && result == Pointer
            Dynamic -> take 1 arguments == [Pointer] && addressOfRest
            Export -> True
            Wrapper -> wrapped /= Nothing
      if fits
        then pure Call {target, symbol, arguments, result, action}
        else unsupportedForeignType position
  where
    -- A wrapper makes the address of a function of the type it is given,
    -- as the report has it. What C gives and gets is what that function is
    -- given and gives.
    wrapped = case typex of
      Type.Function function (Type.Call (Type.Constructor Type2.IO) (Type.Call (Type.Constructor Type2.FunPtr) made))
        | function == made -> Just function
      _ -> Nothing
    -- The address a function is called by is the address of a function of
    -- the type of the rest, as the report has it.
    addressOfRest = case typex of
      Type.Function (Type.Call (Type.Constructor Type2.FunPtr) function) rest -> function == rest
      _ -> False
    go = \case
      Type.Function parameter rest -> do
        argument <- value parameter
        (arguments, result, action) <- go rest
        argument `seq` pure (argument : arguments, result, action)
      Type.Call (Type.Constructor Type2.IO) given -> do
        result <- answer given
        result `seq` pure ([], result, True)
      given -> do
        result <- answer given
        result `seq` pure ([], result, False)
    answer = \case
      Type.Constructor (Type2.Tuple 0) -> pure Unit
      given -> value given
    value given = case given of
      Type.Constructor Type2.Int -> pure Int
      Type.Constructor Type2.Double -> pure Double
      Type.Constructor Type2.Float -> pure Float
      Type.Constructor Type2.Char -> pure Char
      Type.Constructor Type2.Bool -> pure Bool
      Type.Call (Type.Constructor Type2.Ptr) _ -> pure Pointer
      Type.Call (Type.Constructor Type2.FunPtr) _ -> pure Pointer
      Type.Constructor Type2.Int8 -> pure Int8
      Type.Constructor Type2.Int16 -> pure Int16
      Type.Constructor Type2.Int32 -> pure Int32
      Type.Constructor Type2.Int64 -> pure Int64
      Type.Constructor Type2.Word -> pure Word
      Type.Constructor Type2.Word8 -> pure Word8
      Type.Constructor Type2.Word16 -> pure Word16
      Type.Constructor Type2.Word32 -> pure Word32
      Type.Constructor Type2.Word64 -> pure Word64
      _ -> do
        held <- unwrap given
        case held of
          Just held -> value held
          Nothing -> unsupportedForeignType position
