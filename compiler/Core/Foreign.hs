-- |
-- What a function of C is given and gives, as C has them, worked out from
-- the type written for it.
module Core.Foreign
  ( Marshal (..),
    Call (..),
    call,
  )
where

import Core.Tree.Forall (ForallOver (..))
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import Data.Text (Text)
import Data.Void (Void)
import Error (unsupportedForeignType)
import qualified Semantic.Index.Type2 as Type2
import Syntax.Position (Position)

-- | A value as it is passed to C, or given back: an @Int@ as a word, the
-- numbers of a fixed width as numbers of that width, a @Double@ and a
-- @Float@ as they are, a @Char@ as its code, a @Bool@ as an @int@ that is
-- not 0 for @True@, and @()@ as nothing.
data Marshal
  = Int
  | Int8
  | Int16
  | Int32
  | Int64
  | Word
  | Word8
  | Word16
  | Word32
  | Word64
  | Double
  | Float
  | Char
  | Bool
  | Unit
  deriving (Eq, Ord, Show)

-- | A call of a function of C, by its name: what it is given, what it gives,
-- and whether it is an action, as a function whose type ends in @IO@ is.
data Call = Call
  { symbol :: !Text,
    arguments :: ![Marshal],
    result :: !Marshal,
    action :: !Bool
  }
  deriving (Eq, Ord, Show)

call :: Position -> Text -> ForallOver TypeF Void scope -> Call
call position symbol ForallOver {parameters, result}
  | not (null parameters) = unsupportedForeignType position
  | otherwise = case go result of
      (arguments, result, action)
        | result `seq` null arguments && not action -> unsupportedForeignType position
        | otherwise -> Call {symbol, arguments, result, action}
  where
    go = \case
      Type.Function parameter rest -> case (value parameter, go rest) of
        (argument, (arguments, result, action)) -> argument `seq` (argument : arguments, result, action)
      Type.Call (Type.Constructor Type2.IO) given -> ([], answer given, True)
      given -> ([], answer given, False)
    answer = \case
      Type.Constructor (Type2.Tuple 0) -> Unit
      given -> value given
    value = \case
      Type.Constructor Type2.Int -> Int
      Type.Constructor Type2.Double -> Double
      Type.Constructor Type2.Float -> Float
      Type.Constructor Type2.Char -> Char
      Type.Constructor Type2.Bool -> Bool
      Type.Constructor Type2.Int8 -> Int8
      Type.Constructor Type2.Int16 -> Int16
      Type.Constructor Type2.Int32 -> Int32
      Type.Constructor Type2.Int64 -> Int64
      Type.Constructor Type2.Word -> Word
      Type.Constructor Type2.Word8 -> Word8
      Type.Constructor Type2.Word16 -> Word16
      Type.Constructor Type2.Word32 -> Word32
      Type.Constructor Type2.Word64 -> Word64
      _ -> unsupportedForeignType position
