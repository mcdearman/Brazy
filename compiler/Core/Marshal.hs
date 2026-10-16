-- |
-- What a function of C is given and gives, as C has them. This is kept
-- apart from how it is worked out ("Core.Foreign") so that the semantic
-- tree can hold it once it is worked out.
module Core.Marshal
  ( Marshal (..),
    Call (..),
  )
where

import Data.Text (Text)

-- | A value as it is passed to C, or given back: an @Int@ as a word, the
-- numbers of a fixed width as numbers of that width, a @Double@ and a
-- @Float@ as they are, a @Char@ as its code, a @Bool@ as an @int@ that is
-- not 0 for @True@, an address as C has one, and @()@ as nothing.
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
  | Pointer
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
