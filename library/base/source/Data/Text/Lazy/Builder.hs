module Data.Text.Lazy.Builder
  ( Builder,
    toLazyText,
    toLazyTextWith,
    singleton,
    fromText,
    fromLazyText,
    fromString,
    flush,
  )
where

import qualified Data.Text as Strict (unpack)
import qualified Data.Text.Lazy as Lazy (pack, unpack)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import Hazy.Prelude (placeholder)

-- A builder is what it puts before whatever comes after it, so that two
-- are joined without either being gone over again.
newtype Builder = Builder ([Char] -> [Char])

instance Eq Builder where
  a == b = toLazyText a == toLazyText b

instance Ord Builder where
  compare a b = compare (toLazyText a) (toLazyText b)

instance Show Builder where
  showsPrec d builder = showsPrec d (toLazyText builder)

instance Semigroup Builder where
  Builder a <> Builder b = Builder (a . b)

instance Monoid Builder where
  mempty = Builder id

toLazyText :: Builder -> Lazy.Text
toLazyText (Builder build) = Lazy.pack (build [])

toLazyTextWith :: Int -> Builder -> Lazy.Text
toLazyTextWith _ = toLazyText

singleton :: Char -> Builder
singleton c = Builder (c :)

fromText :: Text -> Builder
fromText text = Builder (Strict.unpack text ++)

fromLazyText :: Lazy.Text -> Builder
fromLazyText text = Builder (Lazy.unpack text ++)

fromString :: String -> Builder
fromString characters = Builder (characters ++)

flush :: Builder
flush = Builder id
