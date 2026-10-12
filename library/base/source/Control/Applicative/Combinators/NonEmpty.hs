module Control.Applicative.Combinators.NonEmpty
  ( some,
    endBy1,
    someTill,
    sepBy1,
    sepEndBy1,
  )
where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Applicative.Combinators (sepEndBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Hazy.Prelude (placeholder)

some :: (Alternative m) => m a -> m (NonEmpty a)
some p = (:|) <$> p <*> many p

endBy1 :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
endBy1 p sep = some (p <* sep)

someTill :: (Alternative m) => m a -> m end -> m (NonEmpty a)
someTill = placeholder

sepBy1 :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
sepBy1 p sep = (:|) <$> p <*> many (sep *> p)

sepEndBy1 :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
sepEndBy1 p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])
