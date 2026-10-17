module Control.Applicative.Combinators
  ( (<|>),
    many,
    some,
    optional,
    empty,
    between,
    choice,
    count,
    count',
    eitherP,
    endBy,
    endBy1,
    manyTill,
    manyTill_,
    someTill,
    someTill_,
    option,
    sepBy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    skipMany,
    skipSome,
    skipCount,
    skipManyTill,
    skipSomeTill,
  )
where

import Control.Applicative (Alternative (empty, many, some, (<|>)), optional)

between :: (Applicative m) => m open -> m close -> m a -> m a
between open close p = open *> p <* close

choice :: (Foldable f, Alternative m) => f (m a) -> m a
choice = foldr (<|>) empty

count :: (Applicative m) => Int -> m a -> m [a]
count n p = if n <= 0 then pure [] else (:) <$> p <*> count (n - 1) p

count' :: (Alternative m) => Int -> Int -> m a -> m [a]
-- | At least the first many, and at most the second.
count' atLeast atMost p
  | atMost <= 0 || atLeast > atMost = pure []
  | atLeast > 0 = (:) <$> p <*> count' (atLeast - 1) (atMost - 1) p
  | otherwise = ((:) <$> p <*> count' 0 (atMost - 1) p) <|> pure []

eitherP :: (Alternative m) => m a -> m b -> m (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)

endBy :: (Alternative m) => m a -> m sep -> m [a]
endBy p sep = many (p <* sep)

endBy1 :: (Alternative m) => m a -> m sep -> m [a]
endBy1 p sep = some (p <* sep)

manyTill :: (Alternative m) => m a -> m end -> m [a]
manyTill p end = go
  where
    go = ([] <$ end) <|> ((:) <$> p <*> go)

manyTill_ :: (Alternative m) => m a -> m end -> m ([a], end)
manyTill_ p end = go
  where
    go = ((\final -> ([], final)) <$> end) <|> ((\x (xs, final) -> (x : xs, final)) <$> p <*> go)

someTill :: (Alternative m) => m a -> m end -> m [a]
someTill p end = (:) <$> p <*> manyTill p end

someTill_ :: (Alternative m) => m a -> m end -> m ([a], end)
someTill_ p end = (\x (xs, final) -> (x : xs, final)) <$> p <*> manyTill_ p end

option :: (Alternative m) => a -> m a -> m a
option x p = p <|> pure x

sepBy :: (Alternative m) => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: (Alternative m) => m a -> m sep -> m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepEndBy :: (Alternative m) => m a -> m sep -> m [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

sepEndBy1 :: (Alternative m) => m a -> m sep -> m [a]
sepEndBy1 p sep = (:) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

skipMany :: (Alternative m) => m a -> m ()
skipMany p = (p *> skipMany p) <|> pure ()

skipSome :: (Alternative m) => m a -> m ()
skipSome p = p *> skipMany p

skipCount :: (Applicative m) => Int -> m a -> m ()
skipCount n p = () <$ count n p

skipManyTill :: (Alternative m) => m a -> m end -> m end
skipManyTill p end = go
  where
    go = end <|> (p *> go)

skipSomeTill :: (Alternative m) => m a -> m end -> m end
skipSomeTill p end = p *> skipManyTill p end
