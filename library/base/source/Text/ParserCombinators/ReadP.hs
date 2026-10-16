-- |
-- Parsers that read text, as GHC's base has them: each alternative is gone
-- on with at once, character by character, so that none is held back, and
-- what each reads is given in the order of how much it read.
module Text.ParserCombinators.ReadP
  ( ReadP,
    get,
    look,
    (+++),
    (<++),
    gather,
    pfail,
    eof,
    satisfy,
    char,
    string,
    munch,
    munch1,
    skipSpaces,
    choice,
    count,
    between,
    option,
    optional,
    many,
    many1,
    skipMany,
    skipMany1,
    sepBy,
    sepBy1,
    endBy,
    endBy1,
    chainr,
    chainl,
    chainl1,
    chainr1,
    manyTill,
    ReadS,
    readP_to_S,
    readS_to_P,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus, ap, liftM2)
import Data.Char (isSpace)

infixr 5 +++, <++

-- |
-- What a parser is as it runs: waiting for the next character, or to look
-- at what is left, failed, done with a result and going on, or done with
-- the results it has whatever is left.
data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | -- | Never empty.
    Final [(a, String)]

instance Functor P where
  fmap f p = p >>= \a -> return (f a)

instance Applicative P where
  pure x = Result x Fail
  (<*>) = ap

instance Monad P where
  Get f >>= k = Get (\c -> f c >>= k)
  Look f >>= k = Look (\s -> f s >>= k)
  Fail >>= _ = Fail
  Result x p >>= k = k x <|> (p >>= k)
  Final rs >>= k = final [ys' | (x, s) <- rs, ys' <- run (k x) s]

instance MonadFail P where
  fail _ = Fail

instance Alternative P where
  empty = Fail

  -- Two that wait for a character wait together.
  Get f1 <|> Get f2 = Get (\c -> f1 c <|> f2 c)
  -- A result is given as soon as it can be.
  Result x p <|> q = Result x (p <|> q)
  p <|> Result x q = Result x (p <|> q)
  -- What fails is gone.
  Fail <|> p = p
  p <|> Fail = p
  -- What is done is put together, with one that looks.
  Final r <|> Final t = Final (r ++ t)
  Final r <|> Look f = Look (\s -> Final (r ++ run (f s) s))
  Final r <|> p = Look (\s -> Final (r ++ run p s))
  Look f <|> Final r = Look (\s -> Final (run (f s) s ++ r))
  p <|> Final r = Look (\s -> Final (run p s ++ r))
  -- Two that look look once, and one that looks looks first.
  Look f <|> Look g = Look (\s -> f s <|> g s)
  Look f <|> p = Look (\s -> f s <|> p)
  p <|> Look f = Look (\s -> p <|> f s)

instance MonadPlus P

final :: [(a, String)] -> P a
final [] = Fail
final r = Final r

run :: P a -> ReadS a
run (Get f) (c : s) = run (f c) s
run (Look f) s = run (f s) s
run (Result x p) s = (x, s) : run p s
run (Final rs) _ = rs
run _ _ = []

-- |
-- A parser, as what it goes on to: given what is done with its result, the
-- parser that does that too.
newtype ReadP a = R (forall b. (a -> P b) -> P b)

instance Functor ReadP where
  fmap h (R f) = R (\k -> f (k . h))

instance Applicative ReadP where
  pure x = R (\k -> k x)
  (<*>) = ap

instance Monad ReadP where
  R m >>= f = R (\k -> m (\a -> case f a of R m' -> m' k))

instance MonadFail ReadP where
  fail _ = R (\_ -> Fail)

instance Alternative ReadP where
  empty = pfail
  (<|>) = (+++)

instance MonadPlus ReadP

get :: ReadP Char
get = R Get

look :: ReadP String
look = R Look

pfail :: ReadP a
pfail = R (\_ -> Fail)

-- | Either, and both if both can.
(+++) :: ReadP a -> ReadP a -> ReadP a
R f1 +++ R f2 = R (\k -> f1 k <|> f2 k)

-- | The first, or the second only if the first reads nothing.
(<++) :: ReadP a -> ReadP a -> ReadP a
R f0 <++ q = do
  s <- look
  probe (f0 return) s 0
  where
    probe (Get f) (c : s) n = probe (f c) s (n + 1)
    probe (Look f) s n = probe (f s) s n
    probe p@(Result _ _) _ n = discard n >> R (p >>=)
    probe (Final r) _ _ = R (Final r >>=)
    probe _ _ _ = q
    discard :: Int -> ReadP ()
    discard 0 = return ()
    discard n = get >> discard (n - 1)

-- | What a parser reads, with what it gives.
gather :: ReadP a -> ReadP (String, a)
gather (R m) = R (\k -> gath id (m (\a -> return (\s -> k (s, a)))))
  where
    gath :: (String -> String) -> P (String -> P b) -> P b
    gath l (Get f) = Get (\c -> gath (l . (c :)) (f c))
    gath _ Fail = Fail
    gath l (Look f) = Look (\s -> gath l (f s))
    gath l (Result k p) = k (l []) <|> gath l p
    gath _ (Final _) = errorWithoutStackTrace "do not use readS_to_P in gather!"

satisfy :: (Char -> Bool) -> ReadP Char
satisfy p = do
  c <- get
  if p c then return c else pfail

char :: Char -> ReadP Char
char c = satisfy (c ==)

eof :: ReadP ()
eof = do
  s <- look
  if null s then return () else pfail

string :: String -> ReadP String
string this = do
  s <- look
  scan this s
  where
    scan [] _ = return this
    scan (x : xs) (y : ys) | x == y = do
      _ <- get
      scan xs ys
    scan _ _ = pfail

-- | As many as satisfy it, and no fewer.
munch :: (Char -> Bool) -> ReadP String
munch p = do
  s <- look
  scan s
  where
    scan (c : cs) | p c = do
      _ <- get
      s <- scan cs
      return (c : s)
    scan _ = return ""

munch1 :: (Char -> Bool) -> ReadP String
munch1 p = do
  c <- get
  if p c
    then do
      s <- munch p
      return (c : s)
    else pfail

choice :: [ReadP a] -> ReadP a
choice [] = pfail
choice [p] = p
choice (p : ps) = p +++ choice ps

skipSpaces :: ReadP ()
skipSpaces = do
  s <- look
  skip s
  where
    skip (c : s) | isSpace c = do
      _ <- get
      skip s
    skip _ = return ()

count :: Int -> ReadP a -> ReadP [a]
count n p = sequence (replicate n p)

between :: ReadP open -> ReadP close -> ReadP a -> ReadP a
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

option :: a -> ReadP a -> ReadP a
option x p = p +++ return x

optional :: ReadP a -> ReadP ()
optional p = (p >> return ()) +++ return ()

many :: ReadP a -> ReadP [a]
many p = return [] +++ many1 p

many1 :: ReadP a -> ReadP [a]
many1 p = liftM2 (:) p (many p)

skipMany :: ReadP a -> ReadP ()
skipMany p = many p >> return ()

skipMany1 :: ReadP a -> ReadP ()
skipMany1 p = p >> skipMany p

sepBy :: ReadP a -> ReadP sep -> ReadP [a]
sepBy p sep = sepBy1 p sep +++ return []

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

endBy :: ReadP a -> ReadP sep -> ReadP [a]
endBy p sep = many (do x <- p; _ <- sep; return x)

endBy1 :: ReadP a -> ReadP sep -> ReadP [a]
endBy1 p sep = many1 (do x <- p; _ <- sep; return x)

chainr :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainr p op x = chainr1 p op +++ return x

chainl :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainl p op x = chainl1 p op +++ return x

chainr1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainr1 p op = scan
  where
    scan = p >>= rest
    rest x =
      do
        f <- op
        y <- scan
        return (f x y)
        +++ return x

chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1 p op = p >>= rest
  where
    rest x =
      do
        f <- op
        y <- p
        rest (f x y)
        +++ return x

manyTill :: ReadP a -> ReadP end -> ReadP [a]
manyTill p end = scan
  where
    scan = (end >> return []) <++ liftM2 (:) p scan

readP_to_S :: ReadP a -> ReadS a
readP_to_S (R f) = run (f return)

readS_to_P :: ReadS a -> ReadP a
readS_to_P r =
  R (\k -> Look (\s -> final [bs'' | (a, s') <- r s, bs'' <- run (k a) s']))
