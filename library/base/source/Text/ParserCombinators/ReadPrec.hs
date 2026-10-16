-- |
-- Parsers that know the precedence they are read at, as GHC's base has
-- them.
module Text.ParserCombinators.ReadPrec
  ( ReadPrec,
    Prec,
    minPrec,
    lift,
    prec,
    step,
    reset,
    get,
    look,
    (+++),
    (<++),
    pfail,
    choice,
    readPrec_to_P,
    readP_to_Prec,
    readPrec_to_S,
    readS_to_Prec,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus, ap)
import Text.ParserCombinators.ReadP (ReadP, ReadS)
import qualified Text.ParserCombinators.ReadP as ReadP

infixr 5 +++, <++

newtype ReadPrec a = P (Prec -> ReadP a)

type Prec = Int

instance Functor ReadPrec where
  fmap h (P f) = P (\n -> fmap h (f n))

instance Applicative ReadPrec where
  pure x = P (\_ -> pure x)
  (<*>) = ap

instance Monad ReadPrec where
  P f >>= k = P (\n -> f n >>= \a -> let P f' = k a in f' n)

instance MonadFail ReadPrec where
  fail s = P (\_ -> fail s)

instance Alternative ReadPrec where
  empty = pfail
  (<|>) = (+++)

instance MonadPlus ReadPrec

minPrec :: Prec
minPrec = 0

lift :: ReadP a -> ReadPrec a
lift m = P (\_ -> m)

-- | One step further in, as what a constructor holds is.
step :: ReadPrec a -> ReadPrec a
step (P f) = P (\n -> f (n + 1))

-- | From the start again, as what is in brackets is.
reset :: ReadPrec a -> ReadPrec a
reset (P f) = P (\_ -> f minPrec)

-- | Only at a precedence of at most this, and then at this.
prec :: Prec -> ReadPrec a -> ReadPrec a
prec n (P f) = P (\c -> if c <= n then f n else ReadP.pfail)

get :: ReadPrec Char
get = lift ReadP.get

look :: ReadPrec String
look = lift ReadP.look

(+++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
P f1 +++ P f2 = P (\n -> f1 n ReadP.+++ f2 n)

(<++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
P f1 <++ P f2 = P (\n -> f1 n ReadP.<++ f2 n)

pfail :: ReadPrec a
pfail = lift ReadP.pfail

choice :: [ReadPrec a] -> ReadPrec a
choice ps = foldr (+++) pfail ps

readPrec_to_P :: ReadPrec a -> Int -> ReadP a
readPrec_to_P (P f) = f

readP_to_Prec :: (Int -> ReadP a) -> ReadPrec a
readP_to_Prec f = P f

readPrec_to_S :: ReadPrec a -> Int -> ReadS a
readPrec_to_S (P f) n = ReadP.readP_to_S (f n)

readS_to_Prec :: (Int -> ReadS a) -> ReadPrec a
readS_to_Prec f = P (\n -> ReadP.readS_to_P (f n))
