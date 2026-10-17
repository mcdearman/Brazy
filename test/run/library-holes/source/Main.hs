module Main where

import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators (count, count', eitherP, endBy1, manyTill, manyTill_, skipCount, skipManyTill, skipSome, someTill, someTill_)
import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty
import Data.Functor.Identity (Identity (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import GHC.Clock (getMonotonicTime)
import Numeric (showHFloat)

-- What was left of Data.Text, Data.Text.Lazy, the combinators of
-- applicatives, showHFloat and getMonotonicTime.

-- A parser of a list of characters, for the combinators.
newtype P a = P (String -> [(a, String)])

run :: P a -> String -> [(a, String)]
run (P p) = p

instance Functor P where
  fmap f (P p) = P (\s -> [(f a, rest) | (a, rest) <- p s])

instance Applicative P where
  pure a = P (\s -> [(a, s)])
  P f <*> P a = P (\s -> [(g x, rest') | (g, rest) <- f s, (x, rest') <- a rest])

instance Alternative P where
  empty = P (const [])
  P a <|> P b = P (\s -> a s ++ b s)

char :: Char -> P Char
char c = P (\s -> case s of (x : rest) | x == c -> [(c, rest)]; _ -> [])

main :: IO ()
main = do
  let t = T.pack "a::b::c"
  print (T.foldr (:) "" t, T.foldr' (\c n -> n + fromEnum c) 0 t, T.breakOnEnd (T.pack "::") t, T.breakOnAll (T.pack "::") t)
  print (T.commonPrefixes (T.pack "foobar") (T.pack "fooquux"), T.commonPrefixes (T.pack "x") (T.pack "y"), T.filter (/= ':') t)
  print (T.index t 2, T.findIndex (== 'b') t, T.copy t, T.measureOff 2 (T.pack "λx"), T.measureOff 5 (T.pack "ab"))
  print (runIdentity (T.spanM (pure . (/= ':')) t), runIdentity (T.spanEndM (pure . (/= ':')) t))
  let l = L.pack "one, two, three"
  print (L.breakOnEnd (L.pack ", ") l, L.breakOnAll (L.pack ", ") l, L.commonPrefixes l (L.pack "one, too"), L.fromStrict t)
  print (L.foldrChunks (\c n -> T.length c + n) 0 l, L.foldlChunks (\n c -> n + T.length c) 0 l)
  print (runIdentity (L.spanM (pure . (/= ',')) l), runIdentity (L.spanEndM (pure . (/= ' ')) l))
  print (run (count 2 (char 'a')) "aaa", run (count' 1 3 (char 'a')) "aab", run (eitherP (char 'x') (char 'y')) "y!")
  print (run (endBy1 (char 'a') (char ';')) "a;a;b", run (manyTill (char 'a') (char '.')) "aa.rest", run (manyTill_ (char 'a') (char '.')) "a.")
  print (run (someTill (char 'a') (char '.')) ".", run (someTill_ (char 'a') (char '.')) "aa.", run (skipSome (char 'a')) "aab", run (skipCount 2 (char 'a')) "aaa")
  print (run (skipManyTill (char 'a') (char 'b')) "aab", run (NonEmpty.someTill (char 'a') (char '.')) "aa.", run (NonEmpty.endBy1 (char 'a') (char ';')) "a;a;")
  print (map (\x -> showHFloat (x :: Double) "") [212.21, 0, 1, -0.5, 1 / 0, 0 / 0], showHFloat (-12.76 :: Float) "")
  start <- getMonotonicTime
  later <- getMonotonicTime
  print (later >= start, start >= 0)
