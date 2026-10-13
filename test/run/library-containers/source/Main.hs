module Main where

import Data.Char (chr, digitToInt, intToDigit, isHexDigit, isPunctuation, isUpper, ord, toLower, toUpper)
import Data.List (dropWhileEnd, group, intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, partition, sort, sortOn, transpose)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import Numeric (showHex)
import Text.Read (readMaybe)

-- Numbers in no order, from a seed.
scrambled :: Int -> Int -> [Int]
scrambled count seed = take count (iterate (\x -> (x * 37 + 11) `mod` 1009) seed)

squares :: Map Int Int
squares = Map.fromList [(x, x * x) | x <- scrambled 300 5]

evens, thirds :: Set Int
evens = Set.fromList [x | x <- scrambled 400 3, even x]
thirds = Set.fromList [x | x <- scrambled 400 7, x `mod` 3 == 0]

main :: IO ()
main = do
  -- Maps.
  print (Map.size squares, Map.lookup 5 squares, Map.lookup 4 squares, Map.member 196 squares)
  print (take 8 (Map.toList squares))
  print (take 5 (Map.toDescList squares))
  print (Map.keys squares == sort (nub (scrambled 300 5)))
  let fewer = foldr Map.delete squares (scrambled 150 5)
  print (Map.size fewer, take 6 (Map.keys fewer), Map.lookupMin fewer, Map.lookupMax fewer)
  print (Map.toList (Map.fromListWith (++) [(x `mod` 5, [x]) | x <- [1 .. 12 :: Int]]))
  print (Map.toList (Map.insertWith (+) 3 10 (Map.fromList [(3, 1), (4, 2 :: Int)])) :: [(Int, Int)])
  let a = Map.fromList [(x, "a" ++ show x) | x <- [1 .. 10 :: Int]]
      b = Map.fromList [(x, "b" ++ show x) | x <- [6 .. 14 :: Int]]
  print (Map.elems (Map.union a b))
  print (Map.toList (Map.unionWith (++) a b))
  print (Map.toList (Map.intersectionWith (++) a b))
  print (Map.keys (Map.difference a b), Map.keys (a Map.\\ b))
  print (Map.toList (Map.filterWithKey (\k _ -> odd k) a))
  print (Map.toList (Map.mapWithKey (\k x -> show k ++ x) (Map.filter (/= "a3") a)))
  print (Map.toList (Map.mapKeys (`div` 2) a))
  print (Map.foldrWithKey (\k x rest -> k + length x + rest) 0 a, sum (Map.map length a), length a)
  print (Map.findWithDefault "none" 20 a, a Map.! 4, a Map.!? 40)
  print (Map.toList (Map.alter (fmap (++ "!")) 2 (Map.alter (const (Just "new")) 0 (Map.alter (const Nothing) 1 a))))
  print (Map.lookupLT 6 b, Map.lookupGE 15 b, Map.lookupLE 20 b, Map.lookupGT 6 b)
  print (Map.split 8 a)
  print (Map.keysSet a, Map.fromSet (* 2) (Set.fromList [3, 1, 2 :: Int]))
  print (fmap length a == Map.map length a, a == a, a == Map.map id b, compare a b)
  print (Map.toList (Map.unions [a, b, Map.singleton 99 "z"]) == Map.toList (a <> b <> Map.singleton 99 "z"))
  print (Map.minViewWithKey a, fmap fst (Map.maxView a))
  print (Map.elemAt 3 a, Map.lookupIndex 7 a, Map.toList (Map.take 2 a), Map.keys (Map.drop 8 a))
  print (Map.toList (Map.mapMaybe (\x -> if length x > 2 then Just (length x) else Nothing) a))
  print (Map.partition (> "a5") a)
  print =<< traverse (\x -> pure (x ++ "?")) (Map.take 3 a)
  print (Map.fromAscList [(1 :: Int, 'a'), (1, 'b'), (2, 'c')], Map.fromDistinctAscList [(1 :: Int, 'a'), (2, 'b')])
  print (Map.restrictKeys a (Set.fromList [2, 4, 40]), Map.withoutKeys a (Set.fromList [1 .. 8]))
  -- Sets.
  print (Set.size evens, Set.size thirds, take 6 (Set.toList evens), take 6 (Set.toDescList thirds))
  print (take 10 (Set.toList (Set.union evens thirds)), Set.size (Set.union evens thirds))
  print (take 10 (Set.toList (Set.intersection evens thirds)), Set.size (Set.intersection evens thirds))
  print (take 10 (Set.toList (Set.difference evens thirds)), Set.size (evens Set.\\ thirds))
  print (Set.member 6 evens, Set.notMember 6 evens, Set.member 7 evens)
  print (Set.isSubsetOf (Set.intersection evens thirds) evens, Set.isSubsetOf evens thirds)
  let small = Set.fromList "mississippi"
  print (small, Set.toList (Set.insert 'a' small), Set.toList (Set.delete 's' small))
  print (Set.minView small, Set.maxView small, Set.lookupIndex 'p' small, Set.elemAt 1 small)
  print (Set.filter (> 'm') small, Set.partition (> 'm') small, Set.split 'n' small)
  print (Set.map toUpper small, Set.null small, Set.null (Set.empty :: Set Int), length small, sum (Set.fromList [1, 2, 3, 2 :: Int]))
  print (Set.fromAscList [1, 1, 2, 3, 3 :: Int], Set.unions [small, Set.singleton 'z'], elem 'm' small)
  print (small == Set.fromList "imps", compare small (Set.fromList "imp"), Set.findMin small, Set.findMax small)
  -- Vectors.
  let v = Vector.fromList (scrambled 20 2)
  print (v, Vector.length v, v Vector.! 0, v Vector.! 19, v Vector.!? 20)
  print (Vector.map (* 2) v Vector.! 7, Vector.imap (+) v, Vector.head v, Vector.last v)
  print (Vector.generate 7 (* 3), Vector.replicate 3 'x', Vector.singleton 1 <> Vector.fromList [2, 3 :: Int])
  print (Vector.zipWith (+) v (Vector.drop 15 v), Vector.izipWith (\i x y -> i + x - y) v v)
  print (Vector.toList (Vector.take 3 v), Vector.catMaybes (Vector.fromList [Just 1, Nothing, Just (3 :: Int)]))
  print (Vector.accum (+) (Vector.replicate 4 (0 :: Int)) [(1, 5), (3, 2), (1, 1)], Vector.fromList "abc" Vector.// [(1, 'x')])
  print (sum v, maximum v, foldr (:) [] (Vector.take 4 v), fmap negate (Vector.take 2 v), null v, length v)
  print =<< Vector.replicateM 2 (pure 'q')
  print =<< traverse (pure . (+ 1)) (Vector.take 3 v)
  print (Vector.zipWith3 (,,) v v v Vector.! 1, Vector.zipWith4 (\a b c d -> a + b + c + d) v v v v Vector.! 1)
  print (Vector.uncons (Vector.fromList "ab"), Vector.reverse (Vector.fromList "abc"), Vector.filter even v)
  -- Whatever of a lazy vector is not looked at is never made.
  print (Vector.length (Vector.fromList [undefined, 1 :: Int]), Vector.fromList [undefined, 7 :: Int] Vector.! 1)
  let s = Strict.fromList (scrambled 9 4)
  print (s, Strict.toLazy s, Strict.fromLazy (Vector.take 2 v), Strict.imap (*) s, Strict.unzip (Strict.map (\x -> (x, even x)) s))
  print (s Strict.// [(0, 0), (8, 8)], Strict.fromListN 2 "abc", Strict.last s, Strict.uncons s, Strict.zipWith (-) s s)
  print =<< Strict.imapM (\i x -> pure (i * x)) s
  print =<< Strict.izipWithM (\i x y -> pure (i + x + y)) s s
  print (Strict.generate 4 show, Strict.replicate 2 True, Strict.head s, Strict.empty == (Strict.empty :: Strict.Vector Int))
  -- Text.
  let t = Text.pack "The quick brown fox, the lazy dog"
  print (Text.length t, Text.words t, Text.splitOn (Text.pack ", ") t, Text.toLower t, Text.toUpper (Text.take 9 t))
  print (Text.intercalate (Text.pack "-") (Text.words t), Text.isPrefixOf (Text.pack "The") t, Text.stripSuffix (Text.pack "dog") t)
  print (Text.uncons t, Text.snoc (Text.take 3 t) '!', Text.head t, Text.tail (Text.take 4 t), Text.drop 30 t, Text.null Text.empty)
  print (Text.any (== 'z') t, Text.foldl (\n _ -> n + 1) (0 :: Int) t, Text.map succ (Text.take 3 t), Text.takeWhile (/= ' ') t)
  print (Text.unpack (Text.singleton 'x'), Text.strip (Text.pack "  in  "), Text.replace (Text.pack "the") (Text.pack "a") t)
  print (Text.breakOn (Text.pack "fox") t, Text.lines (Text.pack "one\ntwo\n"), Text.reverse (Text.take 5 t), Text.count (Text.pack "he") t)
  print (Text.unwords [Text.pack "a", Text.pack "b"], Text.split (== ',') (Text.pack "a,b,,c"), Text.isInfixOf (Text.pack "lazy") t)
  let l = Lazy.pack "lazy text"
  print (Lazy.length l, Lazy.unpack l, Lazy.toStrict l, l <> Lazy.fromStrict (Text.pack "!"), Lazy.take 4 l)
  let built = Builder.fromString "one" <> Builder.singleton ' ' <> Builder.fromText (Text.pack "two") <> mempty <> Builder.fromLazyText l
  print (Builder.toLazyText built, built == built)
  -- Lists and characters.
  print (sort (scrambled 30 9), sortOn negate (scrambled 10 9), nub (map (`mod` 7) (scrambled 30 9)))
  print (intercalate ", " ["a", "b", "c"], dropWhileEnd (== ' ') "trail  ", isPrefixOf "ab" "abc", isSuffixOf "bc" "abc", isInfixOf "xx" "abc")
  print (group "aabccc", partition even [1 .. 10 :: Int], transpose ["abc", "de", "f"])
  print (sort "the quick brown fox", sort [(2 :: Int, 'b'), (1, 'z'), (2, 'a')], sort ([] :: [Int]), sort [3, 2, 1 :: Int])
  print (NonEmpty.toList (NonEmpty.reverse (NonEmpty.fromList "abc")), NonEmpty.last (NonEmpty.cons 'x' (NonEmpty.singleton 'y')))
  print (NonEmpty.toList (NonEmpty.sortOn negate (NonEmpty.fromList [1, 3, 2 :: Int])), NonEmpty.head (NonEmpty.fromList "q"))
  print (catMaybes [Just 1, Nothing, Just (2 :: Int)], mapMaybe (\x -> if x > 1 then Just (x * 2) else Nothing) [1, 2, 3 :: Int], fromMaybe 0 (Nothing :: Maybe Int))
  print (map toUpper "straße é", map toLower "ÀB", isUpper 'A', isUpper 'a', isPunctuation '!', isHexDigit 'g')
  print (digitToInt 'f', intToDigit 11, ord 'a', chr 955, showHex (255 :: Int) "", showHex (4096 :: Int) "h")
  print (readMaybe "12" :: Maybe Int, readMaybe "12x" :: Maybe Int, readMaybe " 7 " :: Maybe Int)
