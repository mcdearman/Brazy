module Data.Text
  ( Text,
    pack,
    unpack,
    singleton,
    empty,
    cons,
    snoc,
    append,
    uncons,
    unsnoc,
    head,
    last,
    tail,
    init,
    null,
    length,
    compareLength,
    map,
    intercalate,
    intersperse,
    transpose,
    reverse,
    replace,
    toCaseFold,
    toLower,
    toUpper,
    toTitle,
    justifyLeft,
    justifyRight,
    center,
    foldl,
    foldl,
    foldl1,
    foldl1,
    foldr,
    foldr,
    foldr1,
    concat,
    concatMap,
    any,
    all,
    maximum,
    minimum,
    isAscii,
    scanl,
    scanl1,
    scanr,
    scanr1,
    mapAccumL,
    mapAccumR,
    replicate,
    unfoldr,
    unfoldrN,
    take,
    takeEnd,
    drop,
    dropEnd,
    takeWhile,
    takeWhileEnd,
    dropWhile,
    dropWhileEnd,
    dropAround,
    strip,
    stripStart,
    stripEnd,
    splitAt,
    breakOn,
    breakOnEnd,
    break,
    span,
    spanM,
    spanEndM,
    group,
    groupBy,
    inits,
    tails,
    splitOn,
    split,
    chunksOf,
    lines,
    words,
    unlines,
    unwords,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,
    stripPrefix,
    stripSuffix,
    commonPrefixes,
    filter,
    breakOnAll,
    find,
    elem,
    partition,
    index,
    findIndex,
    count,
    zip,
    zipWith,
    copy,
    measureOff,
  )
where

import qualified Data.Char as Char (isAlpha, isAscii, isSpace, toLower, toUpper)
import qualified Data.List as List
  ( dropWhileEnd, groupBy, inits, intercalate, intersperse, isInfixOf, isPrefixOf, isSuffixOf,
    stripPrefix, tails, transpose )
import Hazy
  ( primTextAppend, primTextHead, primTextLength, primTextNull, primTextSnoc, primTextTail )
import qualified Hazy.Prelude as Hazy (unpack)
import Prelude hiding (all, any, break, concat, concatMap, drop, dropWhile, elem, filter, foldl, foldl1, foldr, foldr1, head, init, last, length, lines, map, maximum, minimum, null, replicate, reverse, scanl, scanl1, scanr, scanr1, span, splitAt, tail, take, takeWhile, unlines, unwords, words, zip, zipWith)
import qualified Prelude
import Hazy.Prelude (Text, pack, placeholder)

unpack :: Text -> String
unpack = Hazy.unpack

singleton :: Char -> Text
singleton c = primTextSnoc (pack []) c

empty :: Text
empty = pack []

infixr 5 `cons`

cons :: Char -> Text -> Text
cons c text = primTextAppend (singleton c) text

snoc :: Text -> Char -> Text
snoc = primTextSnoc

append :: Text -> Text -> Text
append = primTextAppend

uncons :: Text -> Maybe (Char, Text)
uncons text
  | primTextNull text = Nothing
  | otherwise = Just (primTextHead text, primTextTail text)

unsnoc :: Text -> Maybe (Text, Char)
unsnoc text = case unpack text of
  [] -> Nothing
  characters -> Just (pack (Prelude.init characters), Prelude.last characters)

head :: Text -> Char
head text
  | primTextNull text = error "Data.Text.head: empty text"
  | otherwise = primTextHead text

last :: Text -> Char
last text = Prelude.last (unpack text)

tail :: Text -> Text
tail text
  | primTextNull text = error "Data.Text.tail: empty text"
  | otherwise = primTextTail text

init :: Text -> Text
init = lift Prelude.init

null :: Text -> Bool
null = primTextNull

length :: Text -> Int
length = primTextLength

compareLength :: Text -> Int -> Ordering
compareLength text count = compare (length text) count

map :: (Char -> Char) -> Text -> Text
map f = lift (Prelude.map f)

intercalate :: Text -> [Text] -> Text
intercalate separator texts = pack (List.intercalate (unpack separator) (Prelude.map unpack texts))

intersperse :: Char -> Text -> Text
intersperse c = lift (List.intersperse c)

transpose :: [Text] -> [Text]
transpose texts = Prelude.map pack (List.transpose (Prelude.map unpack texts))

reverse :: Text -> Text
reverse = lift Prelude.reverse

replace :: Text -> Text -> Text -> Text
replace needle with text = intercalate with (splitOn needle text)

toCaseFold :: Text -> Text
toCaseFold = toLower

toLower :: Text -> Text
toLower = map Char.toLower

toUpper :: Text -> Text
toUpper = map Char.toUpper

toTitle :: Text -> Text
toTitle = lift title
  where
    title [] = []
    title (c : rest)
      | Char.isAlpha c = Char.toUpper c : inside rest
      | otherwise = c : title rest
    inside [] = []
    inside (c : rest)
      | Char.isAlpha c = Char.toLower c : inside rest
      | otherwise = c : title rest

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft width c = lift (\characters -> characters ++ Prelude.replicate (measure width - Prelude.length characters) c)

justifyRight :: Int -> Char -> Text -> Text
justifyRight width c = lift (\characters -> Prelude.replicate (measure width - Prelude.length characters) c ++ characters)

center :: Int -> Char -> Text -> Text
center width c = lift centre
  where
    centre characters =
      let missing = measure width - Prelude.length characters
          right = missing `div` 2
       in Prelude.replicate (missing - right) c ++ characters ++ Prelude.replicate right c

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z text = Prelude.foldl f z (unpack text)

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z text = strictly z (unpack text)
  where
    strictly state [] = state
    strictly state (c : rest) = let next = f state c in next `seq` strictly next rest

foldl1 :: (Char -> Char -> Char) -> Text -> Char
foldl1 f text = Prelude.foldl1 f (unpack text)

foldl1' :: (Char -> Char -> Char) -> Text -> Char
foldl1' f text = Prelude.foldl1 f (unpack text)

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z text = Prelude.foldr f z (unpack text)

foldr' :: (Char -> a -> a) -> a -> Text -> a
foldr' = placeholder

foldr1 :: (Char -> Char -> Char) -> Text -> Char
foldr1 f text = Prelude.foldr1 f (unpack text)

concat :: [Text] -> Text
concat texts = pack (Prelude.concatMap unpack texts)

concatMap :: (Char -> Text) -> Text -> Text
concatMap f = lift (Prelude.concatMap (unpack . f))

any :: (Char -> Bool) -> Text -> Bool
any p text = Prelude.any p (unpack text)

all :: (Char -> Bool) -> Text -> Bool
all p text = Prelude.all p (unpack text)

maximum :: Text -> Char
maximum text = Prelude.maximum (unpack text)

minimum :: Text -> Char
minimum text = Prelude.minimum (unpack text)

isAscii :: Text -> Bool
isAscii = all Char.isAscii

scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z = lift (Prelude.scanl f z)

scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 f = lift (Prelude.scanl1 f)

scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr f z = lift (Prelude.scanr f z)

scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 f = lift (Prelude.scanr1 f)

mapAccumL :: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumL f state text = case go state (unpack text) of
  (final, characters) -> (final, pack characters)
  where
    go state [] = (state, [])
    go state (c : rest) = case f state c of
      (next, d) -> case go next rest of
        (final, others) -> (final, d : others)

mapAccumR :: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumR f state text = case go (unpack text) of
  (final, characters) -> (final, pack characters)
  where
    go [] = (state, [])
    go (c : rest) = case go rest of
      (after, others) -> case f after c of
        (final, d) -> (final, d : others)

replicate :: Int -> Text -> Text
replicate count text = pack (Prelude.concat (Prelude.replicate (measure count) (unpack text)))

unfoldr :: (a -> Maybe (Char, a)) -> a -> Text
unfoldr f seed = pack (go seed)
  where
    go state = case f state of
      Just (c, next) -> c : go next
      Nothing -> []

unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> Text
unfoldrN count f seed = pack (go (measure count) seed)
  where
    go left state
      | left <= 0 = []
      | otherwise = case f state of
          Just (c, next) -> c : go (left - 1) next
          Nothing -> []

take :: Int -> Text -> Text
take count = lift (Prelude.take (measure count))

takeEnd :: Int -> Text -> Text
takeEnd count = lift (\characters -> Prelude.drop (Prelude.length characters - measure count) characters)

drop :: Int -> Text -> Text
drop count = lift (Prelude.drop (measure count))

dropEnd :: Int -> Text -> Text
dropEnd count = lift (\characters -> Prelude.take (Prelude.length characters - measure count) characters)

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p = lift (Prelude.takeWhile p)

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p = lift (Prelude.reverse . Prelude.takeWhile p . Prelude.reverse)

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p = lift (Prelude.dropWhile p)

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = lift (List.dropWhileEnd p)

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = lift (List.dropWhileEnd p . Prelude.dropWhile p)

strip :: Text -> Text
strip = dropAround Char.isSpace

stripStart :: Text -> Text
stripStart = dropWhile Char.isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd Char.isSpace

splitAt :: Int -> Text -> (Text, Text)
splitAt count text = case Prelude.splitAt (measure count) (unpack text) of
  (before, after) -> (pack before, pack after)

breakOn :: Text -> Text -> (Text, Text)
breakOn needle text = case found (unpack needle) (unpack text) of
  (before, after) -> (pack before, pack after)

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd = placeholder

break :: (Char -> Bool) -> Text -> (Text, Text)
break p text = case Prelude.break p (unpack text) of
  (before, after) -> (pack before, pack after)

span :: (Char -> Bool) -> Text -> (Text, Text)
span p text = case Prelude.span p (unpack text) of
  (before, after) -> (pack before, pack after)

spanM :: (Monad m) => (Char -> m Bool) -> Text -> m (Text, Text)
spanM = placeholder

spanEndM :: (Monad m) => (Char -> m Bool) -> Text -> m (Text, Text)
spanEndM = placeholder

group :: Text -> [Text]
group = groupBy (==)

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy same text = Prelude.map pack (List.groupBy same (unpack text))

inits :: Text -> [Text]
inits text = Prelude.map pack (List.inits (unpack text))

tails :: Text -> [Text]
tails text = Prelude.map pack (List.tails (unpack text))

splitOn :: Text -> Text -> [Text]
splitOn needle text
  | null needle = error "Text.splitOn: empty delimiter"
  | otherwise = Prelude.map pack (go (unpack text))
  where
    separator = unpack needle
    width = Prelude.length separator
    go characters = case found separator characters of
      (before, []) -> [before]
      (before, after) -> before : go (Prelude.drop width after)

split :: (Char -> Bool) -> Text -> [Text]
split p text = Prelude.map pack (go (unpack text))
  where
    go characters = case Prelude.break p characters of
      (before, []) -> [before]
      (before, _ : after) -> before : go after

chunksOf :: Int -> Text -> [Text]
chunksOf count text
  | measure count <= 0 = []
  | otherwise = Prelude.map pack (go (unpack text))
  where
    go [] = []
    go characters = case Prelude.splitAt (measure count) characters of
      (before, after) -> before : go after

lines :: Text -> [Text]
lines text = Prelude.map pack (Prelude.lines (unpack text))

words :: Text -> [Text]
words text = Prelude.map pack (Prelude.words (unpack text))

unlines :: [Text] -> Text
unlines texts = pack (Prelude.unlines (Prelude.map unpack texts))

unwords :: [Text] -> Text
unwords texts = pack (Prelude.unwords (Prelude.map unpack texts))

isPrefixOf :: Text -> Text -> Bool
isPrefixOf a b = List.isPrefixOf (unpack a) (unpack b)

isSuffixOf :: Text -> Text -> Bool
isSuffixOf a b = List.isSuffixOf (unpack a) (unpack b)

isInfixOf :: Text -> Text -> Bool
isInfixOf a b = List.isInfixOf (unpack a) (unpack b)

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix prefix text = case List.stripPrefix (unpack prefix) (unpack text) of
  Just rest -> Just (pack rest)
  Nothing -> Nothing

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix suffix text = case List.stripPrefix (Prelude.reverse (unpack suffix)) (Prelude.reverse (unpack text)) of
  Just rest -> Just (pack (Prelude.reverse rest))
  Nothing -> Nothing

commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
commonPrefixes = placeholder

filter :: (Char -> Bool) -> Text -> Text
filter p = lift (Prelude.filter p)

breakOnAll :: Text -> Text -> [(Text, Text)]
breakOnAll = placeholder

find :: (Char -> Bool) -> Text -> Maybe Char
find p text = case Prelude.filter p (unpack text) of
  c : _ -> Just c
  [] -> Nothing

elem :: Char -> Text -> Bool
elem c text = Prelude.elem c (unpack text)

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p text = (filter p text, filter (not . p) text)

index :: Text -> Int -> Char
index text position = unpack text !! measure position

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex = placeholder

count :: Text -> Text -> Int
count needle text = tally (Prelude.length (splitOn needle text) - 1)

zip :: Text -> Text -> [(Char, Char)]
zip a b = Prelude.zip (unpack a) (unpack b)

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f a b = pack (Prelude.zipWith f (unpack a) (unpack b))

copy :: Text -> Text
copy text = text

measureOff :: Int -> Text -> Int
measureOff = placeholder

lift :: ([Char] -> [Char]) -> Text -> Text
lift f text = pack (f (unpack text))

-- What comes before the first place some characters are found among
-- others, and the rest, which begins with them or is empty.
found :: [Char] -> [Char] -> ([Char], [Char])
found needle = go
  where
    go characters
      | List.isPrefixOf needle characters = ([], characters)
      | otherwise = case characters of
          [] -> ([], [])
          c : rest -> case go rest of
            (before, after) -> (c : before, after)

measure, tally :: Int -> Int
measure count = count

tally count = count
