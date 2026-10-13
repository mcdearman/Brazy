module Data.Maybe
  ( Maybe (..),
    maybe,
    isJust,
    isNothing,
    fromJust,
    fromMaybe,
    listToMaybe,
    maybeToList,
    catMaybes,
    mapMaybe,
  )
where


isJust :: Maybe a -> Bool
isJust = \case
  Just _ -> True
  Nothing -> False

isNothing :: Maybe a -> Bool
isNothing = \case
  Just _ -> False
  Nothing -> True

fromJust :: Maybe a -> a
fromJust = \case
  Just a -> a
  Nothing -> error "Maybe.fromJust: Nothing"

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback = \case
  Just a -> a
  Nothing -> fallback

listToMaybe :: [a] -> Maybe a
listToMaybe = \case
  a : _ -> Just a
  [] -> Nothing

maybeToList :: Maybe a -> [a]
maybeToList = \case
  Just a -> [a]
  Nothing -> []

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [a | Just a <- xs]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = [b | Just b <- map f xs]
