module Data.Either
  ( Either (..),
    either,
    lefts,
    rights,
    isLeft,
    isRight,
    fromLeft,
    fromRight,
    partitionEithers,
  )
where

lefts :: [Either a b] -> [a]
lefts xs = [a | Left a <- xs]

rights :: [Either a b] -> [b]
rights xs = [b | Right b <- xs]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _ = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _ = b

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left a ~(l, r) = (a : l, r)
    right a ~(l, r) = (l, a : r)
