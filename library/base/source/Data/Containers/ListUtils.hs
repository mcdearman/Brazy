module Data.Containers.ListUtils
  ( nubOrd,
    nubOrdOn,
    nubInt,
    nubIntOn,
  )
where

import qualified Data.Set as Set (empty, insert, member)

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = nubOrdOn id

nubOrdOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdOn f = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member key seen = go seen xs
      | otherwise = x : go (Set.insert key seen) xs
      where
        key = f x

nubInt :: [Int] -> [Int]
nubInt = nubOrd

nubIntOn :: (a -> Int) -> [a] -> [a]
nubIntOn = nubOrdOn
