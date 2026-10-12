module Main where

import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map

data Detail = Detail {index :: !Int, selector :: [Int]}

data Binding f = (:@) {position :: !Int, value :: f Detail}

combine :: Binding Identity -> Binding Identity -> Binding Identity
combine (position :@ Identity (left@Detail {selector})) ~(_ :@ Identity Detail {selector = selector'}) =
  position :@ Identity left {selector = selector ++ selector'}

clash :: Binding Identity -> Binding Identity -> Binding Identity
clash (a :@ x) (b :@ y) = a :@ (both <$> x <*> y)
  where
    both Detail {index = i} Detail {index = j}
      | i == j = Detail {index = i, selector = []}
      | otherwise = error "clash"

local, one, two :: Binding Identity
local = 1 :@ Identity Detail {index = 10, selector = []}
one = 2 :@ Identity Detail {index = 20, selector = [1]}
two = 3 :@ Identity Detail {index = 30, selector = [2]}

shifted :: Binding Identity -> Binding Identity
shifted (position :@ detail) = position :@ fmap (\Detail {index, selector} -> Detail {index = index + 1, selector = map (+ 1) selector}) detail

main :: IO ()
main = do
  -- A field that is changed is not evaluated by being changed.
  case Detail {index = 1, selector = [1]} of
    old -> print (index old {selector = undefined})
  -- What a lazy pattern is matched against is not looked at until something
  -- it binds is.
  case combine local (clash one two) of
    _ :@ Identity Detail {index} -> print index
  let imported = Map.unionWith clash (Map.fromList [("position", one)]) (Map.fromList [("position", two), ("other", two)])
      locals = Map.fromList [("position", local)]
      scope = fmap shifted (Map.unionWith combine locals (fmap shifted imported))
  case Map.lookup "position" scope of
    Just (position :@ Identity Detail {index}) -> print (position, index)
    Nothing -> print "missing"
  case Map.lookup "other" scope of
    Just (position :@ Identity Detail {index, selector}) -> print (position, index, selector)
    Nothing -> print "missing"
  print (Map.size scope)
