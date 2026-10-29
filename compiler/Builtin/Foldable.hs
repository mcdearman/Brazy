-- |
-- The builtin class @Foldable@, written as Haskell source: its methods,
-- defaults for all but @foldr@, and the fixity of @elem@. 'builtinClass'
-- compiles it into the names, Core definition and extra the compiler keeps
-- of it.
module Builtin.Foldable where

import Builtin (builtinClass)
import Core.Tree.Class (Class)
import Core.Tree.ClassExtra (ClassExtra (..))
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

-- The defaults have only what is built in, so a value is made before it is
-- gone on with the way the Prelude's seq makes it: by building a constructor
-- whose fields are strict, which is made with them.

bindings :: Bindings () scope
definition :: Class scope
extra :: ClassExtra scope
(bindings, definition, extra) =
  Type2.Foldable
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Foldable t where
        fold :: Monoid m => t m -> m
        foldMap :: Monoid m => (a -> m) -> t a -> m
        foldMap' :: Monoid m => (a -> m) -> t a -> m
        foldr :: (a -> b -> b) -> b -> t a -> b
        foldl :: (b -> a -> b) -> b -> t a -> b
        foldl' :: (b -> a -> b) -> b -> t a -> b
        foldr1 :: (a -> a -> a) -> t a -> a
        foldl1 :: (a -> a -> a) -> t a -> a
        toList :: t a -> [a]
        null :: t a -> Bool
        length :: t a -> Int
        elem :: Eq a => a -> t a -> Bool
        maximum :: Ord a => t a -> a
        minimum :: Ord a => t a -> a
        sum :: Num a => t a -> a
        product :: Num a => t a -> a

        fold t = foldMap (\\m -> m) t
        foldMap f t = foldr (\\a rest -> f a <> rest) mempty t
        foldMap' f t = foldl' (\\rest a -> rest <> f a) mempty t
        foldl f z t = go z (toList t)
          where
            go acc [] = acc
            go acc (x : xs) = go (f acc x) xs
        foldl' f z t = go z (toList t)
          where
            go acc [] = acc
            go acc (x : xs) = let next = f acc x in case next :% next of
              _ :% _ -> go next xs
        foldr1 f t = go (toList t)
          where
            go [x] = x
            go (x : xs) = f x (go xs)
        foldl1 f t = case toList t of
          x : xs -> go x xs
          where
            go acc [] = acc
            go acc (y : ys) = go (f acc y) ys
        toList t = foldr (:) [] t
        null t = case toList t of
          [] -> True
          _ -> False
        length t = go 0 (toList t)
          where
            go count [] = count
            go count (_ : xs) = let next = count + 1 in case next :% next of
              _ :% _ -> go next xs
        elem x t = go (toList t)
          where
            go [] = False
            go (y : ys) = if x == y then True else go ys
        maximum t = case toList t of
          x : xs -> go x xs
          where
            go best [] = best
            go best (y : ys) = go (max best y) ys
        minimum t = case toList t of
          x : xs -> go x xs
          where
            go best [] = best
            go best (y : ys) = go (min best y) ys
        sum t = foldl' (+) 0 t
        product t = foldl' (*) 1 t
      infix 4 `elem`
      """
