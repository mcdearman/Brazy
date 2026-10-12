module Main where

newtype Wrap m = Wrap (m Int)

newtype Morph f g = Morph (forall m. (Monad m) => f m -> g m)

data Boxed f g = Boxed (forall m. (Monad m) => f m -> g m)

apply :: Morph f g -> f Maybe -> g Maybe
apply (Morph f) x = f x

both :: Morph f g -> (f Maybe, f []) -> (g Maybe, g [])
both (Morph f) (x, y) = (f x, f y)

applyBoxed :: Boxed f g -> f Maybe -> g Maybe
applyBoxed (Boxed f) x = f x

bump :: (Monad m) => Wrap m -> Wrap m
bump (Wrap m) = Wrap (m >>= \x -> return (x + 1))

compose :: Morph g h -> Morph f g -> Morph f h
compose (Morph f) (Morph g) = Morph (\x -> f (g x))

main :: IO ()
main = do
  case apply (Morph bump) (Wrap (Just 1)) of
    Wrap result -> print result
  case both (Morph bump) (Wrap (Just 1), Wrap [1, 2]) of
    (Wrap a, Wrap b) -> print (a, b)
  case applyBoxed (Boxed bump) (Wrap (Just 2)) of
    Wrap result -> print result
  case apply (compose (Morph bump) (Morph bump)) (Wrap (Just 4)) of
    Wrap result -> print result
  let lazy = Morph bump
  case apply lazy (Wrap Nothing) of
    Wrap result -> print result
