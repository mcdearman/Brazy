module Main where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))

-- An instance may require a parameter of its type to be a particular type.
-- The instance is then the one for the type whatever it is given, and using
-- it makes what the type is given that type.

data Tagged tag a = Tagged a

class Describe f where
  describe :: f -> String

instance (tag ~ Int) => Describe (Tagged tag a) where
  describe _ = "tagged with a number"

number :: Tagged tag a -> tag -> tag
number _ tag = tag

-- Inside the instance the parameter is known to be the type.
class Sum f where
  total :: f -> Int

data Box a = Box a a

instance (a ~ Int) => Sum (Box a) where
  total (Box x y) = x + y

-- With a class required as well.
class Render f where
  render :: f -> String

data Pair a b = Pair a b

instance (a ~ Bool, Show b) => Render (Pair a b) where
  render (Pair a b) = (if a then "yes " else "no ") ++ show b

main :: IO ()
main = do
  let tagged = Tagged 'x'
  putStrLn (describe tagged)
  print (number tagged 3 + 1)
  print (total (Box 1 2))
  putStrLn (render (Pair True 'x') ++ render (Pair False [1 :: Int, 2]))
  -- The library's functors that do nothing and that put one in another.
  print (runIdentity (fmap (+ 1) (Identity (1 :: Int))), runIdentity (Identity (+ 2) <*> pure (2 :: Int)))
  print (getCompose (fmap not (Compose [Just True, Nothing])))
  print (runIdentity (traverse (Identity . succ) "abc"))
