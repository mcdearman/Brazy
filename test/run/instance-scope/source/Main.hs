module Main where

import Data.Kind (Type)
import Data.Ord (Down (..), comparing)
import Control.Arrow (first, second, (&&&), (***))
import Control.Monad.ST (ST, runST)

-- The type variables of an instance are in scope in its methods, where a
-- type written after an expression may use them.

class Zero a where
  zero :: [a]

instance Zero Bool where
  zero = [False]

-- The method has no type variables of its own.
instance (Zero a) => Zero (Maybe a) where
  zero = [Nothing] ++ map Just (zero :: [a])

-- The method has one, which comes between and has no name here.
class Pick f where
  pick :: f b -> b -> Int

data Tagged a b = Tagged a

instance (Show a) => Pick (Tagged a) where
  pick (Tagged a) _ = length (show (a :: a))

-- A variable that is not the type of a value.
type Proxy :: (Type -> Type) -> Type
data Proxy f = Proxy

class Named f where
  name :: Proxy f -> String

instance Named Maybe where
  name _ = "Maybe"

class Wrapped t where
  wrapped :: t b -> b -> String

type Wrap :: (Type -> Type) -> Type -> Type
data Wrap f b = Wrap

instance (Named f) => Wrapped (Wrap f) where
  wrapped _ _ = name (Proxy :: Proxy f)

-- An action that is run. Its state is its own.
sums :: Int -> Int
sums n = runST $ do
  a <- pure n
  b <- pure (a + 1)
  pure (a + b)

main :: IO ()
main = do
  print (zero :: [Maybe Bool])
  print (pick (Tagged True) 'x')
  putStrLn (wrapped (Wrap :: Wrap Maybe Int) 1)
  print (runST (pure 3) + (1 :: Int), sums 20)
  -- What the library has for orders turned round, and for functions as arrows.
  print (compare (Down 1) (Down (2 :: Int)), comparing snd ('a', 2 :: Int) ('b', 1))
  print (first not (True, 'x'), second succ ('x', 'y'), (succ &&& pred) 'm', (not *** succ) (False, 'a'))
