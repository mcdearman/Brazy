module Main where

-- A tuple's constructor, written with only its commas, in a type and in an
-- instance's head: a comma between each two of what it holds.

class Swap p where
  swap :: p a a -> p a a

instance Swap (,) where
  swap (a, b) = (b, a)

class Middle t where
  middle :: t a b c -> b

instance Middle (,,) where
  middle (_, b, _) = b

instance Middle ((,,,) x) where
  middle (_, _, b, _) = b

pair :: (,) Int Bool
pair = (1, True)

triple :: (,,) Int Bool Char
triple = (1, True, 'c')

pairs :: Seq ((,) Char) Int
pairs = Seq ('a', 1)

newtype Seq f a = Seq (f a)

unseq :: Seq f a -> f a
unseq (Seq x) = x

main :: IO ()
main = do
  print (pair, triple, (,) 'a' 'b', (,,) 1 2 3)
  print (swap (1 :: Int, 2), middle ('a', "b", 'c'), middle ((), 'x', "y", 'z'))
  print (unseq pairs)
