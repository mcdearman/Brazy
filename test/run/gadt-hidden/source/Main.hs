module Main where

-- A type variable of a constructor that the type it makes does not settle
-- is hidden: where the constructor is matched on, it is some type that
-- nothing else is known of.

data Value a where
  Number :: Int -> Value Int
  Truth :: Bool -> Value Bool
  Wrapped :: Value a -> Value (Maybe a)
  Both :: Value a -> Value b -> Value (a, b)

data Some where
  Some :: a -> (a -> Int) -> Some

-- A type declared with where whose constructor hides nothing and tells
-- nothing is a type like any other, and needs no signature to be matched.
data Box a where
  Box :: a -> Box a

open (Box a) = a

-- What is hidden may be used with what was hidden with it.
use :: Some -> Int
use (Some a f) = f a

twice :: a -> (a, a)
twice a = (a, a)

-- Matching again on a value whose type is given a hidden type tells what
-- the hidden type is, differently on each branch.
get :: Value a -> a
get (Number n) = n
get (Truth b) = b
get (Wrapped x) = case x of
  Number n -> Just n
  Truth b -> Just b
  Wrapped y -> Just (Just (get y))
  Both p q -> Just (get p, get q)
get (Both x y) = (get x, get y)

-- The same with one pattern inside another.
depth :: Value a -> Int
depth (Wrapped (Wrapped _)) = 2
depth (Wrapped (Number _)) = 11
depth (Wrapped _) = 1
depth (Both (Number n) (Truth _)) = n
depth _ = 0

-- A type that is known where a constructor would hide one.
both :: Value (Int, Bool) -> Int
both (Both (Number n) (Truth b)) = if b then n else 0

-- Two hidden types that a third value says are the same.
data Same a b where
  Same :: Same c c

data Pair where
  Pair :: a -> b -> Same a b -> Pair

swap :: Pair -> Pair
swap (Pair a b Same) = Pair b a Same

size :: Pair -> Int
size (Pair a b Same) = length [a, b]

-- A hidden type that is not the type of a value.
data Packed where
  Packed :: f Int -> (f Int -> Int) -> Packed

run :: Packed -> Int
run (Packed x f) = f x

main :: IO ()
main = do
  print (open (Box 'x'), (\(Box b) -> b) (Box True), case Box 3 of Box n -> n + 1)
  print (use (Some 'x' (const 7)), use (Some 4 (+ 1)))
  case Some 'x' (const 7) of Some a f -> print (f (fst (twice a)) + length [a, a])
  Some a f <- pure (Some "do" length)
  print (f a)
  print (get (Wrapped (Wrapped (Number 1))), get (Wrapped (Both (Number 2) (Truth True))))
  print (depth (Wrapped (Wrapped (Number 1))), depth (Wrapped (Number 1)), depth (Both (Number 9) (Truth True)))
  print (both (Both (Number 4) (Truth True)))
  print (size (swap (Pair 'x' 'y' Same)))
  print (run (Packed [1, 2, 3] length) + run (Packed (Just 4) (maybe 0 id)))
