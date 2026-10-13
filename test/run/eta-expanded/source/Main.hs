module Main where

-- A definition that is another given some of its arguments takes what is
-- left of the other's.

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b * 10 + c * 100

-- Another definition by another name.
same :: Int -> Int -> Int -> Int
same = add3

-- Given one, and given two.
one :: Int -> Int -> Int
one = add3 1

two :: Int -> Int
two = add3 1 2

-- One that is one that is another.
far :: Int -> Int
far = one 2

-- Written as taking one, and taking another besides.
mixed :: Int -> Int -> Int
mixed a = add3 a 2

-- Given all of them: a number, not a function.
whole :: Int
whole = add3 1 2 3

-- What requires something of its type, at a type and at any.
plus :: (Num a) => a -> a -> a
plus a b = a + b

atInt :: Int -> Int -> Int
atInt = plus

atAny :: (Num a) => a -> a -> a
atAny = plus

-- Given something that is worked out, which is left as it is written.
worked :: Int -> Int
worked = add3 (length "four") 2

-- Inside a newtype.
newtype Wrapped = Wrapped (Int -> Int)

wrapped :: Wrapped
wrapped = Wrapped (add3 1 2)

unwrap :: Wrapped -> Int -> Int
unwrap (Wrapped function) = function

-- Each is the other: neither takes anything, and neither is asked.
this, that :: Int -> Int
this = that
that = this

-- A definition of the library's, given some.
total :: [Int] -> Int
total = foldr (+) 0

-- Used as a value, and given fewer than it takes.
main :: IO ()
main = do
  print (same 1 2 3)
  print (one 2 3)
  print (two 3)
  print (far 3)
  print (mixed 1 3)
  print whole
  print (atInt 1 2)
  print (atAny 1 (2 :: Integer))
  print (worked 3)
  print (unwrap wrapped 3)
  print (total [1, 2, 3])
  print (map two [1, 2, 3])
  print (map (one 2) [1, 2, 3])
  print (zipWith same [1, 2] [3, 4] <*> [5])
  print (length [this, that])
