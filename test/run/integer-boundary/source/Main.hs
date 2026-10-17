module Main where

-- Integers on either side of 64 bits, where the runtime keeps a small one
-- in its block and makes a bigger one apart: each operation across the
-- bound, both ways.

top, bottom :: Integer
top = 2 ^ 63 - 1
bottom = negate (2 ^ 63)

main :: IO ()
main = do
  print (top, bottom)
  print (top + 1, bottom - 1, top + 1 - 1, bottom - 1 + 1)
  print (top * 2, bottom * 2, (top * 2) `div` 2, bottom * (-1))
  print (negate bottom, abs bottom, signum bottom, negate (negate bottom))
  print (bottom `quot` (-1), bottom `rem` (-1), bottom `div` (-1), bottom `mod` (-1))
  print ((top + 1) `quot` 2, (bottom - 1) `rem` 10, (top * top) `quot` top)
  print (top == top + 1 - 1, top < top + 1, bottom > bottom - 1, compare (top + 1) top)
  print (sum [top, top, bottom, bottom], product [2 :: Integer, 2 ^ 62, 2])
  print (gcd (top + 1) (2 ^ 40), lcm (2 ^ 40) (3 ^ 30), (top + 1) `divMod` 7)
  print [x * x | x <- [3037000499, 3037000500 :: Integer]]
