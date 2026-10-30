module Main where

-- A default declaration lists a type of the program's own, which is a
-- number: what is never worked out is taken to be it.
default (Clock, Integer)

-- | Hours on a clock, which go round at twelve.
newtype Clock = Clock Int

instance Show Clock where
  show (Clock hours) = "Clock " ++ show hours

instance Num Clock where
  Clock a + Clock b = Clock ((a + b) `mod` 12)
  Clock a * Clock b = Clock ((a * b) `mod` 12)
  abs = id
  signum _ = Clock 1
  fromInteger n = Clock (fromInteger (n `mod` 12))
  negate (Clock a) = Clock ((12 - a) `mod` 12)

main :: IO ()
main = do
  print (9 + 5)
  print (7 * 7)
  -- Clock is no instance of Integral, so this one is an Integer.
  print (2 ^ 70 `div` 3)
