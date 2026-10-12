module Main where

-- A constructor may hold something whose type has a forall of its own.
-- What it is given there has to have that type whatever the variables of
-- the forall are, and what is taken out may be used at any types.

newtype Natural f g = Morph (forall a. f a -> g a)

apply :: Natural f g -> f a -> g a
apply (Morph f) x = f x

listed :: Natural Maybe []
listed = Morph (maybe [] (\x -> [x]))

-- Taken out of one and put in another.
compose :: Natural g h -> Natural f g -> Natural f h
compose (Morph f) (Morph g) = Morph (\x -> f (g x))

-- Used at two types in one place.
data Twice = Twice (forall a. a -> (a, a))

both :: Twice -> (Int, Bool) -> ((Int, Int), (Bool, Bool))
both (Twice f) (n, b) = (f n, f b)

-- With names: built, matched, selected from and updated.
data Pick = Pick {pick :: forall a. a -> a -> a, name :: String}

flipped :: Pick -> Pick
flipped p = p {pick = \_ b -> b, name = "second"}

-- A type that anything has is the type of something that is never there.
data Formula a = Formula {cycle :: forall b. b, run :: Int -> a}

use :: Formula a -> Int -> a
use Formula {run} n = run n

never :: Formula a -> Int
never Formula {cycle} = if False then cycle else length (if False then cycle else "ab")

-- The type may require something of its variables. What is held is then
-- given the evidence where it is used.
data Shower = Shower (forall a. (Show a) => a -> String)

shown :: Shower -> String
shown (Shower f) = f 'x' ++ f True ++ f (1 :: Int)

data Runner m = Runner {go :: forall loeb. (Monad loeb) => loeb Int -> loeb m, label :: String}

ran :: Runner m -> (Maybe m, [m])
ran Runner {go} = (go (Just 1), go [1, 2])

-- Held strictly, beside a constructor that holds something else.
data Index = Named !Int | Builtin !(forall scope. Maybe scope)

out :: Index -> Maybe Int
out (Builtin b) = b
out (Named n) = Just n

-- What a constructor is given may be given through a function that can
-- only apply, whichever function of that type it is.
labelled :: Shower -> String
labelled (Shower f) = f "label"

other :: (a -> b) -> a -> b
other f = f

main :: IO ()
main = do
  print (apply (Morph $ take 1 . maybe [] (\x -> [x, x])) (Just 'x'))
  print (apply (Morph $! maybe [] (\x -> [x])) (Just 'y'), apply (Morph `other` maybe [] (\x -> [x])) (Just 'z'))
  putStrLn $ labelled $ Shower $ \a -> show a ++ "!"
  print (apply listed (Just 'x'), apply listed (Just True), length (apply listed (Nothing :: Maybe Int)))
  print (apply (compose (Morph (take 1)) (Morph (maybe [] (\x -> [x, x])))) (Just 'x'))
  print (both (Twice (\x -> (x, x))) (1, True))
  let first = Pick {pick = \a _ -> a, name = "first"}
  print (pick first (1 :: Int) 2, pick first 'a' 'b', name first)
  case flipped first of Pick {pick, name} -> print (pick (1 :: Int) 2, name)
  let formula = Formula {cycle = error "cycle", run = \n -> n + 1}
  print (use formula 1, never formula)
  putStrLn (shown (Shower show) ++ shown (Shower (\a -> "<" ++ show a ++ ">")))
  print (ran Runner {go = fmap show, label = "r"})
  print (out (Builtin Nothing), out (Named 3))
