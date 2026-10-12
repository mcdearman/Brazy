module Main where

-- A constructor may require something of its types. A value it makes holds
-- the evidence, and a match on it has the evidence to use.

data Showable where
  Showable :: (Show a) => a -> Showable

render :: Showable -> String
render (Showable a) = show a

instance Show Showable where
  show s = "<" ++ render s ++ ">"

-- What the class requires in turn is had as well.
data Ordered where
  Ordered :: (Ord a, Show a) => a -> a -> Ordered

same :: Ordered -> (Bool, Bool)
same (Ordered a b) = (a == b, a < b)

-- In guards and in what is declared for a branch.
big :: Ordered -> String
big (Ordered a b)
  | a > b = shown a
  | otherwise = shown b
  where
    shown x = "<" ++ show x ++ ">"

-- Required of a type that the type made settles, with and without a
-- signature where it is matched.
data Shown a where
  Shown :: (Show a) => a -> Shown a

known :: Shown a -> String
known (Shown a) = show a

unknown (Shown a) = show a

-- Together with constructors that tell the type.
data Term a where
  Lit :: (Show a) => a -> Term a
  Pair :: Term a -> Term b -> Term (a, b)
  IsZero :: Term Int -> Term Bool

pretty :: Term a -> String
pretty (Lit a) = show a
pretty (Pair a b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"
pretty (IsZero t) = "zero? " ++ pretty t

-- With names for what is held: built, matched, selected from and updated.
data Rec where
  Rec :: (Show a) => {value :: a, label :: String} -> Rec

named :: Rec -> String
named Rec {label = l, value = v} = l ++ "=" ++ show v

relabel :: Rec -> Rec
relabel r = r {label = "new"}

-- Inside other patterns.
both :: (Showable, Maybe Showable) -> String
both (Showable a, Just (Showable b)) = show a ++ show b
both (Showable a, Nothing) = show a

-- Required of a type that is not the type of a value.
data Mapped where
  Mapped :: (Functor f) => f Int -> (f Int -> Int) -> Mapped

run :: Mapped -> Int
run (Mapped x f) = f (fmap (+ 1) x)

-- A constructor may require two types to be the same. That is what a
-- constructor that says what type it makes does, said another way.
data Value a where
  Number :: (a ~ Int) => Int -> Value a
  Truth :: (a ~ Bool) => Bool -> Value a

eval :: Value a -> a
eval (Number n) = n
eval (Truth b) = b

data Same a b where
  Same :: (a ~ b) => Same a b

cast :: Same a b -> a -> b
cast Same x = x

-- Both kinds of requirement at once, and of a type that is hidden.
data Listed a where
  Listed :: (a ~ [b], Show b) => b -> Listed a

twice :: Listed a -> a
twice (Listed b) = [b, b]

shown :: Listed a -> String
shown (Listed b) = show b

data Counted where
  Counted :: (a ~ Int) => a -> Counted

next :: Counted -> Int
next (Counted a) = a + 1

main :: IO ()
main = do
  print (eval (Number 4), eval (Truth True), cast Same 'x')
  putStrLn (show (twice (Listed 'x')) ++ shown (Listed True))
  print (next (Counted 41))
  putStrLn (concatMap render [Showable 'x', Showable (1 :: Int), Showable True, Showable [Showable ()]])
  putStrLn (concatMap render (map Showable "abc"))
  print (same (Ordered 'a' 'b'), same (Ordered (2 :: Int) 2))
  putStrLn (big (Ordered 'a' 'b') ++ big (Ordered (5 :: Int) 2))
  putStrLn (known (Shown 'x') ++ known (Shown True) ++ unknown (Shown "s"))
  putStrLn (pretty (Pair (Lit 'x') (IsZero (Lit 0))))
  putStrLn (named (Rec {value = 'x', label = "old"}) ++ " " ++ named (relabel (Rec (1 :: Int) "old")))
  putStrLn (label (Rec () "l"))
  putStrLn (both (Showable 'x', Just (Showable True)) ++ both (Showable (1 :: Int), Nothing))
  putStrLn ((\(Showable a) -> show a) (Showable 'y'))
  let s = Showable (3 :: Int)
  putStrLn (case s of Showable a -> show [a, a])
  print (run (Mapped [1, 2] sum) + run (Mapped (Just 4) (maybe 0 id)))
