module Main where

data P
  = W {names :: [Int]}
  | C {names :: [Int], other :: Int}
  | I {names :: [Int], irrefutable :: !Bool, k :: Int}
  | L {irrefutable :: !Bool, names :: [Int]}
  deriving (Show)

add :: Int -> P -> P
add n p = p {names = n : names p}

lazy :: P -> P
lazy p = case p of
  W {} -> p
  C {} -> p
  _ -> p {irrefutable = True}

insert :: P -> P
insert pre = let made = pre in made {names = 7 : names made}

main :: IO ()
main = do
  print (map (add 0) [W [1], C [2] 3, I [4] False 5, L False [6]])
  print (map lazy [W [1], C [2] 3, I [4] False 5, L False [6]])
  print (insert (I [] True 1), names (insert (L True [1])))
  print ((I [1] False 2) {k = 9, names = []}, (L False [3]) {names = [4], irrefutable = True})
