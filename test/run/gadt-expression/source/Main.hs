module Main where

-- An expression that says what type its value has.
data Expr a where
  Number :: Int -> Expr Int
  Truth :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  Equal :: Expr Int -> Expr Int -> Expr Bool
  Pair :: Expr a -> Expr b -> Expr (a, b)
  First :: Expr (a, b) -> Expr a

eval :: Expr a -> a
eval (Number n) = n
eval (Truth b) = b
eval (Add x y) = eval x + eval y
eval (If c t e) = if eval c then eval t else eval e
eval (Equal x y) = eval x == eval y
eval (Pair x y) = (eval x, eval y)
eval (First p) = fst (eval p)

-- How big an expression is, which needs to know nothing of its type.
size :: Expr a -> Int
size (Number _) = 1
size (Truth _) = 1
size (Add x y) = size x + size y + 1
size (If c t e) = size c + size t + size e + 1
size (Equal x y) = size x + size y + 1
size (Pair x y) = size x + size y + 1
size (First p) = size p + 1

main :: IO ()
main = do
  print (eval (Add (Number 2) (Number 3)))
  print (eval (If (Equal (Number 1) (Number 1)) (Truth False) (Truth True)))
  print (eval (First (Pair (Add (Number 1) (Number 1)) (Truth True))))
  print (eval (Pair (Number 4) (Truth True)))
  print (size (If (Truth True) (Number 1) (Add (Number 2) (Number 3))))
