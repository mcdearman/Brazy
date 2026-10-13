module Main where

data Pair = Int :& Int
  deriving (Show)

infixr 5 :&

deep :: Int
deep = ((((((((((((((((((((((((1 + 2) * 3) - 4) + 5) * 6) - 7) + 8) * 9) - 10) + 11) * 12) - 13) + 14) * 15) - 16) + 17) * 18) - 19) + 20) * 21) - 22) + 23) * 24) - 25)

main :: IO ()
main = do
  -- Sections, of an operator on either side.
  print (map (+ 1) [1, 2], map (1 +) [1, 2], map (subtract 1) [1, 2], map (`div` 2) [4, 6], map (10 `div`) [2, 5])
  print (map (2 * 3 +) [1], map (12 `div` 2 `div`) [3], map (: []) "ab", map ('x' :) ["a"], map (1 :&) [2], map (:& 2) [1])
  print (foldr (.) id [(+ 1), (* 2)] 5, (.) (+ 1) (* 2) 5, ((+) 1 2, (-) 5 1), (subtract 1 . (* 2)) 4)
  -- What only looks like a section.
  print ((- 1), (-1), (1 - 2), (negate 3 +) 1, [(- x) | x <- [1, 2]])
  -- Tuples, units and what has its type said.
  print ((), (1), ((2)), (1, 2), (1, (2, 3)), ((1, 2), 3), (1 :: Int), (1 + 2 :: Int, 'a'), (,) 1 2, (,,) 1 2 3)
  print ((1 :: Int, 2 :: Integer), ((+) :: Int -> Int -> Int) 1 2, (if True then 1 else 2), (\x -> x + 1) 2, (let y = 3 in y))
  print ((case Just 1 of Just x -> x; Nothing -> 0), (1 :& 2), (:&) 3 4, (1 + 2) :& (3 * 4), ((1 +) 2, (+ 2) 1))
  print (deep, (((((((((((((((((((((((((((((('a')))))))))))))))))))))))))))))))
