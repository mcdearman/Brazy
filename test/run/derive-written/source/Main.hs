module Main where

data Pair a = Pair a a
  deriving (Show, Functor, Foldable, Traversable)

data Tree a = Leaf | Node (Tree a) a [Tree a] (Maybe a) Int
  deriving (Show, Functor, Foldable, Traversable)

data Named key a = Named {key :: key, value :: a, others :: [(Maybe a)]}
  deriving (Show, Functor, Foldable, Traversable)

data Colour = Red | Green | Blue
  deriving (Show, Eq, Read)

data Shape a = Circle a | Rectangle a a | Named' {label :: String, colour :: Colour, sides :: [a]} | Empty
  deriving (Show, Eq, Read)

tree :: Tree Int
tree = Node (Node Leaf 1 [] Nothing 0) 2 [Node Leaf 3 [Leaf] (Just 4) 0, Leaf] (Just 5) 9

main :: IO ()
main = do
  print (fmap (+ 1) (Pair 1 2), sum (Pair 3 4), foldr (:) [] (Pair 'a' 'b'))
  print (fmap show tree)
  print (foldr (:) [] tree, sum tree, length tree, maximum tree, elem 4 tree)
  print =<< traverse (\x -> pure (x * 2)) tree
  print (traverse (\x -> if x > 0 then Just x else Nothing) tree)
  print (traverse (\x -> if x > 2 then Just x else Nothing) (Pair 3 1))
  print (sequenceA (Pair [1, 2] [3 :: Int]))
  let named = Named {key = "k", value = 1 :: Int, others = [Just 2, Nothing, Just 3]}
  print (fmap negate named, sum named, fmap (const ()) named)
  print (read "Blue" :: Colour, read " ( Green ) " :: Colour, reads "Purple" :: [(Colour, String)])
  print (read "[Red,Green]" :: [Colour], read "(Red,Blue)" :: (Colour, Colour))
  print (read "Circle 3" :: Shape Int, read "Rectangle 1 (-2)" :: Shape Int, read "Empty" :: Shape Int)
  print (read "Named' {label = \"a b\", colour = Green, sides = [1,2]}" :: Shape Int)
  print (read "[Circle 1,Empty,(Rectangle 2 3)]" :: [Shape Int], read "Just (Circle 1)" :: Maybe (Shape Int))
  print (read (show (Named' "x" Red [Circle 1])) == Named' "x" Red [Circle (1 :: Int)])
