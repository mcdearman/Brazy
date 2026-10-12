module Hidden (Hidden, shown) where

-- The class is exported and its method is not.
class Hidden a where
  private :: a -> Int

instance Hidden Bool where
  private _ = 1

shown :: (Hidden a) => a -> Int
shown = private
