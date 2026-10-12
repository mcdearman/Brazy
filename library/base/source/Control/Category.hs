module Control.Category
  ( Category (..),
    (<<<),
    (>>>),
  )
where

import Prelude hiding (id, (.))

class Category cat where
  id :: cat a a
  infixr 9 .
  (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
  id a = a
  (f . g) a = f (g a)

infixr 1 <<<

(<<<) :: (Category cat) => cat b c -> cat a b -> cat a c
f <<< g = f . g

infixr 1 >>>

(>>>) :: (Category cat) => cat a b -> cat b c -> cat a c
f >>> g = g . f
