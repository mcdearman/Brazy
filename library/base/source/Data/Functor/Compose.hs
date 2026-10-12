module Data.Functor.Compose where

newtype Compose f g a = Compose {getCompose :: f (g a)}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose a) = Compose (fmap (fmap f) a)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose (pure (pure a))
  Compose f <*> Compose a = Compose (liftA2 (<*>) f a)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr f z (Compose a) = foldr (\inner rest -> foldr f rest inner) z a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose a) = fmap Compose (traverse (traverse f) a)
