module Data.Bitraversable
  ( Bitraversable (..),
    bisequenceA,
    bisequence,
    bimapM,
    bifor,
    biforM,
    bimapAccumL,
    bimapAccumR,
    bimapDefault,
    bifoldMapDefault,
  )
where

import Control.Applicative (Const (..))
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Data.Functor.Identity (Identity (..))

-- |
-- A type of two parameters whose values can be gone through in both, in
-- order, with an effect for each.
class (Bifunctor t, Bifoldable t) => Bitraversable t where
  bitraverse :: (Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)

instance Bitraversable (,) where
  bitraverse f g (a, b) = (,) <$> f a <*> g b

instance Bitraversable ((,,) x1) where
  bitraverse f g (x1, a, b) = (,,) x1 <$> f a <*> g b

instance Bitraversable ((,,,) x1 x2) where
  bitraverse f g (x1, x2, a, b) = (,,,) x1 x2 <$> f a <*> g b

instance Bitraversable ((,,,,) x1 x2 x3) where
  bitraverse f g (x1, x2, x3, a, b) = (,,,,) x1 x2 x3 <$> f a <*> g b

instance Bitraversable Either where
  bitraverse f _ (Left a) = Left <$> f a
  bitraverse _ g (Right b) = Right <$> g b

instance Bitraversable Const where
  bitraverse f _ (Const a) = Const <$> f a

bisequenceA :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequenceA = bitraverse id id

bisequence :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequence = bisequenceA

bimapM :: (Bitraversable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bimapM = bitraverse

bifor :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor t f g = bitraverse f g t

biforM :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
biforM = bifor

-- A state passed on from left to right.
newtype StateL s a = StateL (s -> (s, a))

runStateL :: StateL s a -> s -> (s, a)
runStateL (StateL f) = f

instance Functor (StateL s) where
  fmap f (StateL k) = StateL $ \s -> let (s', a) = k s in (s', f a)

instance Applicative (StateL s) where
  pure a = StateL (\s -> (s, a))
  StateL kf <*> StateL kv = StateL $ \s ->
    let (s', f) = kf s
        (s'', v) = kv s'
     in (s'', f v)

-- A state passed on from right to left.
newtype StateR s a = StateR (s -> (s, a))

runStateR :: StateR s a -> s -> (s, a)
runStateR (StateR f) = f

instance Functor (StateR s) where
  fmap f (StateR k) = StateR $ \s -> let (s', a) = k s in (s', f a)

instance Applicative (StateR s) where
  pure a = StateR (\s -> (s, a))
  StateR kf <*> StateR kv = StateR $ \s ->
    let (s', v) = kv s
        (s'', f) = kf s'
     in (s'', f v)

bimapAccumL :: (Bitraversable t) => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumL f g s t = runStateL (bitraverse (StateL . flip f) (StateL . flip g) t) s

bimapAccumR :: (Bitraversable t) => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumR f g s t = runStateR (bitraverse (StateR . flip f) (StateR . flip g) t) s

bimapDefault :: (Bitraversable t) => (a -> b) -> (c -> d) -> t a c -> t b d
bimapDefault f g = runIdentity . bitraverse (Identity . f) (Identity . g)

bifoldMapDefault :: (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefault f g = getConst . bitraverse (Const . f) (Const . g)
