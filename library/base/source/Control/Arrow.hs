module Control.Arrow
  ( Arrow (..),
    Kleisli (..),
    returnA,
    (^>>),
    (>>^),
    (<<^),
    (^<<),
    ArrowZero (..),
    ArrowChoice (..),
    ArrowApply (..),
    ArrowMonad (..),
    leftApp,
    ArrowLoop (..),
  )
where

import Control.Category
  ( Category (id, (.)),
    (<<<),
    (>>>),
  )
import Control.Monad (MonadPlus (..))
import Prelude hiding (id, (.))
import qualified Prelude

class (Category a) => Arrow a where
  arr :: (b -> c) -> a b c
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)
  infixr 3 ***
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  infixr 3 &&&
  (&&&) :: a b c -> a b c' -> a b (c, c')

  first = (*** id)
  second = (id ***)
  f *** g = first f >>> arr swap >>> first g >>> arr swap
    where
      swap ~(x, y) = (y, x)
  f &&& g = arr (\b -> (b, b)) >>> f *** g

instance Arrow (->) where
  arr f = f
  first f (b, d) = (f b, d)
  second f (d, b) = (d, f b)
  (f *** g) (b, b') = (f b, g b')
  (f &&& g) b = (f b, g b)

newtype Kleisli m a b = Kleisli
  { runKleisli :: a -> m b
  }

instance (Monad m) => Category (Kleisli m) where
  id = Kleisli return
  Kleisli f . Kleisli g = Kleisli (\b -> g b >>= f)

instance (Monad m) => Arrow (Kleisli m) where
  arr f = Kleisli (return . f)
  first (Kleisli f) = Kleisli (\ ~(b, d) -> f b >>= \c -> return (c, d))
  second (Kleisli f) = Kleisli (\ ~(d, b) -> f b >>= \c -> return (d, c))

instance (Monad m) => ArrowChoice (Kleisli m) where
  left f = f +++ arr Prelude.id
  right f = arr Prelude.id +++ f
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  Kleisli f ||| Kleisli g = Kleisli (either f g)

instance (Monad m) => ArrowApply (Kleisli m) where
  app = Kleisli (\(Kleisli f, x) -> f x)

instance (MonadPlus m) => ArrowZero (Kleisli m) where
  zeroArrow = Kleisli (\_ -> mzero)

instance (MonadPlus m) => ArrowPlus (Kleisli m) where
  Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)

instance (Monad m) => Functor (Kleisli m a) where
  fmap f (Kleisli g) = Kleisli (fmap f . g)

returnA :: (Arrow a) => a b b
returnA = id

infixr 1 ^>>

(^>>) :: (Arrow a) => (b -> c) -> a c d -> a b d
f ^>> a = arr f >>> a

infixr 1 >>^

(>>^) :: (Arrow a) => a b c -> (c -> d) -> a b d
a >>^ f = a >>> arr f

infixr 1 <<^

(<<^) :: (Arrow a) => a c d -> (b -> c) -> a b d
a <<^ f = a <<< arr f

infixr 1 ^<<

(^<<) :: (Arrow a) => (c -> d) -> a b c -> a b d
f ^<< a = arr f <<< a

class (Arrow a) => ArrowZero a where
  zeroArrow :: a b c

class (ArrowZero a) => ArrowPlus a where
  infixr 5 <+>
  (<+>) :: a b c -> a b c -> a b c

class (Arrow a) => ArrowChoice a where
  left :: a b c -> a (Either b d) (Either c d)
  right :: a b c -> a (Either d b) (Either d c)
  infixr 2 +++, |||
  (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
  (|||) :: a b d -> a c d -> a (Either b c) d

  left = (+++ id)
  right = (id +++)
  f +++ g = left f >>> arr mirror >>> left g >>> arr mirror
    where
      mirror (Left x) = Right x
      mirror (Right y) = Left y
  f ||| g = f +++ g >>> arr untag
    where
      untag (Left x) = x
      untag (Right y) = y

instance ArrowChoice (->) where
  left f = f +++ Prelude.id
  right f = Prelude.id +++ f
  f +++ g = either (Left . f) (Right . g)
  (|||) = either

class (Arrow a) => ArrowApply a where
  app :: a (a b c, b) c

instance ArrowApply (->) where
  app (f, x) = f x

newtype ArrowMonad a b = ArrowMonad (a () b)

instance (Arrow a) => Functor (ArrowMonad a) where
  fmap f (ArrowMonad m) = ArrowMonad (m >>^ f)

instance (Arrow a) => Applicative (ArrowMonad a) where
  pure x = ArrowMonad (arr (const x))
  ArrowMonad f <*> ArrowMonad x = ArrowMonad (f &&& x >>> arr (uncurry Prelude.id))

instance (ArrowApply a) => Monad (ArrowMonad a) where
  ArrowMonad m >>= f =
    ArrowMonad (m >>> arr (\x -> let ArrowMonad h = f x in (h, ())) >>> app)

leftApp :: (ArrowApply a) => a b c -> a (Either b d) (Either c d)
leftApp f = arr choose >>> app
  where
    choose (Left b) = (arr (\() -> b) >>> f >>> arr Left, ())
    choose (Right d) = (arr (\() -> d) >>> arr Right, ())

class (Arrow a) => ArrowLoop a where
  loop :: a (b, d) (c, d) -> a b c

instance ArrowLoop (->) where
  loop f b = let (c, d) = f (b, d) in c
