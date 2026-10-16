-- |
-- Complex numbers, as the Haskell 2010 report has them.
module Data.Complex
  ( Complex ((:+)),
    realPart,
    imagPart,
    mkPolar,
    cis,
    polar,
    magnitude,
    phase,
    conjugate,
  )
where

infix 6 :+

data Complex a = !a :+ !a
  deriving (Eq)

instance (Show a) => Show (Complex a) where
  showsPrec d (x :+ y) = showParen (d > 6) $ showsPrec 7 x . showString " :+ " . showsPrec 7 y

instance (Read a) => Read (Complex a) where
  readsPrec d =
    readParen
      (d > 6)
      (\r -> [(x :+ y, u) | (x, s) <- readsPrec 7 r, (":+", t) <- lex s, (y, u) <- readsPrec 7 t])

realPart :: Complex a -> a
realPart (x :+ _) = x

imagPart :: Complex a -> a
imagPart (_ :+ y) = y

conjugate :: (Num a) => Complex a -> Complex a
conjugate (x :+ y) = x :+ negate y

mkPolar :: (Floating a) => a -> a -> Complex a
mkPolar r theta = (r * cos theta) :+ (r * sin theta)

cis :: (Floating a) => a -> Complex a
cis theta = cos theta :+ sin theta

polar :: (RealFloat a) => Complex a -> (a, a)
polar z = (magnitude z, phase z)

-- Scaled first, so that the squares do not go past what can be held.
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x :+ y) =
  scaleFloat k (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
  where
    k = max (exponent x) (exponent y)
    mk = negate k
    sqr z = z * z

phase :: (RealFloat a) => Complex a -> a
phase (0 :+ 0) = 0
phase (x :+ y) = atan2 y x

instance Functor Complex where
  fmap f (x :+ y) = f x :+ f y

instance (RealFloat a) => Num (Complex a) where
  (x :+ y) + (x' :+ y') = (x + x') :+ (y + y')
  (x :+ y) - (x' :+ y') = (x - x') :+ (y - y')
  (x :+ y) * (x' :+ y') = (x * x' - y * y') :+ (x * y' + y * x')
  negate (x :+ y) = negate x :+ negate y
  abs z = magnitude z :+ 0
  signum (0 :+ 0) = 0
  signum z@(x :+ y) = (x / r) :+ (y / r) where r = magnitude z
  fromInteger n = fromInteger n :+ 0

instance (RealFloat a) => Fractional (Complex a) where
  (x :+ y) / (x' :+ y') = ((x * x'' + y * y'') / d) :+ ((y * x'' - x * y'') / d)
    where
      x'' = scaleFloat k x'
      y'' = scaleFloat k y'
      k = negate (max (exponent x') (exponent y'))
      d = x' * x'' + y' * y''
  fromRational a = fromRational a :+ 0

instance (RealFloat a) => Floating (Complex a) where
  pi = pi :+ 0
  exp (x :+ y) = (expx * cos y) :+ (expx * sin y)
    where
      expx = exp x
  log z = log (magnitude z) :+ phase z
  sqrt (0 :+ 0) = 0
  sqrt z@(x :+ y) = u :+ (if y < 0 then negate v else v)
    where
      (u, v) = if x < 0 then (v', u') else (u', v')
      v' = abs y / (u' * 2)
      u' = sqrt ((magnitude z + abs x) / 2)
  sin (x :+ y) = (sin x * cosh y) :+ (cos x * sinh y)
  cos (x :+ y) = (cos x * cosh y) :+ negate (sin x * sinh y)
  tan (x :+ y) = (sinx * coshy :+ cosx * sinhy) / (cosx * coshy :+ negate (sinx * sinhy))
    where
      sinx = sin x
      cosx = cos x
      sinhy = sinh y
      coshy = cosh y
  sinh (x :+ y) = (cos y * sinh x) :+ (sin y * cosh x)
  cosh (x :+ y) = (cos y * cosh x) :+ (sin y * sinh x)
  tanh (x :+ y) = (cosy * sinhx :+ siny * coshx) / (cosy * coshx :+ siny * sinhx)
    where
      siny = sin y
      cosy = cos y
      sinhx = sinh x
      coshx = cosh x
  asin z@(x :+ y) = y' :+ negate x'
    where
      (x' :+ y') = log (((negate y) :+ x) + sqrt (1 - z * z))
  acos z = y'' :+ negate x''
    where
      (x'' :+ y'') = log (z + ((negate y') :+ x'))
      (x' :+ y') = sqrt (1 - z * z)
  atan z@(x :+ y) = y' :+ negate x'
    where
      (x' :+ y') = log (((1 - y) :+ x) / sqrt (1 + z * z))
  asinh z = log (z + sqrt (1 + z * z))
  acosh z = log (z + (sqrt (z + 1)) * (sqrt (z - 1)))
  atanh z = 0.5 * log ((1.0 + z) / (1.0 - z))
