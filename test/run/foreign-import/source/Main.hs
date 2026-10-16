module Main where

-- Functions of C, called through the foreign function interface: pure
-- ones, ones that are actions, and one of no arguments that gives nothing.

foreign import ccall "math.h sin" c_sin :: Double -> Double

foreign import ccall unsafe "pow" power :: Double -> Double -> Double

foreign import ccall "static math.h floor" c_floor :: Double -> Double

foreign import ccall "sqrtf" rootf :: Float -> Float

foreign import ccall llabs :: Int -> Int

foreign import ccall "toupper" upper :: Char -> Char

foreign import ccall safe "hypot" hypotenuse :: Double -> Double -> IO Double

foreign import ccall "srand" seed :: Int -> IO ()

type Number = Double

foreign import ccall "atan2" angle :: Number -> Number -> Number

main :: IO ()
main = do
  print (c_sin 0, power 2 10, c_floor (-2.5), rootf 2)
  print (llabs (-42), map upper "foreign")
  h <- hypotenuse 3 4
  print h
  seed 7
  print (angle 1 1 * 4)
