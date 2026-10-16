module Main where

import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble, double2Float, float2Double)

-- The bits of floating-point numbers, as IEEE 754 has them.

main :: IO ()
main = do
  print (map castDoubleToWord64 [0, -0, 1, -2.5, 1 / 0, 5.0e-324])
  print (map castFloatToWord32 [0, 1, -2.5, 1 / 0])
  print (castWord64ToDouble 4611686018427387904, castWord32ToFloat 1065353216)
  print (isNaN (castWord64ToDouble 9221120237041090560), float2Double 0.5, double2Float 0.25)
