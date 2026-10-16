-- |
-- The bits of a floating-point number, as IEEE 754 has them, and the
-- number of some bits. The runtime does each as a function of C.
module GHC.Float
  ( castWord32ToFloat,
    castFloatToWord32,
    castWord64ToDouble,
    castDoubleToWord64,
    float2Double,
    double2Float,
  )
where

import Data.Word (Word32, Word64)

foreign import ccall unsafe "hazy_cast_u32_f32" castWord32ToFloat :: Word32 -> Float

foreign import ccall unsafe "hazy_cast_f32_u32" castFloatToWord32 :: Float -> Word32

foreign import ccall unsafe "hazy_cast_u64_f64" castWord64ToDouble :: Word64 -> Double

foreign import ccall unsafe "hazy_cast_f64_u64" castDoubleToWord64 :: Double -> Word64

float2Double :: Float -> Double
float2Double = realToFrac

double2Float :: Double -> Float
double2Float = realToFrac
