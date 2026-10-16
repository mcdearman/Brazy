module Main where

import Foreign.C.String (CString, castCCharToChar, castCharToCChar, newCString, newCStringLen, peekCAString, peekCString, peekCStringLen, withCAString, withCString)
import Foreign.C.Types (CDouble (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (free)

-- Strings as C has them, and functions of C called with C's types.

foreign import ccall unsafe "strlen" c_strlen :: CString' -> IO CSize

foreign import ccall unsafe "strcmp" c_strcmp :: CString' -> CString' -> IO CInt

foreign import ccall unsafe "toupper" c_toupper :: CInt -> CInt

foreign import ccall unsafe "sqrt" c_sqrt :: CDouble -> CDouble

type CString' = CString

main :: IO ()
main = do
  print =<< withCString "héllo" c_strlen
  print =<< withCAString "plain" c_strlen
  written <- newCString "λx → y, naïve"
  print =<< peekCString written
  print =<< c_strlen written
  free written
  (pair, count) <- newCStringLen "αβγ"
  print count
  print =<< peekCStringLen (pair, 4)
  free pair
  print =<< withCString "abc" (\a -> withCString "abd" (\b -> signum <$> c_strcmp a b))
  print =<< withCString "same" (\a -> withCString "same" (c_strcmp a))
  print =<< withCAString "ascii" peekCAString
  print (map (toEnum . fromIntegral . c_toupper . fromIntegral . fromEnum) "shout" :: String)
  print (c_sqrt 2, castCCharToChar (castCharToCChar 'q'))
