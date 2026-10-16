-- |
-- Strings as C has them: bytes in memory, ended by a 0. A 'String' is
-- written as UTF-8, and read as UTF-8, a byte that is not part of a
-- character being read as the character it is the code of.
module Foreign.C.String
  ( CString,
    CStringLen,
    peekCString,
    peekCStringLen,
    newCString,
    newCStringLen,
    withCString,
    withCStringLen,
    castCharToCChar,
    castCCharToChar,
    castCharToCUChar,
    castCUCharToChar,
    castCharToCSChar,
    castCSCharToChar,
    peekCAString,
    peekCAStringLen,
    newCAString,
    newCAStringLen,
    withCAString,
    withCAStringLen,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)
import Foreign.C.Types (CChar, CSChar, CUChar)
import Foreign.Marshal.Array (lengthArray0, newArray, newArray0, peekArray, withArray0, withArrayLen)
import Foreign.Ptr (Ptr, castPtr)

type CString = Ptr CChar

type CStringLen = (Ptr CChar, Int)

peekCString :: CString -> IO String
peekCString string = do
  count <- lengthArray0 0 (bytes string)
  decode <$> peekArray count (bytes string)

peekCStringLen :: CStringLen -> IO String
peekCStringLen (string, count) = decode <$> peekArray count (bytes string)

newCString :: String -> IO CString
newCString text = castPtr <$> newArray0 0 (encode text)

newCStringLen :: String -> IO CStringLen
newCStringLen text = do
  string <- newArray encoded
  pure (castPtr string, length encoded)
  where
    encoded = encode text

withCString :: String -> (CString -> IO a) -> IO a
withCString text action = withArray0 0 (encode text) (action . castPtr)

withCStringLen :: String -> (CStringLen -> IO a) -> IO a
withCStringLen text action = withArrayLen (encode text) (\count string -> action (castPtr string, count))

-- The bytes a string is.
bytes :: CString -> Ptr Word8
bytes = castPtr

-- | A string of one byte to each character, the code of each taken as it
-- is.
peekCAString :: CString -> IO String
peekCAString string = do
  count <- lengthArray0 0 (bytes string)
  map (toEnum . fromIntegral) <$> peekArray count (bytes string)

peekCAStringLen :: CStringLen -> IO String
peekCAStringLen (string, count) = map (toEnum . fromIntegral) <$> peekArray count (bytes string)

newCAString :: String -> IO CString
newCAString text = castPtr <$> newArray0 0 (map narrow text)

newCAStringLen :: String -> IO CStringLen
newCAStringLen text = do
  string <- newArray (map narrow text)
  pure (castPtr string, length text)

withCAString :: String -> (CString -> IO a) -> IO a
withCAString text action = withArray0 0 (map narrow text) (action . castPtr)

withCAStringLen :: String -> (CStringLen -> IO a) -> IO a
withCAStringLen text action = withArrayLen (map narrow text) (\count string -> action (castPtr string, count))

narrow :: Char -> Word8
narrow = fromIntegral . fromEnum

castCharToCChar :: Char -> CChar
castCharToCChar = fromIntegral . fromEnum

castCCharToChar :: CChar -> Char
castCCharToChar = toEnum . fromIntegral . (fromIntegral :: CChar -> Word8)

castCharToCUChar :: Char -> CUChar
castCharToCUChar = fromIntegral . fromEnum

castCUCharToChar :: CUChar -> Char
castCUCharToChar = toEnum . fromIntegral

castCharToCSChar :: Char -> CSChar
castCharToCSChar = fromIntegral . fromEnum

castCSCharToChar :: CSChar -> Char
castCSCharToChar = toEnum . fromIntegral . (fromIntegral :: CSChar -> Word8)

-- A string as the bytes of UTF-8.
encode :: String -> [Word8]
encode = concatMap character
  where
    character c
      | code < 0x80 = [fromIntegral code]
      | code < 0x800 = [0xc0 .|. high 6, continuing 0]
      | code < 0x10000 = [0xe0 .|. high 12, continuing 6, continuing 0]
      | otherwise = [0xf0 .|. high 18, continuing 12, continuing 6, continuing 0]
      where
        code = fromEnum c
        high shift = fromIntegral (code `shiftR` shift)
        continuing shift = 0x80 .|. fromIntegral ((code `shiftR` shift) .&. 0x3f)

-- The bytes of UTF-8 as a string.
decode :: [Word8] -> String
decode = \case
  [] -> []
  byte : rest
    | byte < 0x80 -> toEnum (fromIntegral byte) : decode rest
    | byte >= 0xc0 && byte < 0xe0 -> continued 1 (byte .&. 0x1f) rest
    | byte >= 0xe0 && byte < 0xf0 -> continued 2 (byte .&. 0x0f) rest
    | byte >= 0xf0 && byte < 0xf8 -> continued 3 (byte .&. 0x07) rest
    | otherwise -> toEnum (fromIntegral byte) : decode rest
  where
    continued count first rest = case splitAt count rest of
      (following, after)
        | length following == count,
          all (\byte -> byte .&. 0xc0 == 0x80) following ->
            toEnum (foldl (\code byte -> code `shiftL` 6 .|. fromIntegral (byte .&. 0x3f)) (fromIntegral first) following) : decode after
      _ -> toEnum (fromIntegral first) : decode rest
