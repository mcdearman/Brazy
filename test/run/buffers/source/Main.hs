module Main where

import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, pokeArray, withArrayLen)
import System.Directory (removeFile)
import System.IO

-- Bytes written from memory to a file and to standard output, and read
-- from a file into memory.

main :: IO ()
main = do
  let path = "hazy-buffers-test.bin"
      bytes = map (fromIntegral . fromEnum) "bytes, as they are\n" :: [Word8]
  withFile path WriteMode $ \handle -> do
    hPutStr handle "text, "
    withArrayLen bytes $ \count buffer -> hPutBuf handle buffer count
    sent <- withArrayLen ([65, 66, 10] :: [Word8]) $ \count buffer -> hPutBufNonBlocking handle buffer count
    print sent
  putStr =<< readFile path
  withFile path ReadMode $ \handle -> allocaBytes 64 $ \buffer -> do
    first <- hGetBuf handle buffer 6
    print . (map (toEnum . fromIntegral) :: [Word8] -> String) =<< peekArray first buffer
    rest <- hGetBufSome handle buffer 64
    print (rest, length bytes + 3 - 6 + length "text, ")
    end <- hGetBuf handle buffer 64
    print end
  putStr "before, "
  hFlush stdout
  withArrayLen (map (fromIntegral . fromEnum) "to standard output\n" :: [Word8]) $ \count buffer -> hPutBuf stdout buffer count
  putStrLn "after"
  removeFile path
