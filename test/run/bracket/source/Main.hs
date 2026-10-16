module Main where

import Control.Exception (Exception, SomeException, bracket, bracketOnError, bracket_, catch, finally, onException, throwIO, try)
import Data.IORef (modifyIORef, newIORef, readIORef)

-- What is done when an action throws, and what was thrown, thrown on.

newtype Boom = Boom String
  deriving (Show)

instance Exception Boom

main :: IO ()
main = do
  log' <- newIORef []
  let note message = modifyIORef log' (message :)
  result <- bracket (note "acquire" >> pure 5) (\_ -> note "release") (\n -> note "use" >> pure (n * 2 :: Int))
  print result
  failed <- try (bracket (note "acquire" >> pure ()) (\_ -> note "release") (\_ -> throwIO (Boom "boom")))
  print (either (\e -> show (e :: Boom)) (const "no") failed)
  (throwIO (Boom "inner") `finally` note "finally") `catch` (\e -> note ("caught " ++ show (e :: Boom)))
  (pure () `onException` note "never") >> note "after"
  _ <- try (bracketOnError (pure ()) (\_ -> note "error release") (\_ -> throwIO (Boom "again"))) :: IO (Either SomeException ())
  bracketOnError (pure ()) (\_ -> note "not released") (\_ -> note "fine")
  bracket_ (note "before") (note "after2") (note "thing")
  print . reverse =<< readIORef log'
