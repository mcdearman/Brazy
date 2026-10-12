module Control.Exception where

import Hazy (primCatch, primThrow)
import Hazy.Prelude (IO (..), placeholder)

-- Keep this absolutely minimal until I figure out how to deal with exceptions

class (Show e) => Exception e

-- What is thrown is caught by whichever catch was begun last, whatever type
-- its handler takes: nothing here tells one type of exception from another.
-- A program is safe as long as it throws one type.
throw :: (Exception e) => e -> a
throw = primThrow

throwIO :: (Exception e) => e -> IO a
throwIO exception = pure () >>= \() -> throw exception

catch :: (Exception e) => IO a -> (e -> IO a) -> IO a
catch (IO action) handler = IO (primCatch action (\exception -> runIO (handler exception)))

handle :: (Exception e) => (e -> IO a) -> IO a -> IO a
handle handler action = catch action handler

try :: (Exception e) => IO a -> IO (Either e a)
try action = catch (fmap Right action) (\exception -> pure (Left exception))

evaluate :: a -> IO a
evaluate value = pure () >>= \() -> value `seq` pure value

data SomeException

instance Show SomeException where
  showsPrec = placeholder

instance Exception SomeException

data AsyncException
  = StackOverflow
  | HeapOverflow
  | ThreadKilled
  | UserInterrupt

instance Show AsyncException where
  showsPrec = placeholder

instance Exception AsyncException
