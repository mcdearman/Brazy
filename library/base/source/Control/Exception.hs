module Control.Exception where

import Data.Text (Text, pack, unpack)
import Hazy (primCatch, primThrow, primUnsafeCoerce)
import Hazy.Prelude (IO (..))

-- What a type of exception is called, to tell it from the others.
newtype Tag e = Tag String

-- Something of any type, as it was thrown.
data Any

-- |
-- A type whose values can be thrown. A handler catches the exceptions of
-- its own type and lets the rest go by.
--
-- Types are told apart by a name each gives itself. One that gives none is
-- taken for any other that gives none: a program is safe as long as it
-- throws one such type, or names them.
class (Show e) => Exception e where
  exceptionTag :: Tag e
  exceptionTag = Tag ""

  -- What a program that throws this and catches nothing exits with, or a
  -- negative number if that is a failure to be reported.
  exceptionStatus :: e -> Int
  exceptionStatus _ = -1

  -- The exception a handler is given, of what was thrown and how that is
  -- shown.
  caught :: Any -> Text -> e
  caught thrown _ = primUnsafeCoerce thrown

-- What is thrown: the name of its type, what a program that does not catch
-- it exits with, how it is shown, and the value. The runtime looks at the
-- second and the third when nothing catches it.
data Thrown = Thrown String Int Text Any

throw :: (Exception e) => e -> a
throw exception =
  primThrow (Thrown name (exceptionStatus exception) (pack (show exception)) (primUnsafeCoerce exception))
  where
    Tag name = tagOf exception

tagOf :: (Exception e) => e -> Tag e
tagOf _ = exceptionTag

throwIO :: (Exception e) => e -> IO a
throwIO exception = pure () >>= \() -> throw exception

catch :: (Exception e) => IO a -> (e -> IO a) -> IO a
catch (IO action) handler = IO (primCatch action handle')
  where
    Tag wanted = handles handler
    handle' thrown@(Thrown name _ shown value)
      | wanted == name || wanted == "*" = runIO (handler (caught value shown))
      | otherwise = primThrow thrown

handles :: (Exception e) => (e -> IO a) -> Tag e
handles _ = exceptionTag

handle :: (Exception e) => (e -> IO a) -> IO a -> IO a
handle handler action = catch action handler

try :: (Exception e) => IO a -> IO (Either e a)
try action = catch (fmap Right action) (\exception -> pure (Left exception))

evaluate :: a -> IO a
evaluate value = pure () >>= \() -> value `seq` pure value

-- |
-- Any exception at all, as how it is shown.
data SomeException = SomeException String

instance Show SomeException where
  showsPrec _ (SomeException shown) = showString shown

instance Exception SomeException where
  exceptionTag = Tag "*"
  caught _ shown = SomeException (unpack shown)

data AsyncException
  = StackOverflow
  | HeapOverflow
  | ThreadKilled
  | UserInterrupt
  deriving (Show)

instance Exception AsyncException where
  exceptionTag = Tag "AsyncException"
