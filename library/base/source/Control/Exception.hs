module Control.Exception
  ( module Control.Exception,
    IOException,
    ErrorCall (..),
    ArithException (..),
    PatternMatchFail (..),
  )
where

import Data.Text (Text, pack, unpack)
import Hazy (primCatch, primThrow, primUnsafeCoerce)
import Hazy.Prelude (IO (..), runIO)
import Hazy.PreludeIO (Any, ArithException (..), ErrorCall (..), IOException, PatternMatchFail (..), Thrown (..))

-- What a type of exception is called, to tell it from the others.
newtype Tag e = Tag String

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

-- What is thrown is a 'Thrown', which the runtime has, so that what goes
-- wrong with input and output is thrown in the same way.

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

-- |
-- The first, and the second as well if the first throws, which is thrown
-- on as it was.
onException :: IO a -> IO b -> IO a
onException (IO action) (IO cleanup) = IO (primCatch action (\thrown -> cleanup >> primThrow (thrown :: Thrown)))

-- | The first, and then the second, whether the first gives or throws.
finally :: IO a -> IO b -> IO a
finally action cleanup = do
  result <- action `onException` cleanup
  _ <- cleanup
  pure result

-- |
-- What is had for a while: it is had, used, and given back, whether its
-- use gives or throws.
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket acquire release use = do
  resource <- acquire
  result <- use resource `onException` release resource
  _ <- release resource
  pure result

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

-- | What is had, given back only if its use throws.
bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError acquire release use = do
  resource <- acquire
  use resource `onException` release resource

-- | Nothing is thrown at a program from outside, so there is nothing to
-- hold off.
mask_ :: IO a -> IO a
mask_ action = action

uninterruptibleMask_ :: IO a -> IO a
uninterruptibleMask_ action = action

evaluate :: a -> IO a
evaluate value = pure () >>= \() -> value `seq` pure value

-- |
-- Any exception at all, as how it is shown.
data SomeException = SomeException String

instance Show SomeException where
  showsPrec _ (SomeException shown) = showString shown

-- What 'error' throws. One that nothing catches ends the program with its
-- message, as an error does.
instance Exception ErrorCall where
  exceptionTag = Tag "ErrorCall"
  exceptionStatus _ = -2

-- What a division by nothing throws, which ends a program that does not
-- catch it as an error does.
instance Exception ArithException where
  exceptionTag = Tag "ArithException"
  exceptionStatus _ = -2

-- What is thrown where no pattern matches.
instance Exception PatternMatchFail where
  exceptionTag = Tag "PatternMatchFail"
  exceptionStatus _ = -2

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

-- What goes wrong with input and output, which the Prelude throws.
instance Exception IOException where
  exceptionTag = Tag "IOException"
