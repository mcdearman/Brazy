module Data.STRef
  ( STRef,
    newSTRef,
    readSTRef,
    writeSTRef,
    modifySTRef,
    modifySTRef',
  )
where

import Control.Monad.ST (ST)
import Hazy (primNewRef, primReadRef, primSameRef, primWriteRef)

-- A place that holds a value. It is the runtime's, and has nothing that can
-- be matched.
data STRef s a

-- Two are equal if they are the one place.
instance Eq (STRef s a) where
  a == b = primSameRef a b

newSTRef :: a -> ST s (STRef s a)
newSTRef = primNewRef

readSTRef :: STRef s a -> ST s a
readSTRef = primReadRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef = primWriteRef

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef reference f = do
  value <- readSTRef reference
  writeSTRef reference (f value)

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' reference f = do
  value <- readSTRef reference
  let changed = f value
  changed `seq` writeSTRef reference changed
