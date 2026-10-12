module InstanceEquality where

class Sum f where
  total :: f -> ()

data Box a = Box a

instance (a ~ ()) => Sum (Box a) where
  total (Box x) = x

wrong :: ()
wrong = total (Box (Box ()))
