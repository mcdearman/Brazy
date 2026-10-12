module RunSTEscape where

import Control.Monad.ST (ST, runST)

data Tag s = Tag

make :: ST s (Tag s)
make = pure Tag

wrong :: ()
wrong = case runST make of Tag -> ()
