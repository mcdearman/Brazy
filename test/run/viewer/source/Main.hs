module Main where

-- The pages the interactive session shows, written out one after another.

import Data.Text (pack)
import Viewer (Page (..), view)

main :: IO ()
main =
  view
    (pack "two pages")
    [ Page {title = pack "first", content = pack "one\ntwo", keywords = []},
      Page {title = pack "second", content = pack "three", keywords = [pack "three"]}
    ]
