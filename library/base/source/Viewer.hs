-- |
-- Pages of text, shown one after another: what the interactive session of a
-- compiler that Hazy made shows for @:ir@. The compiler GHC makes shows them
-- in a window of the terminal, a page at a time with a tab for each
-- ("viewer/Viewer.hs", over the @brick@ package), which needs more of the
-- terminal than this library has. Here each page is written out with its
-- title over it, and the words to pick out are not picked out.
module Viewer where

import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

data Page = Page
  { title :: !Text,
    content :: !Text,
    keywords :: [Text]
  }

view :: Text -> [Page] -> IO ()
view heading pages = do
  Text.IO.putStrLn heading
  mapM_ page pages
  where
    page Page {title, content} = do
      putStrLn ""
      putStr "== "
      Text.IO.putStrLn title
      Text.IO.putStrLn content
