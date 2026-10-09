module Viewer where

import Data.Text (Text)
import Hazy.Prelude (placeholder)

data Page = Page
  { title :: !Text,
    content :: !Text,
    keywords :: [Text]
  }

view :: Text -> [Page] -> IO ()
view = placeholder
