module Diagnostics where

import Data.Map (Map)
import Data.Text (Text)
import Hazy.Prelude (placeholder)
import System.IO (Handle)

data Location = Location
  { file :: !Text,
    line :: !Int,
    column :: !Int
  }

data Report = Report
  { code :: !(Maybe Text),
    message :: !Text,
    location :: !Location,
    related :: [(Location, Text)]
  }

type Sources = Map Text Text

load :: [Report] -> IO Sources
load = placeholder

render :: Sources -> [Report] -> String
render = placeholder

print :: Handle -> Sources -> [Report] -> IO ()
print = placeholder
