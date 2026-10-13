module System.Info where

import Hazy (primOS)
import Hazy.Prelude (unpack)

os :: String
os = unpack primOS
