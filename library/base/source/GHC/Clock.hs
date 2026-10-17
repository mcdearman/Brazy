module GHC.Clock where

import Data.Word (Word64)
import Hazy (primClock)
import Hazy.Prelude (IO (..))

getMonotonicTime :: IO Double
-- | The time since some moment that does not change, in seconds.
getMonotonicTime = IO (fmap (\thousandths -> fromIntegral thousandths / 1000) primClock)

-- The time since some moment that does not change, in billionths of a
-- second. The runtime counts in thousandths.
getMonotonicTimeNSec :: IO Word64
getMonotonicTimeNSec = IO (fmap (\thousandths -> fromIntegral thousandths * 1000000) primClock)
