module Utils.Time(nanos) where

import Reporting.Logs
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)

nanos :: LattePipeline Int
nanos = liftIO $ floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime