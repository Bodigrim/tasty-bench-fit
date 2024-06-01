{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Tasty.Bench.Utils (
  Measurement (..),
  measRelStDev,
  measure,
  traceShowM',
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Test.Tasty (Timeout)
import Test.Tasty.Bench (Benchmarkable, RelStDev (..), measureCpuTimeAndStDev)
import Text.Printf (printf)

#ifdef DEBUG
import Debug.Trace
#endif

-- | Represents a time measurement for a given problem's size.
data Measurement = Measurement
  { measTime :: !Double
  , measStDev :: !Double
  }
  deriving (Eq, Ord, Generic)

instance Show Measurement where
  show (Measurement t err) = printf "%.3g ± %.3g" t err

instance NFData Measurement

measure :: Timeout -> RelStDev -> Benchmarkable -> IO Measurement
measure x y z = uncurry Measurement <$> measureCpuTimeAndStDev x y z

measRelStDev :: Measurement -> RelStDev
measRelStDev (Measurement mean stDev) = RelStDev (stDev / mean)

traceShowM' :: (Applicative m, Show a) => a -> m ()
#ifdef DEBUG
traceShowM' = traceShowM
#else
traceShowM' = const (pure ())
#endif
