{-# HLINT ignore "Avoid restricted function" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Find a crossover point to switch between two algorithms.
--
-- @since 0.1.1
module Test.Tasty.Bench.Crossover (
  crossover,
  crossovers,
  mkCrossoverConfig,
  CrossoverConfig (..),
) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import System.IO.Unsafe (unsafeInterleaveIO)
import Test.Tasty (Timeout (..), mkTimeout)
import Test.Tasty.Bench (Benchmarkable, RelStDev (..), nf)
import Test.Tasty.Bench.Utils (Measurement (..), measRelStDev, measure, traceShowM')

-- | Configuration for 'crossover' / 'crossovers'.
--
-- @since 0.1.1
data CrossoverConfig = CrossoverConfig
  { eqlFasterOnLow :: Word -> Benchmarkable
  -- ^ A benchmark which is faster at 'eqlLow', typically 'nf' @f@.
  , eqlFasterOnHigh :: Word -> Benchmarkable
  -- ^ A benchmark which is faster at 'eqlHigh', typically 'nf' @g@.
  , eqlLow :: Word
  -- ^ An argument at which 'eqlFasterOnLow' is faster than 'eqlFasterOnHigh'.
  , eqlHigh :: Word
  -- ^ An argument at which 'eqlFasterOnHigh' is faster than 'eqlFasterOnLow'.
  , eqlTimeout :: Timeout
  -- ^ Timeout of individual measurements.
  }

-- | Generate a default 'crossover' / 'crossovers' configuration.
--
-- @since 0.1.1
mkCrossoverConfig
  :: (NFData a)
  => (Word -> a)
  -- ^ An algorithm which is faster for small arguments, without 'nf'.
  -> (Word -> a)
  -- ^ An algorithm which is faster for large arguments, without 'nf'.
  -> (Word, Word)
  -- ^ Small and large arguments.
  -> CrossoverConfig
mkCrossoverConfig fLow fHigh (low, high) =
  CrossoverConfig
    { eqlFasterOnLow = nf fLow
    , eqlFasterOnHigh = nf fHigh
    , eqlLow = low
    , eqlHigh = high
    , eqlTimeout = mkTimeout 1e8
    }

-- | Determine a crossover region to switch between two algorithms.
-- Ideally the returned crossover region is just a point like @(n, n + 1)@,
-- but depending on 'eqlTimeout' it could be a larger interval.
--
-- While suitable for automatic estimates, 'crossover' generally provides bad user
-- experience in interactive environments, because it can take a very long time
-- before it returns a result without any heartbeat in between. Consider using
-- 'crossovers' or enabling @debug@ flag.
--
-- @since 0.1.1
crossover :: CrossoverConfig -> IO (Word, Word)
crossover = fmap NE.last . crossovers

-- | Same as 'crossover', but interactively emits a list of crossover regions,
-- gradually tightening to the final result.
--
-- @since 0.1.1
crossovers :: CrossoverConfig -> IO (NonEmpty (Word, Word))
crossovers CrossoverConfig {..} = NE.fromList <$> go (RelStDev (1 / 3)) eqlLow eqlHigh
  where
    go targetRelStdDev lo hi = fmap ((lo, hi) :) $
      unsafeInterleaveIO $ do
        let mid = (lo + hi) `quot` 2
        if mid == lo
          then pure []
          else do
            (cmp, targetRelStdDev') <- compareBenchmarks eqlTimeout targetRelStdDev (eqlFasterOnLow mid) (eqlFasterOnHigh mid)
            case cmp of
              LT -> go targetRelStdDev' mid hi
              EQ -> pure []
              GT -> go targetRelStdDev' lo mid

compareBenchmarks
  :: Timeout
  -> RelStDev
  -> Benchmarkable
  -> Benchmarkable
  -> IO (Ordering, RelStDev)
compareBenchmarks tmt = go dummyMeasure
  where
    dummyMeasure = Measurement {measTime = 1 / 0, measStDev = 1 / 0}

    go meas1 tgtRelStdDev bench1 bench2 = do
      meas2 <- measure tmt tgtRelStdDev bench2
      traceShowM' (tgtRelStdDev, meas2)
      let derived = deriveRelStdDev meas1 meas2
          derivedTgtRelStdDev = if derived > 0 then derived else tgtRelStdDev
      case compareMeasurements meas1 meas2 of
        LT -> pure (LT, derivedTgtRelStdDev)
        GT -> pure (GT, derivedTgtRelStdDev)
        EQ ->
          if measRelStDev meas2 > tgtRelStdDev
            then pure (EQ, 0.0)
            else do
              let tgtRelStdDev' = max derivedTgtRelStdDev (tgtRelStdDev / 2)
              first flipOrdering <$> go meas2 tgtRelStdDev' bench2 bench1

compareMeasurements
  :: Measurement
  -> Measurement
  -> Ordering
compareMeasurements (Measurement mean1 stdev1) (Measurement mean2 stdev2)
  | mean1 + 2 * stdev1 < mean2 - 2 * stdev2 = LT
  | mean2 + 2 * stdev2 < mean1 - 2 * stdev1 = GT
  | otherwise = EQ

deriveRelStdDev
  :: Measurement
  -> Measurement
  -> RelStDev
deriveRelStdDev (Measurement mean1 _) (Measurement mean2 _)
  | isInfinite mean1 || isInfinite mean2 = RelStDev 0.0
  | otherwise =
      RelStDev $ abs (mean1 - mean2) / max mean1 mean2 / 4

flipOrdering
  :: Ordering
  -> Ordering
flipOrdering = \case
  LT -> GT
  EQ -> EQ
  GT -> LT
