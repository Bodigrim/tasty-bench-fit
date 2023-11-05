{-# LANGUAGE CPP #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Guess complexity of the function.
module Test.Tasty.Bench.Fit (
  -- * Fit benchmarks
  fit,
  fits,
  mkFitConfig,
  FitConfig (..),

  -- * Complexity
  Complexity (..),
  Measurement (..),
  guessComplexity,
  evalComplexity,

  -- * Predicates
  isConstant,
  isLogarithmic,
  isLinear,
  isLinearithmic,
  isQuadratic,
  isCubic,
) where

import Control.DeepSeq (NFData)
import Data.List (maximumBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import System.IO.Unsafe (unsafeInterleaveIO)
import Test.Tasty (Timeout, mkTimeout)
import Test.Tasty.Bench (Benchmarkable, RelStDev (..), measureCpuTimeAndStDev, nf)
import Test.Tasty.Bench.Fit.Complexity (
  Complexity (..),
  Measurement (..),
  evalComplexity,
  guessComplexity,
  isConstant,
  isCubic,
  isLinear,
  isLinearithmic,
  isLogarithmic,
  isQuadratic,
 )

#ifdef DEBUG
import Debug.Trace
#endif

-- | Configuration for 'fit' / 'fits'.
data FitConfig = FitConfig
  { fitBench :: Word -> Benchmarkable
  -- ^ Which function to measure? Typically 'nf' @f@.
  , fitLow :: Word
  -- ^ The smallest size of the input.
  -- It should be as small as possible, but big enough for the main asymptotic
  -- term to dwarf constant overhead and other terms.
  , fitHigh :: Word
  -- ^ The largest size of the input.
  -- As large as practically possible, at least 100x larger than
  -- the smallest size.
  , fitTimeout :: Timeout
  -- ^ Timeout of individual measurements.
  , fitRelStDev :: RelStDev
  -- ^ Target relative standard deviation of individual measurements.
  , fitOracle :: Map Word Measurement -> Complexity
  -- ^ An oracle to determine complexity from measurements.
  -- Typically 'guessComplexity'.
  }

-- | Generate a default 'fit' / 'fits' configuration.
mkFitConfig
  :: (NFData a)
  => (Word -> a)
  -- ^ Raw function to measure, without 'nf'.
  -> (Word, Word)
  -- ^ The smallest and the largest sizes of the input.
  -> FitConfig
mkFitConfig f (low, high) =
  FitConfig
    { fitBench = nf f
    , fitLow = low
    , fitHigh = high
    , fitTimeout = mkTimeout 1e8
    , fitRelStDev = RelStDev 0.02
    , fitOracle = guessComplexity
    }

-- | Determine time complexity of the function:
--
-- >>> fit $ mkFitConfig (\x -> sum [1..x]) (10, 10000)
-- 1.2153e-8 * x
-- >>> fit $ mkFitConfig (\x -> Data.List.nub [1..x]) (10, 10000)
-- 2.8369e-9 * x ^ 2
-- >>> fit $ mkFitConfig (\x -> Data.List.sort $ take (fromIntegral x) $ iterate (\n -> n * 6364136223846793005 + 1) (1 :: Int)) (10, 100000)
-- 5.2990e-8 * x * log x
--
-- One can usually get reliable results for functions, which do not
-- allocate much: like in-place vector sort or fused list operations like
-- 'sum' @[1..x]@.
--
-- Unfortunately, fitting functions, which allocate a lot,
-- is likely to be disappointing: GC kicks in irregularly depending on nursery
-- and heap sizes and often skews observations beyond any recognition.
-- Consider running such measurements with @-O0@ or in @ghci@ prompt. This is how
-- the usage example above was generated. Without optimizations your program
-- allocates much more and triggers GC regularly, somewhat evening out its effect.
--
-- While suitable for automatic estimates, 'fit' generally provides bad user
-- experience in interactive environments, because it can take a very long time
-- before it returns a result without any heartbeat in between. Consider using
-- 'fits' or enabling @debug@ flag.
fit :: FitConfig -> IO Complexity
fit cnf = converge <$> fits cnf

converge :: NonEmpty Complexity -> Complexity
converge xs = case zs of
  [] -> NE.last xs
  (_, _, z) : _ -> z
  where
    ys = NE.toList xs
    zs =
      dropWhile (\(x, y, z) -> p x z || p y z) $
        zip3 ys (drop 1 ys) (drop 2 ys)
    p
      Complexity {cmplVarPower = varPow, cmplLogPower = logPow, cmplMultiplier = mult}
      Complexity {cmplVarPower = varPow', cmplLogPower = logPow', cmplMultiplier = mult'} =
        abs (varPow - varPow') > 0.001
          || logPow /= logPow'
          || abs ((mult - mult') / mult) > 0.01

-- | Same as 'fit', but interactively emits a list of complexities,
-- gradually converging to the final result.
--
-- If 'fit' takes too long, you might wish to implement your own criterion
-- of convergence atop of 'fits' directly.
--
-- >>> cmpls <- fits $ mkFitConfig (\x -> sum [1..x]) (10, 10000)
-- >>> traverse print cmpls
-- 3.36e-8 * x ^ 0.903
-- 1.39e-8 * x
-- 1.38e-8 * x
-- ...
fits :: FitConfig -> IO (NonEmpty Complexity)
fits FitConfig {..} = unsafeInterleaveIO $ do
  lowTime <- measure fitLow
  highTime <- measure fitHigh
  let mp = M.fromList [(fitLow, lowTime), (fitHigh, highTime)]
      cmpl = fitOracle mp
  cmpl `seq` (cmpl :|) <$> go mp
  where
    measure :: Word -> IO Measurement
    measure =
      fmap (uncurry Measurement)
        . measureCpuTimeAndStDev fitTimeout fitRelStDev
        . fitBench

    processGap
      :: forall t
       . (Ord t)
      => [(Word, t)]
      -> Map Word Measurement
      -> IO (Map Word Measurement)
    processGap gaps mp
      | M.null gaps' = pure mp
      | otherwise = (\m -> M.insert maxGap m mp) <$> measure maxGap
      where
        gaps' = M.fromList gaps `M.difference` mp
        maxGap = fst $ maximumBy (comparing snd) $ M.toList gaps'

    go :: Map Word Measurement -> IO [Complexity]
    go mp = unsafeInterleaveIO $ do
      let xys = M.toAscList $ fmap measTime mp
          paired = zip xys (drop 1 xys)

          arithGaps :: [(Word, Double)]
          arithGaps =
            map
              (\((x, tx), (y, ty)) -> (round ((d x + d y) / 2), ty - tx))
              paired

          geomGaps :: [(Word, Double)]
          geomGaps =
            map
              (\((x, tx), (y, ty)) -> (round (sqrt (d x * d y)), ty / tx))
              paired

      mp' <- processGap arithGaps mp
      mp'' <- processGap geomGaps mp'
      traceShowM' (M.keys mp'')
      let cmpl = fitOracle mp''
      traceShowM' cmpl
      (cmpl :) <$> (if mp == mp'' then pure [] else go mp'')

d :: Word -> Double
d = fromIntegral

traceShowM' :: (Applicative m, Show a) => a -> m ()
#ifdef DEBUG
traceShowM' = traceShowM
#else
traceShowM' = const (pure ())
#endif
