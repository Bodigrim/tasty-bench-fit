-- | Generate samples from a given interval.
module Test.Tasty.Bench.Fit.Samples (
  genSamples,
) where

import Data.List (sort)
import qualified Data.List.NonEmpty as NE

-- | Generate exponentially-distributed samples from a given interval.
-- The idea is that benchmarking a function
-- on these samples should provide suitable amount of data
-- to determine asymptotic time complexity.
--
-- >>> genSamples 10 1000
-- [10,16,25,40,63,100,158,251,398,631,1000]
genSamples :: Int -> Int -> [Int]
genSamples low high
  | low' > high' = []
  | low' == high' = [low]
  | otherwise = nubOrd $ map mkSample [0 .. count]
  where
    d :: Int -> Double
    d = fromIntegral

    low' = d $ max 1 low
    high' = d $ max 1 high
    count = round (logBase 2 (high' - low'))

    mkSample :: Int -> Int
    mkSample c =
      round $
        max low' $
          min high' $
            low' ** (d (count - c) / d count) * high' ** (d c / d count)

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = map NE.head . NE.group . sort
