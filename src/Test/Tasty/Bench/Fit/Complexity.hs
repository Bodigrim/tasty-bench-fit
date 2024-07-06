{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Guess complexity from data.
module Test.Tasty.Bench.Fit.Complexity (
  Complexity (..),
  guessComplexity,
  guessPolynomialComplexity,
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
import Data.List (intercalate, minimumBy)
import Data.List.Infinite (Infinite (..), (...))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Math.Regression.Simple (
  Fit (..),
  V2 (..),
  levenbergMarquardt1WithYerrors,
  levenbergMarquardt2WithYerrors,
  linear,
 )
import Test.Tasty.Bench.Utils (Measurement (..))
import Text.Printf (printf)
import Prelude hiding (log)
import qualified Prelude as P

#ifdef DEBUG
import Debug.Trace
#endif

log :: Word -> Double
log x = if x >= 1 then P.log (d x) else 0

-- | 'Complexity' @a@ @b@ @k@ represents a time complexity
-- \( k \, x^a \log^b x \), where \( x \) is problem's size.
data Complexity = Complexity
  { cmplVarPower :: !Double
  , cmplLogPower :: !Word
  , cmplMultiplier :: !Double
  }
  deriving (Eq, Ord, Generic)

instance NFData Complexity

-- | Is the complexity \( f(x) = k \)?
isConstant :: Complexity -> Bool
isConstant = \case
  Complexity {cmplVarPower = 0, cmplLogPower = 0} -> True
  _ -> False

-- | Is the complexity \( f(x) = k \log x \)?
isLogarithmic :: Complexity -> Bool
isLogarithmic = \case
  Complexity {cmplVarPower = 0, cmplLogPower = 1} -> True
  _ -> False

-- | Is the complexity \( f(x) = k \, x \)?
isLinear :: Complexity -> Bool
isLinear = \case
  Complexity {cmplVarPower = 1, cmplLogPower = 0} -> True
  _ -> False

-- | Is the complexity \( f(x) = k \, x \log x \)?
isLinearithmic :: Complexity -> Bool
isLinearithmic = \case
  Complexity {cmplVarPower = 1, cmplLogPower = 1} -> True
  _ -> False

-- | Is the complexity \( f(x) = k \, x^2 \)?
isQuadratic :: Complexity -> Bool
isQuadratic = \case
  Complexity {cmplVarPower = 2, cmplLogPower = 0} -> True
  _ -> False

-- | Is the complexity \( f(x) = k \, x^3 \)?
isCubic :: Complexity -> Bool
isCubic = \case
  Complexity {cmplVarPower = 3, cmplLogPower = 0} -> True
  _ -> False

instance Show Complexity where
  show Complexity {..} =
    intercalate " * " $
      filter
        (not . null)
        [ case cmplMultiplier of
            1 -> ""
            _ -> printf "%.2g" cmplMultiplier
        , case cmplVarPower of
            0 -> ""
            1 -> "x"
            _ -> "x ^ " <> round3 cmplVarPower
        , case cmplLogPower of
            0 -> ""
            1 -> "log x"
            _ -> "(log x) ^ " <> show cmplLogPower
        ]
    where
      round3 :: Double -> String
      round3 x = if x == d x' then show x' else printf "%.3f" x
        where
          x' :: Word
          x' = round x

-- | Evaluate time complexity for a given size of the problem.
evalComplexity :: Complexity -> Word -> Double
evalComplexity Complexity {..} x =
  cmplMultiplier * d x ** cmplVarPower * log x ^ cmplLogPower

bestOf :: [(Complexity, Double)] -> Complexity
bestOf = fst . minimumBy (comparing weigh)
  where
    weigh (Complexity {..}, wssr) =
      wssr
        * powPenalty
        -- Penalty for high power of logarithm.
        * d (max 1 cmplLogPower)
      where
        -- Penalty for non-integer power.
        powPenalty :: Double
        powPenalty = case abs (cmplVarPower - d (round cmplVarPower)) of
          0 -> 1
          -- Severe penalty for almost integer powers
          diff ->
            if diff < 0.05
              then 100
              else (if diff < 0.15 then 32 else 10)

-- | Guess time complexity from a map where keys
-- are problem's sizes and values are time measurements (or instruction counts).
--
-- >>> :set -XNumDecimals
-- >>> guessComplexity $ Data.Map.fromList $ map (\(x, t) -> (x, Measurement t 1)) [(2, 4), (3, 10), (4, 15), (5, 25)]
-- 0.993 * x ^ 2
-- >>> guessComplexity $ Data.Map.fromList $ map (\(x, t) -> (x, Measurement t 1)) [(1e2, 2.1), (1e3, 2.9), (1e4, 4.1), (1e5, 4.9)]
-- 0.433 * log x
--
-- This function uses following simplifying assumptions:
--
-- * All coefficients are non-negative.
-- * The power of \( \log x \) ('cmplLogPower') is unlikely to be \( > 1 \).
-- * The power of \( x \) ('cmplVarPower') is unlikely to be fractional.
--
-- This function is unsuitable to guess
-- [superpolynomial](https://en.wikipedia.org/wiki/Time_complexity#Superpolynomial_time)
-- and higher classes of complexity.
guessComplexity :: Map Word Measurement -> Complexity
guessComplexity xys =
  trace'
    ("guessComplexity " ++ show (M.assocs xys))
    bestOf
    (takeUntilLocalMin cmpls)
  where
    cmpls :: Infinite ((Complexity, Double), (Complexity, Double))
    cmpls = fmap (guessComplexityForFixedLog xys) (0 ...)

    takeUntilLocalMin
      :: Infinite ((Complexity, Double), (Complexity, Double))
      -> [(Complexity, Double)]
    takeUntilLocalMin ((c1, c2) :< (c3, c4) :< cs)
      | snd c1 > snd c3 || snd c2 > snd c4 =
          c1 : c2 : takeUntilLocalMin ((c3, c4) :< cs)
      | otherwise =
          [c1, c2]

-- | Same as guessComplexity, but the power of \( \log x \) ('cmplLogPower')
-- is pinned to be 0.
--
-- @since 0.1.1
guessPolynomialComplexity :: Map Word Measurement -> Complexity
guessPolynomialComplexity xys =
  trace'
    ("guessPolynomialComplexity " ++ show (M.assocs xys))
    bestOf
    [cmpl1, cmpl2]
  where
    (cmpl1, cmpl2) = guessComplexityForFixedLog xys 0

guessComplexityForFixedLog
  :: Map Word Measurement
  -> Word
  -> ((Complexity, Double), (Complexity, Double))
guessComplexityForFixedLog xys logPow = trace' msg res
  where
    -- varPow might be negative here, so always pass it through mkCmpl
    V2 _ varPow = guessComplexityWithoutLog xys logPow
    mkCmpl varPow' = guessComplexityForFixedPowAndLog xys varPow' logPow
    res@((res1, wssr1), (res2, wssr2)) =
      (mkCmpl (max 0 varPow), mkCmpl (d (round varPow)))

    msg =
      printf
        "forFixedLog:\n\t%s, RSS %.4g\n\t%s, RSS %.4g"
        (show res1)
        wssr1
        (show res2)
        wssr2

guessComplexityWithoutLog :: Map Word Measurement -> Word -> V2
guessComplexityWithoutLog (M.assocs -> xys) logPow = finish
  where
    -- Fit y_i ~ a x_i^b, which is equivalent to log y_i ~ log a + b log x_i.
    -- This is not ideal, because minimizing the sum of (log y_i - log a - b log x_i) ^ 2
    -- is not equivalent to minimizing the sum of (y_i - a * x_i^b) ^ 2, but close enough,
    -- so we are going to use it as a starting point for Levenberg-Marquardt.
    V2 b0 la0 =
      linear (\(x, Measurement y _) -> (log x, P.log (y / log x ^ logPow))) xys
    start = V2 (exp la0) (max 0 b0)

    Fit {fitParams = finish} =
      NE.last $
        levenbergMarquardt2WithYerrors
          ( \(V2 mult varPow) (x, Measurement y err) ->
              ( y
              , mult * d x ** varPow * log x ^ logPow
              , V2
                  (d x ** varPow * log x ^ logPow)
                  (mult * d x ** varPow * log x ^ (logPow + 1))
              , err
              )
          )
          start
          xys

guessComplexityForFixedPowAndLog
  :: Map Word Measurement
  -> Double
  -> Word
  -> (Complexity, Double)
guessComplexityForFixedPowAndLog (M.assocs -> xys) varPow logPow = (res, wssr)
  where
    -- We want to find a which minimizes \sum_i (y_i - a f(x_i))^2 for f(x) = x^b * log^c x.
    -- Then d/da = 0 means that \sum_i (2 a f(x_i)^2 - 2 f(x_i) y_i) = 0
    -- or equivalently a = \sum_i f(x_i) y_i / \sum_i x_i^2.
    eval x = d x ** varPow * log x ^ logPow
    sumXY = sum $ map (\(x, Measurement y _) -> eval x * y) xys
    sumX2 = sum $ map (\(x, _) -> eval x ** 2) xys
    start = sumXY / sumX2

    ft =
      NE.last $
        levenbergMarquardt1WithYerrors
          ( \mult (x, Measurement y err) ->
              ( y
              , mult * d x ** varPow * log x ^ logPow
              , d x ** varPow * log x ^ logPow
              , err
              )
          )
          start
          xys
    res =
      Complexity
        { cmplMultiplier = fitParams ft
        , cmplVarPower = varPow
        , cmplLogPower = logPow
        }
    wssr = fitWSSR ft

d :: Word -> Double
d = fromIntegral

trace' :: String -> b -> b
#ifdef DEBUG
trace' = trace
#else
trace' = const id
#endif
