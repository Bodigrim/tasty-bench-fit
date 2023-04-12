{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | Guess complexity from data.
module Test.Tasty.Bench.Fit.Complexity (
  Complexity (..),
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

import Data.List (foldl', intercalate, minimumBy)
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Math.Regression.Simple
import Text.Printf (printf)
import Prelude hiding (log)
import qualified Prelude as P

#ifdef DEBUG
import Debug.Trace
#endif

log :: Double -> Double
log x = if x > 1 then P.log x else 0

avg :: [Double] -> Double
avg xs = a / b
  where
    (a, b) = foldl' (\(acc, len) x -> (acc + x, len + 1)) (0, 0) xs

stdev :: [Double] -> Double
stdev xs = sqrt $ (/ fromIntegral (length xs)) $ sum $ map (\x -> (x - avg xs) ** 2) xs

-- | 'Complexity' @a@ @b@ @c@ represents a time complexity
-- \( a \, x^b \log^c x \), where \( x \) is problem's size.
data Complexity = Complexity
  { cmplMultiplier :: Double
  , cmplVarPower :: Double
  , cmplLogPower :: Double
  }
  deriving (Eq)

-- | Is the complexity \( f(x) = a \)?
isConstant :: Complexity -> Bool
isConstant = \case
  Complexity _ 0 0 -> True
  _ -> False

-- | Is the complexity \( f(x) = a \log x \)?
isLogarithmic :: Complexity -> Bool
isLogarithmic = \case
  Complexity _ 0 1 -> True
  _ -> False

-- | Is the complexity \( f(x) = a \, x \)?
isLinear :: Complexity -> Bool
isLinear = \case
  Complexity _ 1 0 -> True
  _ -> False

-- | Is the complexity \( f(x) = a \, x \log x \)?
isLinearithmic :: Complexity -> Bool
isLinearithmic = \case
  Complexity _ 1 1 -> True
  _ -> False

-- | Is the complexity \( f(x) = a \, x^2 \)?
isQuadratic :: Complexity -> Bool
isQuadratic = \case
  Complexity _ 2 0 -> True
  _ -> False

-- | Is the complexity \( f(x) = a \, x^3 \)?
isCubic :: Complexity -> Bool
isCubic = \case
  Complexity _ 3 0 -> True
  _ -> False

fromV3 :: V3 -> Complexity
fromV3 (V3 a b c) = normalizeComplexity (Complexity a b c)

normalizeComplexity :: Complexity -> Complexity
normalizeComplexity (Complexity a b c) =
  Complexity (max 0 a) (max 0 b) (max 0 c)

instance Show Complexity where
  show (Complexity a b c) =
    intercalate " * " $
      filter
        (not . null)
        [ if a /= 1 then printf "%.4g" a else ""
        , if b /= 0 then ("x" <> (if b == 1 then "" else " ^ " <> round4 b)) else ""
        , if c /= 0 then ("log" <> (if c == 1 then "" else " ^ " <> round4 c) <> " x") else ""
        ]
    where
      round4 :: Double -> String
      round4 x = if x == fromIntegral x' then show x' else printf "%.4f" x
        where
          x' :: Int
          x' = truncate x

-- | Evaluate time complexity for a given size of the problem.
evalComplexity :: Complexity -> Double -> Double
evalComplexity (Complexity a b c) x =
  a * x ** b * (log x) ** c

diffAsComplexity :: V3 -> Double -> V3
diffAsComplexity v3@(V3 a' b' c') x =
  V3
    (if a' < 0 then 0 else x ** b * (log x) ** c)
    (if b' < 0 then 0 else a * x ** b * (log x) ** (c + 1))
    (if c' < 0 then 0 else a * x ** b * (log x) ** c * log (log x))
  where
    Complexity a b c = normalizeComplexity (fromV3 v3)

wssrComplexity :: Complexity -> [(Double, Double)] -> Double
wssrComplexity cmpl xys =
  sum $ map (\(x, y) -> (y - evalComplexity cmpl x) ^ (2 :: Int)) xys

bestOf :: [Complexity] -> [(Double, Double)] -> Complexity
bestOf cs xys = minimumBy (comparing weigh) cs
  where
    weigh cmpl@(Complexity _ b c) =
      wssrComplexity cmpl xys
        -- Penalty for non-integer power. Just fine-tuned magic numbers.
        * (if b == b' then 1 else (if b <= 1 then 7 else 14))
        -- Penalty for high power of logarithm.
        * (max 1 c)
      where
        b' = fromIntegral (round b :: Int)

stdevComplexity :: [Complexity] -> Complexity
stdevComplexity cs =
  Complexity
    { cmplMultiplier = stdev $ map cmplMultiplier cs
    , cmplVarPower = stdev $ map cmplVarPower cs
    , cmplLogPower = stdev $ map cmplLogPower cs
    }

-- | Guess time complexity from a list of pairs, where the first component
-- is problem's size and the second component is problem's time.
--
-- >>> guessComplexity [(2, 4), (3, 10), (4, 15), (5, 25)]
-- 0.9928 * x ^ 2
-- >>> guessComplexity [(1e2, 2.1), (1e3, 2.9), (1e4, 4.1), (1e5, 4.9)]
-- 0.4327 * log x
--
-- This functions uses following simplifying assumptions:
--
-- * All coefficients are non-negative.
-- * The power of \( \log x \) ('cmplLogPower') is an integer.
-- * The power of \( x \) ('cmplVarPower') is unlikely to be close to integer fractional number such as 0.9 or 2.1.
--
-- This function is unsuitable to guess
-- [superpolynomial](https://en.wikipedia.org/wiki/Time_complexity#Superpolynomial_time)
-- and higher classes of complexity.
guessComplexity :: [(Double, Double)] -> Complexity
guessComplexity = fst . guessComplexityInterval

guessComplexityInterval :: [(Double, Double)] -> (Complexity, Complexity)
guessComplexityInterval xs =
  ( guessSingleComplexity xs
  , stdevComplexity $ bootstrapComplexities xs
  )

bootstrapComplexities :: [(Double, Double)] -> [Complexity]
bootstrapComplexities xs = map guessSingleComplexity xss
  where
    xss = map (\i -> let (ys, zs) = splitAt i xs in ys <> drop 1 zs) [0 .. length xs - 1]

guessSingleComplexity :: [(Double, Double)] -> Complexity
guessSingleComplexity xys = guessComplexityFromInit start xys
  where
    start = V3 (avg (map snd xys)) 0 0

guessComplexityFromInit :: V3 -> [(Double, Double)] -> Complexity
guessComplexityFromInit start xys = tryToImprovePow (fromV3 finish) xys
  where
    Fit {fitParams = finish} =
      NE.last $
        levenbergMarquardt3
          ( \v3 (x, y) ->
              ( y
              , evalComplexity (fromV3 v3) x
              , diffAsComplexity v3 x
              )
          )
          start
          xys

-- Power of the main term is likely an integer
tryToImprovePow :: Complexity -> [(Double, Double)] -> Complexity
tryToImprovePow (Complexity origA origB origC) xys =
  bestOf (origFit <> floorFit <> ceilingFit) xys
  where
    origFit = guessComplexityForFixedPow (V3 origA origB origC) xys
    floorFit = guessComplexityForFixedPow (V3 origA (fromIntegral (floor origB :: Int)) origC) xys
    ceilingFit = guessComplexityForFixedPow (V3 origA (fromIntegral (ceiling origB :: Int)) origC) xys

guessComplexityForFixedPow :: V3 -> [(Double, Double)] -> [Complexity]
guessComplexityForFixedPow (V3 _ b initC) xys = concat fits
  where
    -- Power of the logarithmic term is always an integer
    fits =
      [ let c = fromIntegral (ceiling initC :: Int)
         in findFirstLocalMin (c NE.:| [c + 1, c + 2 ..])
      , let f = fromIntegral (floor initC :: Int)
         in findFirstLocalMin (f NE.:| [f - 1, f - 2 .. 0])
      ]

    findFirstLocalMin cs = go cmpl cmpls
      where
        cmpl NE.:| cmpls = fmap (\c -> guessComplexityForFixedPowAndLog (V3 1 b c) xys) cs

        go curr [] = [curr]
        go curr (next : rest)
          | wssrComplexity curr xys > wssrComplexity next xys = curr : go next rest
          | otherwise = [curr]

-- We want to find a which minimizes \sum_i (y_i - a f(x_i))^2 for f(x) = x^b * log^c x.
-- Then d/da = 0 means that \sum_i (2 a f(x_i)^2 - 2 f(x_i) y_i) = 0
-- or equivalently a = \sum_i f(x_i) y_i / \sum_i x_i^2.
guessComplexityForFixedPowAndLog :: V3 -> [(Double, Double)] -> Complexity
guessComplexityForFixedPowAndLog (V3 _ b c) xys =
  trace'
    (printf "guessComplexityForFixedPowAndLog %s, RSS %.4g" (show res) (wssrComplexity res xys))
    res
  where
    eval x = evalComplexity (fromV3 (V3 1 b c)) x
    sumXY = sum $ map (\(x, y) -> eval x * y) xys
    sumX2 = sum $ map (\(x, _) -> eval x ** 2) xys
    fitA = sumXY / sumX2
    res = fromV3 (V3 fitA b c)

trace' :: String -> b -> b
#ifdef DEBUG
trace' = trace
#else
trace' = const id
#endif
