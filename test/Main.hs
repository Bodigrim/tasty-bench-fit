{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Data.List (nub, sort)
import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.Bench (RelStDev(..))
import Test.Tasty.Bench.Fit
import Test.Tasty.ExpectedFailure
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testShow
  , testGuess
  , testProperty "sum is linear" $ once $ ioProperty $ do
    c <- fit $ mkFitConfig (\x -> sum [1..x]) (10, 10000)
    pure $ counterexample (show c) $ isLinear c
  -- , testProperty "nub is quadratic" $ ioProperty $ do
  --   c <- fit $ mkFitConfig (\x -> nub [1..x]) (10, 10000)
  --   pure $ counterexample (show c) $ isQuadratic c
  -- , testProperty "sort is linearithmic" $ ioProperty $ do
  --   c <- fit $ (mkFitConfig (\x -> Data.List.sort $ take (fromIntegral x) $
  --     iterate (\n -> n * 6364136223846793005 + 1) (1 :: Word)) (10, 100000))
  --   pure $ counterexample (show c) $ isLinearithmic c
  ]

testShow :: TestTree
testShow = testGroup "Show"
  [ testProperty "Constant" $ once $
    show (Complexity {cmplMultiplier = 5.978911764705882e-9, cmplVarPower = 0, cmplLogPower = 0}) === "5.98e-9"
  , testProperty "Logarithmic" $ once $
    show (Complexity {cmplMultiplier = 5.968840304461118e-8, cmplVarPower = 0, cmplLogPower = 1}) === "5.97e-8 * log x"
  , testProperty "Logarithmic2" $ once $
    show (Complexity {cmplMultiplier = 5.793061712300056e-8, cmplVarPower = 0, cmplLogPower = 2}) === "5.79e-8 * (log x) ^ 2"
  , testProperty "Sqrt" $ once $
    show (Complexity {cmplMultiplier = 8.974841776295381e-10, cmplVarPower = 0.4690504530830262, cmplLogPower = 0}) === "8.97e-10 * x ^ 0.469"
  , testProperty "Linear" $ once $
    show (Complexity {cmplMultiplier = 5.793579541827657e-10, cmplVarPower = 1, cmplLogPower = 0}) === "5.79e-10 * x"
  , testProperty "Linearithmic" $ once $
    show (Complexity {cmplMultiplier = 1.354437369864389e-9, cmplVarPower = 1, cmplLogPower = 1}) === "1.35e-9 * x * log x"
  , testProperty "Linearithmic2" $ once $
    show (Complexity {cmplMultiplier = 5.754981209362351e-10, cmplVarPower = 1, cmplLogPower = 2}) === "5.75e-10 * x * (log x) ^ 2"
  , testProperty "Quadratic" $ once $
    show (Complexity {cmplMultiplier = 5.763471892706858e-10, cmplVarPower = 2, cmplLogPower = 0}) === "5.76e-10 * x ^ 2"
  , testProperty "Cubic" $ once $
    show (Complexity {cmplMultiplier = 5.736276961481605e-10, cmplVarPower = 3, cmplLogPower = 0}) === "5.74e-10 * x ^ 3"
  ]

testGuess :: TestTree
testGuess = testGroup "Guess"
  [ testProperty "Constant" $ once $ checkComplexity isConstant
    [(10,5.881e-9),(15,6.05e-9),(20,6.0e-9),(30,5.938e-9),(40,6.074e-9),(61,6.194e-9),(80,6.282e-9),(122,6.105e-9),(160,6.108e-9),(244,5.927e-9),(320,5.93e-9),(488,5.949e-9),(640,5.9e-9),(976,5.892e-9),(1280,5.918e-9),(1953,5.901e-9),(2560,5.918e-9),(3906,5.947e-9),(5120,5.901e-9),(7812,5.906e-9),(10240,5.907e-9),(15625,5.95e-9),(20480,5.982e-9),(31250,5.993e-9),(40960,5.946e-9),(62500,5.901e-9),(81920,5.922e-9),(125000,5.93e-9),(163840,5.928e-9),(250000,6.228e-9),(327680,5.961e-9),(500000,6.025e-9),(655360,5.929e-9),(1000000,5.96e-9)]
  , testProperty "Logarithmic" $ once $ checkComplexity isLogarithmic
    [(10,1.54191e-7),(15,1.7357e-7),(20,1.90191e-7),(30,2.20661e-7),(40,2.31167e-7),(61,2.55251e-7),(80,2.71941e-7),(122,2.97561e-7),(160,3.1158e-7),(244,3.38001e-7),(320,3.50998e-7),(488,3.7589e-7),(640,3.93221e-7),(976,4.15118e-7),(1280,4.30965e-7),(1953,4.5525e-7),(2560,4.70494e-7),(3906,4.98164e-7),(5120,5.1214e-7),(7812,5.50508e-7),(10240,5.51175e-7),(15625,5.75977e-7),(20480,5.98614e-7),(31250,6.14841e-7),(40960,6.32332e-7),(62500,6.60092e-7),(81920,6.67221e-7),(125000,6.94264e-7),(163840,7.08961e-7),(250000,7.3384e-7),(327680,7.48206e-7),(500000,7.75123e-7),(655360,7.90332e-7),(1000000,8.12204e-7)]
  , testProperty "Logarithmic2" $ once $ checkComplexity isLogarithmic2
    [(10,3.2606e-7),(15,4.41324e-7),(20,5.44409e-7),(30,6.84828e-7),(40,8.1833e-7),(61,1.017501e-6),(80,1.155663e-6),(122,1.376089e-6),(160,1.527048e-6),(244,1.783776e-6),(320,1.963467e-6),(488,2.255293e-6),(640,2.482833e-6),(976,2.813505e-6),(1280,2.995748e-6),(1953,3.392042e-6),(2560,3.609635e-6),(3906,4.011062e-6),(5120,4.266189e-6),(7812,4.679779e-6),(10240,4.963383e-6),(15625,5.409899e-6),(20480,5.70888e-6),(31250,6.266229e-6),(40960,6.554699e-6),(62500,7.130075e-6),(81920,7.429367e-6),(125000,7.953335e-6),(163840,8.318951e-6),(250000,8.901232e-6),(327680,9.295489e-6),(500000,9.926727e-6),(655360,1.034403e-5),(1000000,1.0997949e-5)]
  , testProperty "Logarithmic6" $ once $ checkComplexity isLogarithmic6
    [(10,8.628924e-6),(15,2.2718701e-5),(20,4.1468945e-5),(30,8.90083e-5),(40,1.44395898e-4),(61,2.76677148e-4),(80,4.07342187e-4),(122,7.0664375e-4),(160,9.83112109e-4),(244,1.594223437e-3),(320,2.119596093e-3),(488,3.235329687e-3),(640,4.227651562e-3),(976,6.111234375e-3),(1280,7.672078125e-3),(1953,1.08728375e-2),(2560,1.344470625e-2),(3906,1.8441325e-2),(5120,2.2353e-2),(7812,2.97963125e-2),(10240,3.5516875e-2),(15625,4.6447275e-2),(20480,5.477735e-2),(31250,7.038505e-2),(40960,8.21456e-2),(62500,0.10381915),(81920,0.1200662),(125000,0.1495741),(163840,0.1715495),(250000,0.2110635),(327680,0.2401788),(500000,0.2922768),(655360,0.3303902),(1000000,0.3993154)]
  , testProperty "Sqrt" $ once $ checkComplexity isSqrt
    [(10,9.179e-9),(15,9.129e-9),(20,9.721e-9),(30,1.0412e-8),(40,1.0851e-8),(61,1.1441e-8),(80,1.2102e-8),(122,1.3903e-8),(160,1.4497e-8),(244,1.6241e-8),(320,1.746e-8),(488,2.0412e-8),(640,2.2189e-8),(976,2.5812e-8),(1280,2.7865e-8),(1953,3.3284e-8),(2560,3.5825e-8),(3906,4.7608e-8),(5120,5.3362e-8),(7812,6.3584e-8),(10240,7.0786e-8),(15625,8.4566e-8),(20480,9.4943e-8),(31250,1.13772e-7),(40960,1.29419e-7),(62500,1.56251e-7),(81920,1.76977e-7),(125000,2.16214e-7),(163840,2.46864e-7),(250000,3.01931e-7),(327680,3.43224e-7),(500000,4.24035e-7),(655360,4.80871e-7),(1000000,5.90708e-7)]
  , testProperty "Linear" $ once $ checkComplexity isLinear
    [(10,1.2137e-8),(19,1.7499e-8),(20,1.8083e-8),(39,2.849e-8),(40,2.9045e-8),(78,5.6604e-8),(80,5.7759e-8),(156,1.01422e-7),(160,1.03728e-7),(312,1.9109e-7),(320,1.962e-7),(625,3.73153e-7),(640,3.81767e-7),(1250,7.34564e-7),(1280,7.49969e-7),(2500,1.469915e-6),(2560,1.505307e-6),(5000,2.896041e-6),(5120,2.967195e-6),(10000,5.777413e-6)]
  , testProperty "Linearithmic" $ once $ checkComplexity isLinearithmic
    [(10,2.2405e-8),(19,4.8021e-8),(20,5.0446e-8),(39,9.8831e-8),(40,1.02064e-7),(78,2.13687e-7),(80,2.20389e-7),(156,4.71726e-7),(160,5.15425e-7),(312,1.094096e-6),(320,1.141173e-6),(625,2.407279e-6),(640,2.426035e-6),(1250,5.195858e-6),(1280,5.407168e-6),(2500,1.1405377e-5),(2560,1.1602465e-5),(5000,2.4561621e-5),(5120,2.5307434e-5),(10000,5.5566052e-5)]
  , testProperty "Linearithmic2" $ once $ checkComplexity isLinearithmic2
    [(10,4.7181e-8),(19,1.12792e-7),(20,1.22449e-7),(39,3.20318e-7),(40,3.32249e-7),(78,8.75328e-7),(80,9.04648e-7),(156,2.329658e-6),(160,2.419274e-6),(312,6.00791e-6),(320,6.189526e-6),(625,1.5036761e-5),(640,1.5424169e-5),(1250,3.6494995e-5),(1280,3.7753466e-5),(2500,8.7988085e-5),(2560,9.1063623e-5),(5000,2.09651562e-4),(5120,2.13942675e-4),(10000,4.88197265e-4)]
  , testProperty "Quadratic" $ once $ checkComplexity isQuadratic
    [(10,6.9102e-8),(15,1.41744e-7),(20,2.42625e-7),(31,5.7397e-7),(40,9.56442e-7),(62,2.248535e-6),(80,3.740179e-6),(125,9.10119e-6),(160,1.4752551e-5),(250,3.5977856e-5),(320,5.9150854e-5),(500,1.44069628e-4),(640,2.36670605e-4),(1000,5.76093359e-4)]
  , testProperty "Quadrithmic4" $ once $ checkComplexity isQuadrithmic4
    [(5,3.5023e-8),(9,1.59489e-7),(10,2.12688e-7),(19,1.627995e-6),(20,1.919146e-6),(39,1.5753015e-5),(40,1.704226e-5),(78,1.25921337e-4),(80,1.35570312e-4),(156,9.08977343e-4),(160,9.75295703e-4),(312,6.07931875e-3),(320,6.496996875e-3),(625,3.86003e-2),(640,4.0853375e-2),(1250,0.2314005),(1280,0.2456381),(2500,1.339004),(2560,1.4231234),(5000,7.528904)]
  , testProperty "Cubic" $ once $ checkComplexity isCubic
    [(10,5.98066e-7),(11,7.7601e-7),(20,4.609741e-6),(23,6.99185e-6),(40,3.6676177e-5),(46,5.5854089e-5),(80,2.92877148e-4),(93,4.66834472e-4),(160,2.344111718e-3),(187,3.740370312e-3),(320,1.8751396875e-2),(375,3.018281875e-2),(640,0.150031325),(750,0.2414234),(1280,1.1982404),(1500,1.9280254),(2560,9.5926602),(3000,15.4408018)]
  , testProperty "Quartic" $ once $ checkComplexity isQuartic
    [(5,1.6603e-8),(7,3.0392e-8),(10,9.1955e-8),(15,3.25706e-7),(20,9.76705e-7),(31,5.34298e-6),(40,1.472517e-5),(62,8.4625488e-5),(80,2.34295019e-4),(125,1.395396875e-3),(160,3.74655e-3),(250,2.23739875e-2),(320,6.0027625e-2),(500,0.357446),(640,0.9591132),(1000,5.725087)]

  , testProperty "Karatsuba" $ once $ checkComplexity isKaratsuba
    [(10,4.8364e-8),(12,6.0228e-8),(20,9.9186e-8),(24,1.21309e-7),(40,2.31972e-7),(48,2.9911e-7),(80,6.2843e-7),(97,8.46736e-7),(160,1.846619e-6),(195,2.503655e-6),(320,5.42057e-6),(390,7.376403e-6),(640,1.6124441e-5),(781,2.2078021e-5),(1280,4.8210919e-5),(1562,6.6098962e-5),(2560,1.44684423e-4),(3125,1.98921875e-4),(5120,4.34806054e-4),(6250,5.9602539e-4),(10240,1.301581445e-3),(12500,1.784770312e-3),(20480,3.903389843e-3),(25000,5.354964062e-3),(40960,1.171065625e-2),(50000,1.604174375e-2),(81920,3.509155625e-2),(100000,4.82393125e-2)]
  , testProperty "Strassen" $ once $ checkComplexity isStrassen
    [(10,4.02343e-7),(11,5.14162e-7),(20,2.64022e-6),(23,3.884503e-6),(40,1.8058444e-5),(46,2.6682556e-5),(80,1.258875e-4),(93,1.92064208e-4),(160,8.8356914e-4),(187,1.362714843e-3),(320,6.163835937e-3),(375,9.710895312e-3),(640,4.32488375e-2),(750,6.8603265625e-2),(1280,0.30328925),(1500,0.4761277),(2560,2.1247246),(3000,3.3290002)]

  , testProperty "Linear again" $ once $ checkComplexity isLinear
    [(1,6.9753e-8),(2,1.28492e-7),(4,2.46261e-7),(5,3.05419e-7),(8,4.82596e-7),(10,6.01227e-7)]
  ]

checkComplexity :: (Complexity -> Bool) -> [(Word, Double)] -> Property
checkComplexity p xys = counterexample (show cmpl) (p cmpl)
  where
    cmpl = guessComplexity $ fmap (`Measurement` 1) $ M.fromList xys

isSqrt :: Complexity -> Bool
isSqrt = \case
  Complexity {cmplVarPower = varPow, cmplLogPower = 0} -> varPow > 0.4 && varPow < 0.6
  _ -> False

isLogarithmic2 :: Complexity -> Bool
isLogarithmic2 = \case
  Complexity {cmplVarPower = 0, cmplLogPower = 2} -> True
  _ -> False

isLogarithmic6 :: Complexity -> Bool
isLogarithmic6 = \case
  Complexity {cmplVarPower = 0, cmplLogPower = 6} -> True
  _ -> False

isLinearithmic2 :: Complexity -> Bool
isLinearithmic2 = \case
  Complexity {cmplVarPower = 1, cmplLogPower = 2} -> True
  _ -> False

isQuadrithmic4 :: Complexity -> Bool
isQuadrithmic4 = \case
  Complexity {cmplVarPower = 2, cmplLogPower = 4} -> True
  _ -> False

isQuartic :: Complexity -> Bool
isQuartic = \case
  Complexity {cmplVarPower = 4, cmplLogPower = 0} -> True
  _ -> False

isKaratsuba :: Complexity -> Bool
isKaratsuba = \case
  Complexity {cmplVarPower = varPow, cmplLogPower = 0} -> varPow > 1.5 && varPow < 1.7
  _ -> False

isStrassen :: Complexity -> Bool
isStrassen = \case
  Complexity {cmplVarPower = varPow, cmplLogPower = 0} -> varPow > 2.7 && varPow < 2.9
  _ -> False
