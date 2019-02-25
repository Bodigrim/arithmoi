-- Model fitting to derive coefficients in
-- Math.NumberTheory.Primes.Sequence.chooseAlgorithm

module Main where

import Numeric.GSL.Fitting

-- | Benchmarks Sequence/filterIsPrime
-- ([start, length], ([time in microseconds], weight))
filterIsPrimeBenchData :: [([Double], ([Double], Double))]
filterIsPrimeBenchData =
  [ ([100000, 1000], ([777], 0.1))
  , ([100000, 10000], ([8523], 0.1))
  , ([1000000, 1000], ([813], 0.1))
  , ([1000000, 10000], ([8247], 0.1))
  , ([1000000, 100000], ([78600], 0.1))
  , ([10000000, 1000], ([765], 0.1))
  , ([10000000, 10000], ([7685], 0.1))
  , ([10000000, 100000], ([78900], 0.1))
  , ([10000000, 1000000], ([785000], 0.1))
  , ([100000000, 1000], ([792], 0.1))
  , ([100000000, 10000], ([8094], 0.1))
  , ([100000000, 100000], ([79280], 0.1))
  , ([100000000, 1000000], ([771600], 0.1))
  , ([100000000, 10000000], ([7670000], 0.1))
  ]

filterIsPrimeBenchModel :: [(Double, Double)]
filterIsPrimeBenchModel = sol
  where
    model [d] [from, len] = [len * d]
    modelDer [d] [from, len] = [[len]]
    (sol, _) = fitModelScaled 1E-10 1E-10 20 (model, modelDer) filterIsPrimeBenchData [1]

filterIsPrimeBenchApprox :: ([Double], ([Double], Double)) -> [Double]
filterIsPrimeBenchApprox ([from, len], ([exact], _)) = [from, len, exact, fromInteger (floor (appr / exact * 1000)) / 1000]
  where
    [(d, _)] = filterIsPrimeBenchModel
    appr = len * d

-- | Benchmarks Sequence/eratosthenes
-- ([start, length], ([time in microseconds], weight))
eratosthenesData :: [([Double], ([Double], Double))]
eratosthenesData =
  [ ([10000000000,1000000], ([21490], 0.1))
  , ([10000000000,10000000], ([103200], 0.1))
  , ([10000000000,100000000], ([956800], 0.1))
  , ([10000000000,1000000000], ([9473000], 0.1))
  , ([100000000000,10000000], ([107000], 0.1))
  , ([1000000000000,10000000], ([129900], 0.1))
  , ([10000000000000,10000000], ([202900], 0.1))
  , ([100000000000000,10000000], ([420400], 0.1))
  , ([1000000000000000,10000000], ([1048000], 0.1))
  , ([10000000000000000,10000000], ([2940000], 0.1))
  , ([100000000000000000,10000000], ([8763000], 0.1))
  ]

eratosthenesModel :: [(Double, Double)]
eratosthenesModel = sol
  where
    model [a, b, c] [from, len] = [a * len + b * sqrt from + c]
    modelDer [a, b, c] [from, len] = [[len, sqrt from, 1]]
    (sol, _) = fitModelScaled 1E-10 1E-10 20 (model, modelDer) eratosthenesData [1,0,0]

eratosthenesApprox :: ([Double], ([Double], Double)) -> [Double]
eratosthenesApprox ([from, len], ([exact], _)) = [from, len, exact, fromInteger (floor (appr / exact * 1000)) / 1000]
  where
    [(a, _), (b, _), (c, _)] = eratosthenesModel
    appr = a * len + b * sqrt from + c

coeffs :: (Double, Double)
coeffs = (b / (d - a), c / (d - a))
  where
    [(a, _), (b, _), (c, _)] = eratosthenesModel
    [(d, _)] = filterIsPrimeBenchModel

main :: IO ()
main = do
  print filterIsPrimeBenchModel
  mapM_ (print . filterIsPrimeBenchApprox) filterIsPrimeBenchData
  print eratosthenesModel
  mapM_ (print . eratosthenesApprox) eratosthenesData
  print coeffs
