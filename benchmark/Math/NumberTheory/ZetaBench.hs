{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ZetaBench
  ( benchSuite
  ) where

import Gauge.Main

import Math.NumberTheory.Zeta

benchSuite :: Benchmark
benchSuite = bgroup "Zeta"
  [ bench "riemann zeta"   $ nf (\eps -> sum $ take 20 $ zetas eps) (1e-15 :: Double)
  , bench "dirichlet beta" $ nf (\eps -> sum $ take 20 $ betas eps) (1e-15 :: Double)
  ]
