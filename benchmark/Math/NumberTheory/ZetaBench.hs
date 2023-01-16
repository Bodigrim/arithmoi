{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ZetaBench
  ( benchSuite
  ) where

import Gauge.Main

import qualified Data.List.Infinite as Inf
import Math.NumberTheory.Zeta

benchSuite :: Benchmark
benchSuite = bgroup "Zeta"
  [ bench "riemann zeta"   $ nf (sum . Inf.take 20 . zetas) (1e-15 :: Double)
  , bench "dirichlet beta" $ nf (sum . Inf.take 20 . betas) (1e-15 :: Double)
  ]
