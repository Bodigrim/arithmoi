{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ZetaBench
  ( benchSuite
  ) where

import Test.Tasty.Bench

import Math.NumberTheory.Zeta

benchSuite :: Benchmark
benchSuite = bgroup "Zeta"
  [ bench "riemann zeta"   $ nf (sum . take 20 . zetas) (1e-15 :: Double)
  , bench "dirichlet beta" $ nf (sum . take 20 . betas) (1e-15 :: Double)
  ]
