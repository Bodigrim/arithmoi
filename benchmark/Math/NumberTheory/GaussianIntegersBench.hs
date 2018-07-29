{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Math.NumberTheory.GaussianIntegersBench
  ( benchSuite
  ) where

import Control.DeepSeq
import Gauge.Main

import Math.NumberTheory.GaussianIntegers

instance NFData GaussianInteger

doBench :: Integer -> Benchmark
doBench n = bench ("findPrime'/" ++ show n) $ nf findPrime' n

benchSuite :: Benchmark
benchSuite = bgroup "Gaussian" $ map doBench [1000033, 10000121, 100000037, 1000000009, 10000000033, 100000000057, 1000000000061, 10000000000037]
