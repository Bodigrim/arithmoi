{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.SmoothNumbersBench
  ( benchSuite
  ) where

import Data.List (genericTake)
import Data.Maybe
import Gauge.Main

import Math.NumberTheory.SmoothNumbers

doBench :: Integral a => a -> a
doBench lim = sum $ genericTake lim $ smoothOver $ fromJust $ fromSmoothUpperBound lim

benchSuite :: Benchmark
benchSuite = bgroup "SmoothNumbers"
  [ bench "100"      $ nf doBench    (100 :: Int)
  , bench "1000"     $ nf doBench   (1000 :: Int)
  , bench "10000"    $ nf doBench  (10000 :: Int)
  ]
