{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.MertensBench
  ( benchSuite
  ) where

import Gauge.Main
#if __GLASGOW_HASKELL__ < 709
import Data.Word
#endif

import Math.NumberTheory.ArithmeticFunctions.Mertens

mertensBench :: Word -> Benchmark
mertensBench n = bench (show n) (nf mertens n)

benchSuite :: Benchmark
benchSuite = bgroup "Mertens" $ map mertensBench $ take 4 $ iterate (* 10) 10000000
