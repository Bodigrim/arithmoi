module BinaryExtendedGcdBench (benchmarks) where

import Criterion (Benchmark, bench, nf)
import Math.NumberTheory.GCD (extendedGCD, binaryExtendedGCD)

import Data.Word (Word)

egcdBenchWord :: String -> (Word -> Word -> (Int, Int, Word)) -> Benchmark
egcdBenchWord name f = bench name (nf (f 19234198273) 98176234001)

egcdBenchInt :: String -> (Int -> Int -> (Int, Int, Int)) -> Benchmark
egcdBenchInt name f = bench name (nf (f 19234198273) 98176234001)

egcdBenchInteger :: String -> (Integer -> Integer -> (Integer, Integer, Integer)) -> Benchmark
egcdBenchInteger name f = bench name (nf (f 19234198273) 98176234001)

benchmarks :: [Benchmark]
benchmarks =
    [ egcdBenchWord "binaryExtendedGCD/Word" binaryExtendedGCD

    , egcdBenchInt "extendedGCD/Int" extendedGCD
    , egcdBenchInt "binaryExtendedGCD/Int" binaryExtendedGCD

    , egcdBenchInteger "extendedGCD/Integer" extendedGCD
    , egcdBenchInteger "binaryExtendedGCD/Integer" binaryExtendedGCD
    ]
