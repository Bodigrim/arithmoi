module Main (main) where

import Criterion.Main (bgroup, defaultMain)
import qualified BinaryExtendedGcdBench

main :: IO ()
main = defaultMain
    [ bgroup "Extended GCD" BinaryExtendedGcdBench.benchmarks
    ]

