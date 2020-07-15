module Math.NumberTheory.Primes.LinearAlgebraTests
  ( testSuite
  ) where

import Test.Tasty
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes.Factorisation.LinearAlgebra

testLinear :: Int -> Bool
testLinear dim = dim < 2 || testLinearSolver dim 0.4

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ testSmallAndQuick "LinearSolver" testLinear
  ]
