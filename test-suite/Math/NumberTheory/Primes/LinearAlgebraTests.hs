module Math.NumberTheory.Primes.LinearAlgebraTests
  ( testSuite
  ) where

import Test.Tasty
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes.Factorisation.LinearAlgebra
import qualified Data.List as L
import qualified Data.Vector as V
import System.Random
import System.IO.Unsafe
import GHC.Clock

testLinear :: Int -> Bool
testLinear dim = dim < 2 || isNull (mat `mult` sol)
  where
    sol = linearSolve mat
    mat = SBMatrix (V.fromList listOfColumns)
    -- -1 is arbitrary. It means that the number of rows is at most one less than
    -- the number of columns. 0.4 is the density
    listOfColumns = L.take dim $ getRandomSBVectors (dim - 1) 0.4 $ mkStdGen $ fromIntegral $ unsafePerformIO getMonotonicTimeNSec

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ testSmallAndQuick "LinearSolver" testLinear
  ]
