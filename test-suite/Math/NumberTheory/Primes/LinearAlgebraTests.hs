module Math.NumberTheory.Primes.LinearAlgebraTests
  ( testSuite
  ) where

import Test.Tasty
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes.Factorisation.LinearAlgebra
import qualified Data.List as L
import qualified Data.IntSet as S
import qualified Data.Vector as V
import System.Random
import System.IO.Unsafe
import GHC.Clock

testLinear :: Int -> Bool
testLinear dim = dim < 2 || (mat `mult` sol == mempty)
  where
    sol = linearSolve mat
    mat = SBMatrix (V.fromList listOfColumns)
    -- -2 is arbitrary. It means that the number of rows is at most one less than
    -- the number of columns
    listOfColumns = L.take dim $ getRandomColumns [0..(dim - 2)] 0.3 $ mkStdGen $ fromIntegral $ unsafePerformIO getMonotonicTimeNSec

getRandomColumns :: [Int] -> Double -> StdGen -> [SBVector]
getRandomColumns rows sparsity gen = go randomEntries
  where
    randomEntries = zip (cycle rows) (randomRs (0, 1) gen)
    go :: [(Int, Double)] -> [SBVector]
    go list = newVector : go backOfList
      where
        newVector = SBVector (S.fromList listOfEntries)
        listOfEntries = fmap fst $ filter (\(_, rDouble) -> rDouble < sparsity) frontOfList
        (frontOfList, backOfList) = L.splitAt (length rows) list

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ testSmallAndQuick "LinearSolver" testLinear
  ]
