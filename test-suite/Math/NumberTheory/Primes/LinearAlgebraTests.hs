{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Primes.LinearAlgebraTests
  ( testSuite
  ) where

import qualified Data.List as L
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes.Factorisation.LinearAlgebra
import GHC.TypeNats
import Data.Proxy
import Data.Maybe
import System.Random
import System.IO.Unsafe
import System.CPUTime

-- The floating point number is the density of the matrix.
testLinear :: Int -> Bool
testLinear dim = dim < 2 || testLinearSolver dim 0.4

-- Input number of columns of matrix and density coefficient. It returns a random matrix.
testLinearSolver :: Int -> Double -> Bool
testLinearSolver dim density = case someNatVal (fromIntegral dim) of
  SomeNat (_ :: Proxy dim) -> let sol :: DBVector dim = linearSolve dim mat in
    mat `mult` sol == mempty
      where
        mat = SBMatrix $ fromJust $ SV.fromList listOfColumns
        -- Choosing @(dim - 2)@ below implies that the number of rows is at most one less than
        -- the number of columns. This ensures the matrix is singular.
        listOfColumns = L.take dim $ getRandomSBVectors (dim - 2) density $ mkStdGen $ fromIntegral $ unsafePerformIO getCPUTime

-- Infinite lists of random SBVectors.
getRandomSBVectors :: KnownNat k => Int -> Double -> StdGen -> [SBVector k]
getRandomSBVectors numberOfRows density gen = go randomEntries
  where
    randomEntries = map (< density) $ randomRs (0, 1) gen
    go :: KnownNat k => [Bool] -> [SBVector k]
    go list = newVector `seq` (newVector : go backOfList)
      where
        newVector = SBVector (U.fromList listOfEntries)
        listOfEntries = map fst $ filter snd $ zip [minBound..maxBound] frontOfList
        (frontOfList, backOfList) = L.splitAt numberOfRows list

testSuite :: TestTree
testSuite = testGroup "Linear Algebra"
  [ testSmallAndQuick "LinearSolver" testLinear
  ]
