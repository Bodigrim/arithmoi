{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Primes.LinearAlgebraTests
  ( testSuite
  ) where

import qualified Data.Set as S
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
import Debug.Trace

-- The floating point number is the density of the matrix.
testLinear :: Int -> Int -> Int ->  Bool
testLinear dim seedSol seedMat = case someNatVal (fromIntegral dim) of
  SomeNat (_ :: Proxy dim) -> dim < 6 || mat `mult` solution == mempty
    where
      solution = linearSolve seedSol mat
      mat :: SBMatrix dim = getRandomMatrix dim (mkStdGen seedMat) 0.3

testVariation :: Int -> Int -> Int -> Bool
testVariation dim seedSol seedMat = case someNatVal (fromIntegral dim) of
  SomeNat (_ :: Proxy dim) -> dim < 6 || numberOfSols > 1
    where
      numberOfSols = trace ("Matrix: " ++ show mat) $ (S.size . S.fromList) solutions
      solutions = map (`linearSolve` mat) [seedSol..seedSol + 24]
      mat :: SBMatrix dim = getRandomMatrix dim (mkStdGen seedMat) 0.3

getRandomMatrix :: KnownNat k => Int -> StdGen -> Double -> SBMatrix k
getRandomMatrix dim seedMat density = SBMatrix $ fromJust . SV.fromList $ listOfColumns
    where
      -- Choosing @(dim - 1)@ below implies that the number of rows is at most one less than
      -- the number of columns. This ensures the matrix is singular.
      listOfColumns = L.take dim $ getRandomSBVectors (dim - 5) density seedMat

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
  , testSmallAndQuick "Variation of Solutions" testVariation
  ]


-- -- Input number of columns of matrix and density coefficient. It returns a random matrix.
-- testLinearSolver :: KnownNat k => Int -> Int -> StdGen -> Double -> DBVector k
-- testLinearSolver dim seedSol seedMat density = case someNatVal (fromIntegral dim) of
--   SomeNat (_ :: Proxy dim) -> let sol :: DBVector dim = linearSolve seedSol mat in
--     sol
--       where
--         mat = SBMatrix $ fromJust $ SV.fromList listOfColumns
--         -- Choosing @(dim - 1)@ below implies that the number of rows is at most one less than
--         -- the number of columns. This ensures the matrix is singular.
--         listOfColumns = L.take dim $ getRandomSBVectors (dim - 1) density seedMat
