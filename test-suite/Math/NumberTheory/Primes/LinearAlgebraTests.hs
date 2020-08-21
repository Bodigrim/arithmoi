-- |
-- Module:      Math.NumberTheory.Primes.LinearAlgebraTests
-- Copyright:   (c) 2020 Federico Bongiorno
-- Licence:     MIT
-- Maintainer:  Federico Bongiorno <federicobongiorno97@gmail.com>
--
-- Tests for Math.NumberTheory.Primes.Factorisation.LinearAlgebra
--

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
-- import qualified Data.Set as S
-- import Debug.Trace

-- This test checks if the vector computed in @linearSolve@ is non-empty and
-- is a solution of the matrix.
testLinear :: Positive Int -> Int ->  Bool
testLinear (Positive dim') seedMat = let dim = dim' + 6 in case someNatVal (fromIntegral dim) of
  SomeNat (Proxy :: Proxy dim) -> mat `mult` solution == mempty && solution /= mempty
    where
      -- 0 is the random seed.
      solution = linearSolve 0 mat
      -- The floating point number is the density of the matrix.
      mat :: SBMatrix dim = getRandomMatrix dim (mkStdGen seedMat) 0.3

-- This test checks whether @linearSolve@ finds different solutions when inputting
-- different random seeds. This is desirable property when using @linearSolve@
-- in the context of integer factorisation. As it stands, the test fails. More
-- precisely, in 10% of the cases, it only finds one solution in 25 attempts.
-- To test, uncomment the two lines importing files at the top, uncomment
-- the test below and uncomment the line at the bottom in the @testSuite@.

-- testVariation :: Positive Int -> Int -> Bool
-- testVariation (Positive dim') seedMat = let dim = dim' + 6 in case someNatVal (fromIntegral dim) of
--   SomeNat (Proxy :: Proxy dim) -> numberOfSols > 1
--     where
--       numberOfSols = trace ("Matrix: " ++ show mat) $ (S.size . S.fromList) solutions
--       solutions = map (`linearSolve` mat) $ take 25 $ [0..]
--       mat :: SBMatrix dim = getRandomMatrix dim (mkStdGen seedMat) 0.3

-- Generates a random matrix.
getRandomMatrix :: KnownNat k => Int -> StdGen -> Double -> SBMatrix k
getRandomMatrix dim seedMat density = SBMatrix $ fromJust . SV.fromList $ listOfColumns
    where
      -- Choosing @(dim - 5)@ implies that the number of rows is five less than
      -- the number of columns. This ensures the matrix is singular.
      listOfColumns = L.take dim $ getRandomSBVectors (dim - 5) density seedMat

-- Infinite lists of random SBVectors to be used as columns of the random matrix.
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
  --, testSmallAndQuick "Variation of Solutions" testVariation
  ]
