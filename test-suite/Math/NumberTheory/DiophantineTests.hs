-- Tests for Math.NumberTheory.Diophantine

{-# LANGUAGE CPP       #-}
{-# LANGUAGE RecordWildCards, GADTs #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.DiophantineTests
  ( testSuite
  ) where

import Data.List (sort)

import Test.Tasty

import Math.NumberTheory.Diophantine
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes

cornacchiaTest :: Positive Integer -> Positive Integer -> Bool
cornacchiaTest (Positive d) (Positive a) = gcd d m /= 1 || all checkSoln (cornacchia d m)
  where m = d + a
        checkSoln (x, y) = x*x + d*y*y == m

-- Testing against a slower reference implementation on coprime inputs
cornacchiaBruteForce :: Positive Integer -> Positive Integer -> Bool
cornacchiaBruteForce (Positive d) (Positive a) = gcd d m /= 1 || findSolutions [] 1 == sort (cornacchia d m)
  where m = d + a
        -- Simple O(sqrt (m/d)) brute force by considering all possible y values
        findSolutions acc y
          | x2 <= 0     = acc
          | x*x == x2   = findSolutions ((x,y) : acc) (y+1)
          | otherwise   = findSolutions acc (y+1)
            where x2 = m - d*y*y
                  x = integerSquareRoot x2

linearTest :: (a ~ Integer) => a -> a -> a -> a -> Bool
linearTest a b c k =
  case solveLinear Lin {..} of
    Nothing -> True -- Disproving this would require a counter example
    Just ls | (x, y) <- runLinearSolution ls k
            -> a*x + b*y == c

linearTest' :: (a ~ Integer) => Prime a -> Prime a -> a -> a -> Bool
linearTest' l c' d k =
  case solveLinear Lin {..} of
    Nothing -> l == c'
    Just ls | (x, y) <- runLinearSolution ls k
            -> a*x + b*y == c
  where
    a = unPrime l
    b = unPrime c'
    c = d


testSuite :: TestTree
testSuite = testGroup "Diophantine"
  [ testSmallAndQuick "Cornacchia correct" cornacchiaTest
  , testSmallAndQuick "Cornacchia same solutions as brute force" cornacchiaBruteForce
  , testSmallAndQuick "Linear correct" linearTest
  , testSmallAndQuick "Linear correct #2" linearTest'
  ]

