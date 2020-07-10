-- Tests for Math.NumberTheory.Diophantine

{-# LANGUAGE CPP       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.DiophantineTests
  ( testSuite
  ) where

import Data.List (sort)

import Test.Tasty

import Math.NumberTheory.Diophantine
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.TestUtils

cornacchiaTest :: Positive Integer -> Positive Integer -> Bool
cornacchiaTest (Positive d) (Positive a) = all checkSoln (cornacchia d m)
  where m = d + a
        checkSoln (x, y) = x*x + d*y*y == m

-- Testing against a slower reference implementation
cornacchiaBruteForce :: Positive Integer -> Positive Integer -> Bool
cornacchiaBruteForce (Positive d) (Positive a) = findSolutions [] 1 == sort (cornacchia d m)
  where m = d + a
        -- Simple O(sqrt (m/d)) brute force by considering all possible y values
        findSolutions acc y
          | x2 <= 0     = acc
          | x*x == x2   = findSolutions ((x,y) : acc) (y+1)
          | otherwise   = findSolutions acc (y+1)
            where x2 = m - d*y*y
                  x = integerSquareRoot x2

testSuite :: TestTree
testSuite = testGroup "Diophantine"
  [ testSmallAndQuick "Cornacchia correct" cornacchiaTest
  , testSmallAndQuick "Cornacchia same solutions as brute force" cornacchiaBruteForce
  ]