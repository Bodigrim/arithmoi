-- Tests for Math.NumberTheory.Diophantine

{-# LANGUAGE CPP       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.DiophantineTests
  ( testSuite
  ) where

import Test.Tasty

import Math.NumberTheory.Primes
import Math.NumberTheory.Diophantine
import Math.NumberTheory.TestUtils

-- Test cornacchia implementation by generating coprime (d,m) with a solution
cornacchiaTest :: Prime Integer -> NonNegative Integer -> Positive Integer -> Bool
cornacchiaTest p (NonNegative a) (Positive b) = all checkSoln solns && (a*d+1, b) `elem` solns
  where d = unPrime p
        m = (a*d+1)^2 + d*b*b
        solns = cornacchia d m
        checkSoln (x, y) = x*x + d*y*y == m

testSuite :: TestTree
testSuite = testGroup "Diophantine" [ testSmallAndQuick "Cornacchia correct" cornacchiaTest]