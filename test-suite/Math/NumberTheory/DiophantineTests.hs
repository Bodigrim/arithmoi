-- Tests for Math.NumberTheory.Diophantine

{-# LANGUAGE CPP       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.DiophantineTests
  ( testSuite
  ) where

import Test.Tasty

import Math.NumberTheory.Diophantine
import Math.NumberTheory.TestUtils

cornacchiaTest :: Positive Integer -> Positive Integer -> Bool
cornacchiaTest (Positive d) (Positive a) = all checkSoln (cornacchia d m)
  where m = d + a
        checkSoln (x, y) = x*x + d*y*y == m

testSuite :: TestTree
testSuite = testGroup "Diophantine" [ testSmallAndQuick "Cornacchia correct" cornacchiaTest]