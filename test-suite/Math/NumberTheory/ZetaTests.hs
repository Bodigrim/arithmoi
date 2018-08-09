-- |
-- Module:      Math.NumberTheory.ZetaTests
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Test driver for @Math.NumberTheory.Zeta.RiemannTests@ and
-- @Math.NumberTheory.Zeta.DirichletTests@
--

module Math.NumberTheory.ZetaTests
  ( testSuite
  ) where

import Test.Tasty (TestTree, testGroup)

import qualified Math.NumberTheory.Zeta.DirichletTests as DirichletTests
import qualified Math.NumberTheory.Zeta.RiemannTests as RiemannTests

testSuite :: TestTree
testSuite = testGroup "Zeta functions"
  [ DirichletTests.testSuite
  , RiemannTests.testSuite
  ]
