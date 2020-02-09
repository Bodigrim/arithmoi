-- |
-- Module:       Math.NumberTheory.RootsOfUnityTests
-- Licence:      MIT
--
-- Tests for Math.NumberTheory.RootsOfUnity
--

module Math.NumberTheory.RootsOfUnityTests where

import Test.Tasty

import Data.Complex
import Data.Ratio
import Data.Semigroup

import Math.NumberTheory.RootsOfUnity
import Math.NumberTheory.TestUtils (testSmallAndQuick, Positive(..))

rootOfUnityTest :: Integer -> Positive Integer -> Bool
rootOfUnityTest n (Positive d) = toComplex ((d `div` gcd n d) `stimes` toRootOfUnity (n % d)) == (1 :: Complex Double)

testSuite :: TestTree
testSuite = testSmallAndQuick "RootOfUnity contains roots of unity" rootOfUnityTest
