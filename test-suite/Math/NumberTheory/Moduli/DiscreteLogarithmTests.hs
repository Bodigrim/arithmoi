{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Math.NumberTheory.Moduli.DiscreteLogarithmTests
  ( testSuite
  ) where

import Data.Maybe
import Numeric.Natural
import Test.Tasty

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.DiscreteLogarithm
import Math.NumberTheory.TestUtils

-- | Check that 'discreteLogarithm' computes the logarithm
discreteLogarithmProperty :: Positive Natural -> Integer -> Integer -> Bool
discreteLogarithmProperty (Positive m) a b = fromMaybe True $ case modulo a m of
    SomeMod a' -> let b' = realToFrac b in do 
      e <- discreteLogarithm a' b'
      Just $ a' ^% e == b'
    InfMod  {} -> error "Impossible"

-- | Check that 'discreteLogarith' finds a solution when one exists
discreteLogarithm2Property :: Positive Natural -> Integer -> NonNegative Integer -> Bool
discreteLogarithm2Property (Positive m) a (NonNegative e) = isJust $ case a `modulo` m of
  SomeMod a' -> let b = a' ^% e in discreteLogarithm a' b
  InfMod {} -> error "Impossible"

testSuite :: TestTree
testSuite = testGroup "Discrete logarithm"
  [ testSmallAndQuick "discreteLogarithm" discreteLogarithmProperty
  , testSmallAndQuick "discreteLogarithm2" discreteLogarithm2Property
  ]
