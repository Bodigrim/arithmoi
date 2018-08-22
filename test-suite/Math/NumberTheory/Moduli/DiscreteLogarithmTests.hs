{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Math.NumberTheory.Moduli.DiscreteLogarithmTests
  ( testSuite
  ) where

import Data.Maybe
import Numeric.Natural
import Test.Tasty
import Data.Proxy
import GHC.TypeNats.Compat

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Moduli.DiscreteLogarithm
import Math.NumberTheory.TestUtils

-- | Check that 'discreteLogarithm' computes the logarithm
discreteLogarithmProperty :: Positive Natural -> Integer -> Integer -> Bool
discreteLogarithmProperty (Positive m) a b =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> fromMaybe True $ do
      a' <- isPrimitiveRoot (fromInteger a :: Mod m)
      b' <- isMultElement (fromInteger b :: Mod m)
      let e = discreteLogarithm a' b'
      return $ powMod (fromInteger a :: Mod m) e == (fromInteger b :: Mod m)

testSuite :: TestTree
testSuite = testGroup "Discrete logarithm"
  [ testSmallAndQuick "discreteLogarithm" discreteLogarithmProperty
  ]
