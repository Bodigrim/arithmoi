{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Math.NumberTheory.Moduli.DiscreteLogarithmTests
  ( testSuite
  ) where

import Data.Maybe
import Numeric.Natural
import Test.Tasty
import Data.Semigroup
import Data.Proxy
import GHC.TypeNats.Compat

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.Moduli.Multiplicative
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.TestUtils

-- | Ensure 'discreteLogarithm' returns in the appropriate range.
discreteLogRange :: Positive Natural -> Integer -> Integer -> Bool
discreteLogRange (Positive m) a b =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> fromMaybe True $ do
      cg <- cyclicGroup :: Maybe (CyclicGroup Integer m)
      a' <- isPrimitiveRoot cg (fromInteger a)
      b' <- isMultElement (fromInteger b)
      return $ discreteLogarithm cg a' b' < totient m

-- | Check that 'discreteLogarithm' inverts exponentiation.
discreteLogarithmProperty :: Positive Natural -> Integer -> Integer -> Bool
discreteLogarithmProperty (Positive m) a b =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> fromMaybe True $ do
      cg <- cyclicGroup :: Maybe (CyclicGroup Integer m)
      a' <- isPrimitiveRoot cg (fromInteger a)
      b' <- isMultElement (fromInteger b)
      return $ discreteLogarithm cg a' b' `stimes` unPrimitiveRoot a' == b'

-- | Check that 'discreteLogarithm' inverts exponentiation in the other direction.
discreteLogarithmProperty' :: Positive Natural -> Integer -> Natural -> Bool
discreteLogarithmProperty' (Positive m) a k =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> fromMaybe True $ do
      cg <- cyclicGroup :: Maybe (CyclicGroup Integer m)
      a'' <- isPrimitiveRoot cg (fromInteger a)
      let a' = unPrimitiveRoot a''
      return $ discreteLogarithm cg a'' (k `stimes` a') == k `mod` totient m

testSuite :: TestTree
testSuite = testGroup "Discrete logarithm"
  [ testSmallAndQuick "output is correct range" discreteLogRange
  , testSmallAndQuick "a^(log_a b) == b"        discreteLogarithmProperty
  , testSmallAndQuick "log_a a^k == k"          discreteLogarithmProperty'
  ]
