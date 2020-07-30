{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.DiscreteLogarithmTests
  ( testSuite
  ) where

import Test.Tasty

import Data.Maybe
import Data.Mod
import Data.Proxy
import Data.Semigroup
import GHC.TypeNats (SomeNat(..), KnownNat, someNatVal)
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.Moduli.Multiplicative
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Primes
import Math.NumberTheory.TestUtils

nextPrimitiveRoot :: (KnownNat m, UniqueFactorisation a, Integral a) => CyclicGroup a m -> Mod m -> Maybe (PrimitiveRoot m)
nextPrimitiveRoot cg g = listToMaybe $ mapMaybe (isPrimitiveRoot cg) [g..g+100]

nextMultElement :: KnownNat m => Mod m -> Maybe (MultMod m)
nextMultElement g = listToMaybe $ mapMaybe isMultElement [g..g+100]

-- | Ensure 'discreteLogarithm' returns in the appropriate range.
discreteLogRange :: Positive Natural -> Integer -> Integer -> Bool
discreteLogRange (Positive m) a b =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> (/= Just False) $ do
      cg <- cyclicGroup :: Maybe (CyclicGroup Integer m)
      a' <- nextPrimitiveRoot cg (fromInteger a)
      b' <- nextMultElement (fromInteger b)
      return $ discreteLogarithm cg a' b' < totient m

-- | Check that 'discreteLogarithm' inverts exponentiation.
discreteLogarithmProperty :: Positive Natural -> Integer -> Integer -> Bool
discreteLogarithmProperty (Positive m) a b =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> (/= Just False) $ do
      cg <- cyclicGroup :: Maybe (CyclicGroup Integer m)
      a' <- nextPrimitiveRoot cg (fromInteger a)
      b' <- nextMultElement (fromInteger b)
      return $ discreteLogarithm cg a' b' `stimes` unPrimitiveRoot a' == b'

-- | Check that 'discreteLogarithm' inverts exponentiation in the other direction.
discreteLogarithmProperty' :: Positive Natural -> Integer -> Natural -> Bool
discreteLogarithmProperty' (Positive m) a k =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> (/= Just False) $ do
      cg <- cyclicGroup :: Maybe (CyclicGroup Integer m)
      a'' <- nextPrimitiveRoot cg (fromInteger a)
      let a' = unPrimitiveRoot a''
      return $ discreteLogarithm cg a'' (k `stimes` a') == k `mod` totient m

testSuite :: TestTree
testSuite = testGroup "Discrete logarithm"
  [ testSmallAndQuick "output is correct range" discreteLogRange
  , testSmallAndQuick "a^(log_a b) == b"        discreteLogarithmProperty
  , testSmallAndQuick "log_a a^k == k"          discreteLogarithmProperty'
  ]
