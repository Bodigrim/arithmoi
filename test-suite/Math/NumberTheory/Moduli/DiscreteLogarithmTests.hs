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
import Test.Tasty.HUnit

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Moduli.DiscreteLogarithm
import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.TestUtils

-- | Ensure 'discreteLogarithm' returns in the appropriate range.
discreteLogRange :: Positive Natural -> Integer -> Integer -> Bool
discreteLogRange (Positive m) a b =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> fromMaybe True $ do
      a' <- isPrimitiveRoot (fromInteger a :: Mod m)
      b' <- isMultElement (fromInteger b)
      return $ discreteLogarithm a' b' < totient m

-- | Check that 'discreteLogarithm' inverts exponentiation.
discreteLogarithmProperty :: Positive Natural -> Integer -> Integer -> Bool
discreteLogarithmProperty (Positive m) a b =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> fromMaybe True $ do
      a' <- isPrimitiveRoot (fromInteger a :: Mod m)
      b' <- isMultElement (fromInteger b)
      return $ discreteLogarithm a' b' `stimes` unPrimitiveRoot a' == b'

-- | Check that 'discreteLogarithm' inverts exponentiation in the other direction.
discreteLogarithmProperty' :: Positive Natural -> Integer -> Natural -> Bool
discreteLogarithmProperty' (Positive m) a k =
  case someNatVal m of
    SomeNat (_ :: Proxy m) -> fromMaybe True $ do
      a'' <- isPrimitiveRoot (fromInteger a :: Mod m)
      let a' = unPrimitiveRoot a''
      return $ discreteLogarithm a'' (k `stimes` a') == k `mod` totient m

-- cases = mapMaybe makeCase [ (5,  8, 10^9 + 7)
--                           , (2,  7, 3^20)
--                           , (2,  3, 10^11 + 3)
--                           , (3, 17, 5^16)
--                           ]

pointTest1 :: Assertion
pointTest1 = discreteLogarithm a b @?= 145514840
  where Just a = isPrimitiveRoot (5 :: Mod (10^9 + 7))
        Just b = isMultElement 8
pointTest2 :: Assertion
pointTest2 = discreteLogarithm a b @?= 151298512
  where Just a = isPrimitiveRoot (2 :: Mod (3^20))
        Just b = isMultElement 7
pointTest3 :: Assertion
pointTest3 = discreteLogarithm a b @?= 1452889085
  where Just a = isPrimitiveRoot (2 :: Mod (10^11 + 3))
        Just b = isMultElement 3
pointTest4 :: Assertion
pointTest4 = discreteLogarithm a b @?= 70218319539
  where Just a = isPrimitiveRoot (3 :: Mod (5^16))
        Just b = isMultElement 17

testSuite :: TestTree
testSuite = testGroup "Discrete logarithm"
  [ testSmallAndQuick "output is correct range" discreteLogRange
  , testSmallAndQuick "a^(log_a b) == b"        discreteLogarithmProperty
  , testSmallAndQuick "log_a a^k == k"          discreteLogarithmProperty'
  , testGroup "example cases" [ testCase "large prime" pointTest1
                              , testCase "prime power" pointTest2
                              , testCase "larger prime" pointTest3
                              , testCase "prime to larger power" pointTest4
                              ]
  ]
