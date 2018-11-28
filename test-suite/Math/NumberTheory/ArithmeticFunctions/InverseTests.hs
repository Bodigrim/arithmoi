-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.InverseTests
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.ArithmeticFunctions.Inverse
--

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ArithmeticFunctions.InverseTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative
import Data.Semigroup
import Data.Semiring (Semiring(..))
import qualified Data.Set as S
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.ArithmeticFunctions.Inverse
import Math.NumberTheory.Euclidean
import Math.NumberTheory.Primes
import Math.NumberTheory.Recurrences
import Math.NumberTheory.TestUtils

-------------------------------------------------------------------------------
-- Totient

totientProperty1 :: forall a. (Semiring a, Euclidean a, UniqueFactorisation a, Ord a) => Positive a -> Bool
totientProperty1 (Positive x) = Product x `S.member` (inverseTotient (S.singleton . Product) (totient x))

totientProperty2 :: (Semiring a, Euclidean a, UniqueFactorisation a, Ord a) => Positive a -> Bool
totientProperty2 (Positive x) = all (== x) (S.map (totient . getProduct) (inverseTotient (S.singleton . Product) x))

-- | http://oeis.org/A055506
totientCountFactorial :: [Word]
totientCountFactorial =
  [ 2
  , 3
  , 4
  , 10
  , 17
  , 49
  , 93
  , 359
  , 1138
  , 3802
  , 12124
  , 52844
  , 182752
  , 696647
  , 2852886
  , 16423633
  , 75301815
  , 367900714
  -- , 1531612895
  -- , 8389371542
  -- , 40423852287
  -- , 213232272280
  -- , 1295095864798
  -- , 7991762413764
  -- , 42259876674716
  -- , 252869570952706
  -- , 1378634826630301
  -- , 8749244047999717
  ]

totientSpecialCases1 :: [Assertion]
totientSpecialCases1 = zipWith mkAssert (tail factorial) totientCountFactorial
  where
    mkAssert n m = assertEqual "should be equal" m (getSum $ getConst $ totientCount n)

    totientCount :: Word -> Const (Sum Word) Word
    totientCount = inverseTotient (const $ Const 1)

-- | http://oeis.org/A055487
totientMinFactorial :: [Word]
totientMinFactorial =
  [ 1
  , 3
  , 7
  , 35
  , 143
  , 779
  , 5183
  , 40723
  , 364087
  , 3632617
  , 39916801
  , 479045521
  , 6227180929
  , 87178882081
  , 1307676655073
  , 20922799053799
  , 355687465815361
  , 6402373865831809
  -- , 121645101106397521
  -- , 2432902011297772771
  -- , 51090942186005065121
  -- , 1124000727844660550281
  -- , 25852016739206547966721
  -- , 620448401734814833377121
  -- , 15511210043338862873694721
  -- , 403291461126645799820077057
  -- , 10888869450418352160768000001
  -- , 304888344611714964835479763201
  ]

totientSpecialCases2 :: [Assertion]
totientSpecialCases2 = zipWith mkAssert (tail factorial) totientMinFactorial
  where
    mkAssert n m = assertEqual "should be equal" m (totientMin n)

    totientMin :: Word -> Word
    totientMin = unMinWord . inverseTotient MinWord

-- | http://oeis.org/A165774
totientMaxFactorial :: [Word]
totientMaxFactorial =
  [ 2
  , 6
  , 18
  , 90
  , 462
  , 3150
  , 22050
  , 210210
  , 1891890
  , 19969950
  , 219669450
  , 2847714870
  , 37020293310
  , 520843112790
  , 7959363061650
  , 135309172048050
  , 2300255924816850
  , 41996101027370490
  -- , 797925919520039310
  -- , 16504589035937252250
  -- , 347097774991217099850
  -- , 7751850308137181896650
  -- , 179602728970220622816750
  -- , 4493489228616853106091450
  -- , 112337230715421327652286250
  -- , 2958213742172761628176871250
  -- , 79871771038664563960775523750
  -- , 2279417465795734863803670716250
  ]

totientSpecialCases3 :: [Assertion]
totientSpecialCases3 = zipWith mkAssert (tail factorial) totientMaxFactorial
  where
    mkAssert n m = assertEqual "should be equal" m (totientMax n)

    totientMax :: Word -> Word
    totientMax = unMaxWord . inverseTotient MaxWord

-------------------------------------------------------------------------------
-- Sigma

sigmaProperty1 :: forall a. (Semiring a, Euclidean a, UniqueFactorisation a, Integral a) => Positive a -> Bool
sigmaProperty1 (Positive x) = Product x `S.member` (inverseSigma (S.singleton . Product) (sigma 1 x))

sigmaProperty2 :: (Semiring a, Euclidean a, UniqueFactorisation a, Integral a) => Positive a -> Bool
sigmaProperty2 (Positive x) = all (== x) (S.map (sigma 1 . getProduct) (inverseSigma (S.singleton . Product) x))

-------------------------------------------------------------------------------
-- TestTree

testSuite :: TestTree
testSuite = testGroup "Inverse"
  [ testGroup "Totient"
    [ testGroup "forward"
      [ testSmallAndQuick "Int"     (totientProperty1 :: Positive Int     -> Bool)
      , testSmallAndQuick "Word"    (totientProperty1 :: Positive Word    -> Bool)
      , testSmallAndQuick "Integer" (totientProperty1 :: Positive Integer -> Bool)
      , testSmallAndQuick "Natural" (totientProperty1 :: Positive Natural -> Bool)
      ]
    , testGroup "backward"
      [ testSmallAndQuick "Int"     (totientProperty2 :: Positive Int     -> Bool)
      , testSmallAndQuick "Word"    (totientProperty2 :: Positive Word    -> Bool)
      , testSmallAndQuick "Integer" (totientProperty2 :: Positive Integer -> Bool)
      , testSmallAndQuick "Natural" (totientProperty2 :: Positive Natural -> Bool)
      ]
    , testGroup "count"
      (zipWith (\i a -> testCase ("factorial " ++ show i) a) [1..] totientSpecialCases1)
    , testGroup "min"
      (zipWith (\i a -> testCase ("factorial " ++ show i) a) [1..] totientSpecialCases2)
    , testGroup "max"
      (zipWith (\i a -> testCase ("factorial " ++ show i) a) [1..] totientSpecialCases3)
    ]
  , testGroup "Sigma1"
    [ testGroup "forward"
      [ testSmallAndQuick "Int"     (sigmaProperty1 :: Positive Int     -> Bool)
      , testSmallAndQuick "Word"    (sigmaProperty1 :: Positive Word    -> Bool)
      , testSmallAndQuick "Integer" (sigmaProperty1 :: Positive Integer -> Bool)
      , testSmallAndQuick "Natural" (sigmaProperty1 :: Positive Natural -> Bool)
      ]
    , testGroup "backward"
      [ testSmallAndQuick "Int"     (sigmaProperty2 :: Positive Int     -> Bool)
      , testSmallAndQuick "Word"    (sigmaProperty2 :: Positive Word    -> Bool)
      , testSmallAndQuick "Integer" (sigmaProperty2 :: Positive Integer -> Bool)
      , testSmallAndQuick "Natural" (sigmaProperty2 :: Positive Natural -> Bool)
      ]
    ]
  ]
