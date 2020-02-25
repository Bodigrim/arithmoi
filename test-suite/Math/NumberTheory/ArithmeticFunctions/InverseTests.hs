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
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ArithmeticFunctions.InverseTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC hiding (test)
import Test.Tasty.QuickCheck as QC hiding (Positive)

import Data.Bits (Bits)
import Data.Euclidean
import Data.Semiring (Semiring)
import qualified Data.Set as S
import Numeric.Natural (Natural)

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.ArithmeticFunctions.Inverse
import Math.NumberTheory.Primes
import Math.NumberTheory.Recurrences
import Math.NumberTheory.TestUtils
import Math.NumberTheory.TestUtils.Wrappers (Power (..))

-------------------------------------------------------------------------------
-- Totient

totientProperty1 :: forall a. (Euclidean a, Integral a, UniqueFactorisation a) => Positive a -> Bool
totientProperty1 (Positive x) = x `S.member` asSetOfPreimages inverseTotient (totient x)

jordanProperty1
  :: (Euclidean a, Integral a, UniqueFactorisation a)
  => Power Word
  -> Positive a
  -> Bool
jordanProperty1 (Power k') (Positive x) =
  -- 'k' shouldn't be large to avoid slow tests.
  let k = 2 + k' `Prelude.mod` 20
  in x `S.member` asSetOfPreimages (inverseJordan k) (jordan k x)

totientProperty2 :: (Euclidean a, Integral a, UniqueFactorisation a) => Positive a -> Bool
totientProperty2 (Positive x) = all (== x) (S.map totient (asSetOfPreimages inverseTotient x))

jordanProperty2
  :: (Euclidean a, Integral a, UniqueFactorisation a, Ord a)
  => Power Word
  -> Positive a
  -> Bool
jordanProperty2 (Power k') (Positive x) =
  let k = 2 + k' `Prelude.mod` 20
  in all (== x) (S.map (jordan k) (asSetOfPreimages (inverseJordan k) x))

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
  ]

totientSpecialCases1 :: [Assertion]
totientSpecialCases1 = zipWith mkAssert (tail factorial) totientCountFactorial
  where
    mkAssert n m = assertEqual "should be equal" m (totientCount n)

    totientCount :: Word -> Word
    totientCount = inverseTotient (const 1)

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
  ]

totientSpecialCases3 :: [Assertion]
totientSpecialCases3 = zipWith mkAssert (tail factorial) totientMaxFactorial
  where
    mkAssert n m = assertEqual "should be equal" m (totientMax n)

    totientMax :: Word -> Word
    totientMax = unMaxWord . inverseTotient MaxWord

jordans5 :: [Word]
jordans5 =
  [ 1
  , 31
  , 242
  , 992
  , 3124
  , 7502
  , 16806
  , 31744
  , 58806
  , 96844
  , 161050
  , 240064
  , 371292
  , 520986
  , 756008
  , 1015808
  , 1419856
  , 1822986
  , 2476098
  , 3099008
  , 4067052
  , 4992550
  , 6436342
  , 7682048
  , 9762500
  , 11510052
  , 14289858
  , 16671552
  , 20511148
  ]

jordanSpecialCase1 :: [Assertion]
jordanSpecialCase1 = zipWith mkAssert ixs jordans5
  where
    mkAssert a b = assertEqual "should be equal" (S.singleton a) (asSetOfPreimages (inverseJordan 5) b)
    ixs = [1 .. 29]

-------------------------------------------------------------------------------
-- Sigma

sigmaProperty1 :: forall a. (Euclidean a, UniqueFactorisation a, Integral a, Enum (Prime a), Bits a) => Positive a -> Bool
sigmaProperty1 (Positive x) = x `S.member` asSetOfPreimages inverseSigma (sigma 1 x)

sigmaKProperty1
  :: forall a
   . (Euclidean a, UniqueFactorisation a, Integral a, Enum (Prime a), Bits a)
  => Power Word
  -> Positive a
  -> Bool
sigmaKProperty1 (Power k') (Positive x) =
  -- 'k' shouldn't be large to avoid slow tests.
  let k = 2 + k' `Prelude.mod` 20
  in x `S.member` asSetOfPreimages (inverseSigmaK k) (sigma k x)

sigmaProperty2 :: (Euclidean a, UniqueFactorisation a, Integral a, Enum (Prime a), Bits a) => Positive a -> Bool
sigmaProperty2 (Positive x) = all (== x) (S.map (sigma 1) (asSetOfPreimages inverseSigma x))

sigmaKProperty2
  :: (Euclidean a, UniqueFactorisation a, Integral a, Enum (Prime a), Bits a)
  => Power Word
  -> Positive a
  -> Bool
sigmaKProperty2 (Power k') (Positive x) =
  let k = 2 + k' `Prelude.mod` 20
  in all (== x) (S.map (sigma k) (asSetOfPreimages (inverseSigmaK k) x))

-- | http://oeis.org/A055486
sigmaCountFactorial :: [Word]
sigmaCountFactorial =
  [ 1
  , 0
  , 1
  , 3
  , 4
  , 15
  , 33
  , 111
  , 382
  , 1195
  , 3366
  , 14077
  , 53265
  , 229603
  , 910254
  , 4524029
  , 18879944
  , 91336498
  ]

sigmaSpecialCases1 :: [Assertion]
sigmaSpecialCases1 = zipWith mkAssert (tail factorial) sigmaCountFactorial
  where
    mkAssert n m = assertEqual "should be equal" m (sigmaCount n)

    sigmaCount :: Word -> Word
    sigmaCount = inverseSigma (const 1)

-- | http://oeis.org/A055488
sigmaMinFactorial :: [Word]
sigmaMinFactorial =
  [ 5
  , 14
  , 54
  , 264
  , 1560
  , 10920
  , 97440
  , 876960
  , 10263240
  , 112895640
  , 1348827480
  , 18029171160
  , 264370186080
  , 3806158356000
  , 62703141621120
  , 1128159304272000
  ]

sigmaSpecialCases2 :: [Assertion]
sigmaSpecialCases2 = zipWith mkAssert (drop 3 factorial) sigmaMinFactorial
  where
    mkAssert n m = assertEqual "should be equal" m (sigmaMin n)

    sigmaMin :: Word -> Word
    sigmaMin = unMinWord . inverseSigma MinWord

-- | http://oeis.org/A055489
sigmaMaxFactorial :: [Word]
sigmaMaxFactorial =
  [ 5
  , 23
  , 95
  , 719
  , 5039
  , 39917
  , 361657
  , 3624941
  , 39904153
  , 479001599
  , 6226862869
  , 87178291199
  , 1307672080867
  , 20922780738961
  , 355687390376431
  , 6402373545694717
  ]

sigmaSpecialCases3 :: [Assertion]
sigmaSpecialCases3 = zipWith mkAssert (drop 3 factorial) sigmaMaxFactorial
  where
    mkAssert n m = assertEqual "should be equal" m (sigmaMax n)

    sigmaMax :: Word -> Word
    sigmaMax = unMaxWord . inverseSigma MaxWord

sigmaSpecialCase4 :: Assertion
sigmaSpecialCase4 = assertBool "200 should be in inverseSigma(sigma(200))" $
  sigmaProperty1 $ Positive (200 :: Word)

sigmas5 :: [Word]
sigmas5 =
  [ 1
  , 33
  , 244
  , 1057
  , 3126
  , 8052
  , 16808
  , 33825
  , 59293
  , 103158
  , 161052
  , 257908
  , 371294
  , 554664
  , 762744
  , 1082401
  , 1419858
  , 1956669
  , 2476100
  , 3304182
  , 4101152
  , 5314716
  , 6436344
  , 8253300
  , 9768751
  , 12252702
  , 14408200
  , 17766056
  , 20511150
  ]

sigmaSpecialCase5 :: [Assertion]
sigmaSpecialCase5 = zipWith mkAssert ixs sigmas5
 where
  mkAssert a b = assertEqual "should be equal" (S.singleton a) (asSetOfPreimages (inverseSigmaK 5) b)
  ixs = [1 .. 29]

-------------------------------------------------------------------------------
-- TestTree

-- Tests for 'Int', 'Word' are omitted because 'inverseSigmaK/inverseJordan'
-- tests would quickly oveflow in these types.
testIntegralPropertyNoLargeInverse
  :: forall bool. (SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a. (Euclidean a, Semiring a, Integral a, Bits a, UniqueFactorisation a, Show a, Enum (Prime a)) => Power Word -> Positive a -> bool) -> TestTree
testIntegralPropertyNoLargeInverse name f = testGroup name
  [ SC.testProperty "smallcheck Integer" (f :: Power Word -> Positive Integer -> bool)
  , SC.testProperty "smallcheck Natural" (f :: Power Word -> Positive Natural -> bool)
  , QC.testProperty "quickcheck Integer" (f :: Power Word -> Positive Integer -> bool)
  , QC.testProperty "quickcheck Natural" (f :: Power Word -> Positive Natural -> bool)
  ]

testSuite :: TestTree
testSuite = testGroup "Inverse"
  [ testGroup "Totient"
    [ testIntegralPropertyNoLarge "forward"  totientProperty1
    , testIntegralPropertyNoLarge "backward" totientProperty2
    , testGroup "count"
      (zipWith (\i a -> testCase ("factorial " ++ show i) a) [1..] totientSpecialCases1)
    , testGroup "min"
      (zipWith (\i a -> testCase ("factorial " ++ show i) a) [1..] totientSpecialCases2)
    , testGroup "max"
      (zipWith (\i a -> testCase ("factorial " ++ show i) a) [1..] totientSpecialCases3)
    ]
  , testGroup "Sigma1"
    [ testIntegralPropertyNoLarge "forward"  sigmaProperty1
    , testIntegralPropertyNoLarge "backward" sigmaProperty2
    , testCase "200" sigmaSpecialCase4
    , testGroup "count"
      (zipWith (\i a -> testCase ("factorial " ++ show i) a) [1..] sigmaSpecialCases1)
    , testGroup "min"
      (zipWith (\i a -> testCase ("factorial " ++ show i) a) [1..] sigmaSpecialCases2)
    , testGroup "max"
      (zipWith (\i a -> testCase ("factorial " ++ show i) a) [1..] sigmaSpecialCases3)
    ]

  , testGroup "Jordan"
    [ testIntegralPropertyNoLargeInverse "forward"  jordanProperty1
    , testIntegralPropertyNoLargeInverse "backward" jordanProperty2
    , testGroup "inverseJordan"
      (zipWith (\i test -> testCase ("inverseJordan 5" ++ show i) test) jordans5 jordanSpecialCase1)
    ]

  , testGroup  "SigmaK"
    [ testIntegralPropertyNoLargeInverse "forward"  sigmaKProperty1
    , testIntegralPropertyNoLargeInverse "backward" sigmaKProperty2
    , testGroup "inverseSigma"
      (zipWith (\i test -> testCase ("inverseSigma 5" ++ show i) test) sigmas5 sigmaSpecialCase5)
    ]
  ]
