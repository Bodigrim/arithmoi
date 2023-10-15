-- |
-- Module:      Math.NumberTheory.Recurrences.BilinearTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Recurrences.Bilinear
--

{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Recurrences.BilinearTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Arrow
import Data.List (sort)
import qualified Data.List.Infinite as Inf
import Data.Ratio

import Math.NumberTheory.Primes
import Math.NumberTheory.Recurrences.Bilinear
import Math.NumberTheory.TestUtils

binomialProperty1 :: Word -> Bool
binomialProperty1 i = length (binomial @Integer Inf.!! i) == fromIntegral i + 1

binomialProperty2 :: Word -> Bool
binomialProperty2 i = take 1 (binomial @Integer Inf.!! i) == [1]

binomialProperty3 :: Word -> Bool
binomialProperty3 i = binomial @Integer Inf.!! i !! fromIntegral i == 1

binomialProperty4 :: Positive Int -> Positive Int -> Bool
binomialProperty4 (Positive i) (Positive j)
  =  j >= i
  || binomial @Integer Inf.!! fromIntegral i !! j
  == binomial Inf.!! (fromIntegral i - 1) !! (j - 1)
  +  binomial Inf.!! (fromIntegral i - 1) !! j

binomialProperty5 :: Word -> Word -> Bool
binomialProperty5 n m' = n > 100000 ||
  sort (map (first unPrime) (factorise (binomial Inf.!! n !! fromIntegral m))) ==
    sort (map (first (toInteger . unPrime)) (binomialFactors n m))
  where
    m = m' `mod` (n + 1)

binomialProperty6 :: Word -> Word -> Bool
binomialProperty6 n m' = n > 100000 ||
  binomial Inf.!! n !! fromIntegral m ==
    product (map (\(p, k) -> toInteger (unPrime p) ^ k) (binomialFactors n m))
  where
    m = m' `mod` (n + 1)

binomialRotatedProperty2 :: Word -> Bool
binomialRotatedProperty2 i = Inf.head (binomialRotated @Integer Inf.!! i) == 1

binomialRotatedProperty3 :: Word -> Bool
binomialRotatedProperty3 i = Inf.head (binomialRotated @Integer) Inf.!! i == 1

binomialRotatedProperty4 :: Positive Int -> Positive Int -> Bool
binomialRotatedProperty4 (Positive i) (Positive j)
  =  binomialRotated @Integer Inf.!! fromIntegral i Inf.!! fromIntegral j
  == binomialRotated Inf.!! fromIntegral i Inf.!! (fromIntegral j - 1)
  +  binomialRotated Inf.!! (fromIntegral i - 1) Inf.!! fromIntegral j

binomialLineProperty1 :: Word -> Word -> Bool
binomialLineProperty1 i j
  =  j >= i
  || binomial @Integer Inf.!! i !! fromIntegral j == binomialLine (toInteger i) !! fromIntegral j

binomialLineProperty2 :: Word -> Word -> Bool
binomialLineProperty2 i j
  = binomialRotated @Integer Inf.!! i Inf.!! j == binomialLine (toInteger (i + j)) !! fromIntegral j

binomialDiagonalProperty1 :: Word -> Word -> Bool
binomialDiagonalProperty1 i j
  = binomialRotated @Integer Inf.!! i Inf.!! j == binomialDiagonal (toInteger i) Inf.!! j

binomialDiagonalProperty2 :: Word -> Word -> Bool
binomialDiagonalProperty2 i j
  = binomial @Integer Inf.!! (i + j) !! fromIntegral j == binomialDiagonal (toInteger i) Inf.!! j

stirling1Property1 :: Word -> Bool
stirling1Property1 i = length (stirling1 Inf.!! i) == fromIntegral i + 1

stirling1Property2 :: Word -> Bool
stirling1Property2 i
  =  take 1 (stirling1 Inf.!! i)
  == [if i == 0 then 1 else 0]

stirling1Property3 :: Word -> Bool
stirling1Property3 i = stirling1 Inf.!! i !! fromIntegral i == 1

stirling1Property4 :: Positive Int -> Positive Int -> Bool
stirling1Property4 (Positive i) (Positive j)
  =  j >= i
  || stirling1 Inf.!! fromIntegral i !! j
  == stirling1 Inf.!! (fromIntegral i - 1) !! (j - 1)
  +  (toInteger i - 1) * stirling1 Inf.!! (fromIntegral i - 1) !! j

stirling2Property1 :: Word -> Bool
stirling2Property1 i = length (stirling2 Inf.!! i) == fromIntegral i + 1

stirling2Property2 :: Word -> Bool
stirling2Property2 i
  =  take 1 (stirling2 Inf.!! i)
  == [if i == 0 then 1 else 0]

stirling2Property3 :: Word -> Bool
stirling2Property3 i = stirling2 Inf.!! i !! fromIntegral i == 1

stirling2Property4 :: Positive Int -> Positive Int -> Bool
stirling2Property4 (Positive i) (Positive j)
  =  j >= i
  || stirling2 Inf.!! fromIntegral i !! j
  == stirling2 Inf.!! (fromIntegral i - 1) !! (j - 1)
  +  toInteger j * stirling2 Inf.!! (fromIntegral i - 1) !! j

lahProperty1 :: Word -> Bool
lahProperty1 i = length (lah Inf.!! i) == fromIntegral i + 1

lahProperty2 :: Word -> Bool
lahProperty2 i
  =  take 1 (lah Inf.!! i)
  == [product [1 .. i+1]]

lahProperty3 :: Word -> Bool
lahProperty3 i = lah Inf.!! i !! fromIntegral i == 1

lahProperty4 :: Positive Int -> Positive Int -> Bool
lahProperty4 (Positive i) (Positive j)
  =  j >= i
  || lah Inf.!! fromIntegral i !! j
  == sum [ stirling1 Inf.!! (fromIntegral i + 1) !! k * stirling2 Inf.!! fromIntegral k !! (j + 1) | k <- [j + 1 .. i + 1] ]

eulerian1Property1 :: Word -> Bool
eulerian1Property1 i = length (eulerian1 Inf.!! i) == fromIntegral i

eulerian1Property2 :: Positive Int -> Bool
eulerian1Property2 (Positive i)
  =  take 1 (eulerian1 Inf.!! fromIntegral i)
  == [1]

eulerian1Property3 :: Positive Int -> Bool
eulerian1Property3 (Positive i) = eulerian1 Inf.!! fromIntegral i !! (i - 1) == 1

eulerian1Property4 :: Positive Int -> Positive Int -> Bool
eulerian1Property4 (Positive i) (Positive j)
  =  j >= i - 1
  || eulerian1 Inf.!! fromIntegral i !! j
  == toInteger (i - j) * eulerian1 Inf.!! (fromIntegral i - 1) !! (j - 1)
  +  (toInteger j + 1) * eulerian1 Inf.!! (fromIntegral i - 1) !! j

eulerian2Property1 :: Word -> Bool
eulerian2Property1 i = length (eulerian2 Inf.!! i) == fromIntegral i

eulerian2Property2 :: Positive Int -> Bool
eulerian2Property2 (Positive i)
  =  take 1 (eulerian2 Inf.!! fromIntegral i)
  == [1]

eulerian2Property3 :: Positive Int -> Bool
eulerian2Property3 (Positive i)
  =  eulerian2 Inf.!! fromIntegral i !! (i - 1)
  == product [1 .. toInteger i]

eulerian2Property4 :: Positive Int -> Positive Int -> Bool
eulerian2Property4 (Positive i) (Positive j)
  =  j >= i - 1
  || eulerian2 Inf.!! fromIntegral i !! j
  == toInteger (2 * i - j - 1) * eulerian2 Inf.!! (fromIntegral i - 1) !! (j - 1)
  +  (toInteger j + 1) * eulerian2 Inf.!! (fromIntegral i - 1) !! j

bernoulliSpecialCase1 :: Assertion
bernoulliSpecialCase1 = assertEqual "B_0 = 1" (Inf.head bernoulli) 1

bernoulliSpecialCase2 :: Assertion
bernoulliSpecialCase2 = assertEqual "B_1 = -1/2" (bernoulli Inf.!! 1) (- 1 % 2)

bernoulliProperty1 :: Word -> Bool
bernoulliProperty1 m
  = case signum (bernoulli Inf.!! m) of
    1  -> m == 0 || m `mod` 4 == 2
    0  -> m /= 1 && odd m
    -1 -> m == 1 || (m /= 0 && m `rem` 4 == 0)
    _  -> False

bernoulliProperty2 :: Word -> (Rational, Rational)
bernoulliProperty2 m =
  ( bernoulli Inf.!! m
  , (if m == 0 then 1 else 0)
  -  sum [ bernoulli Inf.!! k
         * (binomial Inf.!! m !! fromIntegral k % toInteger (m - k + 1))
         | k <- map fromIntegral [(0 :: Int) .. fromIntegral m - 1]
         ]
  )

-- | For every odd positive integer @n@, @E_n@ is @0@.
eulerProperty1 :: Positive Int -> Bool
eulerProperty1 (Positive n) = euler Inf.!! (2 * fromIntegral n - 1) == 0

-- | Every positive even index produces a negative result.
eulerProperty2 :: Word -> Bool
eulerProperty2 n = euler Inf.!! (2 + 4 * n) < 0

-- | The Euler number sequence is https://oeis.org/A122045
eulerSpecialCase1 :: Assertion
eulerSpecialCase1 = assertEqual "euler"
    (Inf.take 20 euler)
    [1, 0, -1, 0, 5, 0, -61, 0, 1385, 0, -50521, 0, 2702765, 0, -199360981, 0, 19391512145, 0, -2404879675441, 0]

-- | For any even positive integer @n@, @E_n(1)@ is @0@.
eulerPAt1Property1 :: Positive Int -> Bool
eulerPAt1Property1 (Positive n) = (eulerPolyAt1 Inf.!! (2 * fromIntegral n)) == 0

-- | The numerators in this sequence are from https://oeis.org/A198631 while the
-- denominators are from https://oeis.org/A006519.
eulerPAt1SpecialCase1 :: Assertion
eulerPAt1SpecialCase1 = assertEqual "eulerPolyAt1"
    (Inf.take 20 eulerPolyAt1)
    (zipWith (%) [1, 1, 0, -1, 0, 1, 0, -17, 0, 31, 0, -691, 0, 5461, 0, -929569, 0, 3202291, 0, -221930581]
                 [1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1, 16, 1, 2, 1, 4])

testSuite :: TestTree
testSuite = testGroup "Bilinear"
  [ testGroup "binomial"
    [ testSmallAndQuick "shape"      binomialProperty1
    , testSmallAndQuick "left side"  binomialProperty2
    , testSmallAndQuick "right side" binomialProperty3
    , testSmallAndQuick "recurrency" binomialProperty4
    , testSmallAndQuick "factorise . binomial = binomialFactors"  binomialProperty5
    , testSmallAndQuick "binomial = factorBack . binomialFactors" binomialProperty6
    , testSmallAndQuick "line"       binomialLineProperty1
    , testSmallAndQuick "diagonal"   binomialDiagonalProperty2
    ]
  , testGroup "binomialRotated"
    [ testSmallAndQuick "left side"  binomialRotatedProperty2
    , testSmallAndQuick "right side" binomialRotatedProperty3
    , testSmallAndQuick "recurrency" binomialRotatedProperty4
    , testSmallAndQuick "line"       binomialLineProperty2
    , testSmallAndQuick "diagonal"   binomialDiagonalProperty1
    ]
  , testGroup "stirling1"
    [ testSmallAndQuick "shape"      stirling1Property1
    , testSmallAndQuick "left side"  stirling1Property2
    , testSmallAndQuick "right side" stirling1Property3
    , testSmallAndQuick "recurrency" stirling1Property4
    ]
  , testGroup "stirling2"
    [ testSmallAndQuick "shape"      stirling2Property1
    , testSmallAndQuick "left side"  stirling2Property2
    , testSmallAndQuick "right side" stirling2Property3
    , testSmallAndQuick "recurrency" stirling2Property4
    ]
  , testGroup "lah"
    [ testSmallAndQuick "shape"         lahProperty1
    , testSmallAndQuick "left side"     lahProperty2
    , testSmallAndQuick "right side"    lahProperty3
    , testSmallAndQuick "zip stirlings" lahProperty4
    ]
  , testGroup "eulerian1"
    [ testSmallAndQuick "shape"      eulerian1Property1
    , testSmallAndQuick "left side"  eulerian1Property2
    , testSmallAndQuick "right side" eulerian1Property3
    , testSmallAndQuick "recurrency" eulerian1Property4
    ]
  , testGroup "eulerian2"
    [ testSmallAndQuick "shape"      eulerian2Property1
    , testSmallAndQuick "left side"  eulerian2Property2
    , testSmallAndQuick "right side" eulerian2Property3
    , testSmallAndQuick "recurrency" eulerian2Property4
    ]
  , testGroup "bernoulli"
    [ testCase "B_0"                           bernoulliSpecialCase1
    , testCase "B_1"                           bernoulliSpecialCase2
    , testSmallAndQuick "sign"                 bernoulliProperty1
    , testEqualSmallAndQuick "recursive definition" bernoulliProperty2
    ]
    , testGroup "Euler numbers"
    [ testCase "First 20 elements of E_n are correct"           eulerSpecialCase1
    , testSmallAndQuick "E_n with n odd is 0"                   eulerProperty1
    , testSmallAndQuick "E_n for n in [2,6,8,12..] is negative" eulerProperty2
    ]
  , testGroup "Euler Polynomial of order N evaluated at 1"
    [ testCase "First 20 elements of E_n(1) are correct"        eulerPAt1SpecialCase1
    , testSmallAndQuick "E_n(1) with n in [2,4,6..] is 0"       eulerPAt1Property1
    ]
  ]
