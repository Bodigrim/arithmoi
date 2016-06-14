-- |
-- Module:      Math.NumberTheory.ArithmeticFunctionsTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.ArithmeticFunctions
--

{-# LANGUAGE CPP       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ArithmeticFunctionsTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as S
import qualified Data.IntSet as IS

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Primes.Factorisation
import Math.NumberTheory.TestUtils

import Numeric.Natural

oeisAssertion :: (Eq a, Show a) => String -> ArithmeticFunction Natural a -> [a] -> Assertion
oeisAssertion name f baseline = assertEqual name baseline (map (runFunction f) [1 .. fromIntegral (length baseline)])

divisorsProperty1 :: Natural -> Bool
divisorsProperty1 n = length (runFunction divisorsA n) == runFunction tauA n

divisorsProperty2 :: Natural -> Bool
divisorsProperty2 n = sum (runFunction divisorsA n) == runFunction (sigmaA 1) n

divisorsProperty3 :: Natural -> Bool
divisorsProperty3 n = all (\d -> n `mod` d == 0) (runFunction divisorsA n)

divisorsProperty4 :: Int -> Bool
divisorsProperty4 n = S.toAscList (runFunction divisorsA n) == IS.toAscList (runFunction divisorsSmallA n)

tauOeis :: Assertion
tauOeis = oeisAssertion "A000005" tauA
  [ 1, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4, 4, 5, 2, 6, 2, 6, 4, 4, 2, 8
  , 3, 4, 4, 6, 2, 8, 2, 6, 4, 4, 4, 9, 2, 4, 4, 8, 2, 8, 2, 6, 6, 4, 2, 10
  , 3, 6, 4, 6, 2, 8, 4, 8, 4, 4, 2, 12, 2, 4, 6, 7, 4, 8, 2, 6, 4, 8, 2
  , 12, 2, 4, 6, 6, 4, 8, 2, 10, 5, 4, 2, 12, 4, 4, 4, 8, 2, 12, 4, 6, 4, 4
  , 4, 12, 2, 6, 6, 9, 2, 8, 2, 8
  ]

sigmaProperty1 :: Natural -> Bool
sigmaProperty1 n = runFunction tauA n == runFunction (sigmaA 0) n

sigmaProperty2 :: Natural -> Bool
sigmaProperty2 n = n <= 1 || runFunction (sigmaA 1) n > n

sigma1Oeis :: Assertion
sigma1Oeis = oeisAssertion "A000203" (sigmaA 1)
  [ 1, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28, 14, 24, 24, 31, 18, 39, 20
  , 42, 32, 36, 24, 60, 31, 42, 40, 56, 30, 72, 32, 63, 48, 54, 48, 91, 38
  , 60, 56, 90, 42, 96, 44, 84, 78, 72, 48, 124, 57, 93, 72, 98, 54, 120
  , 72, 120, 80, 90, 60, 168, 62, 96, 104, 127, 84, 144, 68, 126, 96, 144
  ]

sigma2Oeis :: Assertion
sigma2Oeis = oeisAssertion "A001157" (sigmaA 2)
  [ 1, 5, 10, 21, 26, 50, 50, 85, 91, 130, 122, 210, 170, 250, 260, 341, 290
  , 455, 362, 546, 500, 610, 530, 850, 651, 850, 820, 1050, 842, 1300, 962
  , 1365, 1220, 1450, 1300, 1911, 1370, 1810, 1700, 2210, 1682, 2500, 1850
  , 2562, 2366, 2650, 2210, 3410, 2451, 3255
  ]

totientProperty1 :: Natural -> Bool
totientProperty1 n = n <= 2 || even (runFunction totientA n)

totientProperty2 :: Natural -> Bool
totientProperty2 n = n <= 1 || runFunction totientA n < n

totientSieve100 :: TotientSieve
totientSieve100 = totientSieve 100

totientProperty3 :: Natural -> Bool
totientProperty3 n = n < 1
  || fromIntegral (runFunction totientA n)
    == sieveTotient totientSieve100 (fromIntegral n)

totientOeis :: Assertion
totientOeis = oeisAssertion "A000010" totientA
  [ 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6, 18, 8, 12, 10
  , 22, 8, 20, 12, 18, 12, 28, 8, 30, 16, 20, 16, 24, 12, 36, 18, 24, 16, 40
  , 12, 42, 20, 24, 22, 46, 16, 42, 20, 32, 24, 52, 18, 40, 24, 36, 28, 58
  , 16, 60, 30, 36, 32, 48, 20, 66, 32, 44
  ]

jordanProperty1 :: Natural -> Bool
jordanProperty1 n = n <= 1 || runFunction (jordanA 0) n == 0

jordanProperty2 :: Natural -> Bool
jordanProperty2 n = runFunction totientA n == runFunction (jordanA 1) n

jordan2Oeis :: Assertion
jordan2Oeis = oeisAssertion "A007434" (jordanA 2)
  [ 1, 3, 8, 12, 24, 24, 48, 48, 72, 72, 120, 96, 168, 144, 192, 192, 288
  , 216, 360, 288, 384, 360, 528, 384, 600, 504, 648, 576, 840, 576, 960
  , 768, 960, 864, 1152, 864, 1368, 1080, 1344, 1152, 1680, 1152, 1848, 1440
  , 1728, 1584, 2208, 1536
  ]

moebiusProperty1 :: Natural -> Bool
moebiusProperty1 n = runFunction moebiusA n `elem` [-1, 0, 1]

moebiusLazy :: Assertion
moebiusLazy = assertEqual "moebius" 0 (runFunction moebiusA (2^2 * (2^100000-1) :: Natural))

moebiusOeis :: Assertion
moebiusOeis = oeisAssertion "A008683" moebiusA
  [ 1, -1, -1, 0, -1, 1, -1, 0, 0, 1, -1, 0, -1, 1, 1, 0, -1, 0, -1, 0, 1, 1, -1
  , 0, 0, 1, 0, 0, -1, -1, -1, 0, 1, 1, 1, 0, -1, 1, 1, 0, -1, -1, -1, 0, 0, 1
  , -1, 0, 0, 0, 1, 0, -1, 0, 1, 0, 1, 1, -1, 0, -1, 1, 0, 0, 1, -1, -1, 0, 1
  , -1, -1, 0, -1, 1, 0, 0, 1
  ]

liouvilleProperty1 :: Natural -> Bool
liouvilleProperty1 n = runFunction liouvilleA n `elem` [-1, 1]

liouvilleProperty2 :: Natural -> Bool
liouvilleProperty2 n = m == 0 || l == m
  where
    l = runFunction liouvilleA n
    m = runFunction moebiusA   n

liouvilleOeis :: Assertion
liouvilleOeis = oeisAssertion "A008836" liouvilleA
  [ 1, -1, -1, 1, -1, 1, -1, -1, 1, 1, -1, -1, -1, 1, 1, 1, -1, -1, -1, -1, 1, 1
  , -1, 1, 1, 1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, -1, 1, 1, 1, -1, -1, -1, -1
  , -1, 1, -1, -1, 1, -1, 1, -1, -1, 1, 1, 1, 1, 1, -1, 1, -1, 1, -1, 1, 1, -1
  , -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1, 1, 1, 1, 1
  , -1, 1, 1, -1, 1, 1, 1, 1, -1, -1, -1, 1, -1
  ]

carmichaelProperty1 :: Natural -> Bool
carmichaelProperty1 n = runFunction totientA n `mod` runFunction carmichaelA n == 0

carmichaelSieve100 :: CarmichaelSieve
carmichaelSieve100 = carmichaelSieve 100

carmichaelProperty2 :: Natural -> Bool
carmichaelProperty2 n = n < 1
  || fromIntegral (runFunction carmichaelA n)
    == sieveCarmichael carmichaelSieve100 (fromIntegral n)

carmichaelOeis :: Assertion
carmichaelOeis = oeisAssertion "A002322" carmichaelA
  [ 1, 1, 2, 2, 4, 2, 6, 2, 6, 4, 10, 2, 12, 6, 4, 4, 16, 6, 18, 4, 6, 10, 22, 2
  , 20, 12, 18, 6, 28, 4, 30, 8, 10, 16, 12, 6, 36, 18, 12, 4, 40, 6, 42, 10, 12
  , 22, 46, 4, 42, 20, 16, 12, 52, 18, 20, 6, 18, 28, 58, 4, 60, 30, 6, 16, 12
  , 10, 66, 16, 22, 12, 70, 6, 72, 36, 20, 18, 30, 12, 78, 4, 54
  ]

omegaProperty1 :: Natural -> Bool
omegaProperty1 n = runFunction smallOmegaA n <= runFunction bigOmegaA n

smallOmegaOeis :: Assertion
smallOmegaOeis = oeisAssertion "A001221" smallOmegaA
  [ 0, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2, 1, 2
  , 1, 2, 1, 3, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 3, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2
  , 1, 2, 2, 2, 2, 2, 1, 3, 1, 2, 2, 1, 2, 3, 1, 2, 2, 3, 1, 2, 1, 2, 2, 2, 2, 3
  , 1, 2, 1, 2, 1, 3, 2, 2, 2, 2, 1, 3, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 3, 1, 2
  , 3, 2, 1, 2, 1, 3, 2
  ]

bigOmegaOeis :: Assertion
bigOmegaOeis = oeisAssertion "A001222" bigOmegaA
  [ 0, 1, 1, 2, 1, 2, 1, 3, 2, 2, 1, 3, 1, 2, 2, 4, 1, 3, 1, 3, 2, 2, 1, 4, 2, 2
  , 3, 3, 1, 3, 1, 5, 2, 2, 2, 4, 1, 2, 2, 4, 1, 3, 1, 3, 3, 2, 1, 5, 2, 3, 2, 3
  , 1, 4, 2, 4, 2, 2, 1, 4, 1, 2, 3, 6, 2, 3, 1, 3, 2, 3, 1, 5, 1, 2, 3, 3, 2, 3
  , 1, 5, 4, 2, 1, 4, 2, 2, 2, 4, 1, 4, 2, 3, 2, 2, 2, 6, 1, 3, 3, 4, 1, 3, 1, 4
  , 3, 2, 1, 5, 1, 3, 2
  ]

mangoldtOeis :: Assertion
mangoldtOeis = oeisAssertion "A014963" expMangoldtA
  [ 1, 2, 3, 2, 5, 1, 7, 2, 3, 1, 11, 1, 13, 1, 1, 2, 17, 1, 19, 1, 1, 1, 23, 1
  , 5, 1, 3, 1, 29, 1, 31, 2, 1, 1, 1, 1, 37, 1, 1, 1, 41, 1, 43, 1, 1, 1, 47, 1
  , 7, 1, 1, 1, 53, 1, 1, 1, 1, 1, 59, 1, 61, 1, 1, 2, 1, 1, 67, 1, 1, 1, 71, 1
  , 73, 1, 1, 1, 1, 1, 79, 1, 3, 1, 83, 1, 1, 1, 1, 1, 89, 1, 1, 1, 1, 1, 1
  ]

testSuite :: TestTree
testSuite = testGroup "ArithmeticFunctions"
  [ testGroup "Divisors"
    [ testSmallAndQuick "length . divisors = tau" divisorsProperty1
    , testSmallAndQuick "sum . divisors = sigma_1" divisorsProperty2
    , testSmallAndQuick "matches definition" divisorsProperty3
    , testSmallAndQuick "divisors = divisorsSmall" divisorsProperty4
    ]
  , testGroup "Tau"
    [ testCase "OEIS" tauOeis
    ]
  , testGroup "Sigma"
    [ testSmallAndQuick "sigma_0 = tau" sigmaProperty1
    , testSmallAndQuick "sigma_1 n > n" sigmaProperty2
    , testCase "OEIS sigma_1" sigma1Oeis
    , testCase "OEIS sigma_2" sigma2Oeis
    ]
  , testGroup "Totient"
    [ testSmallAndQuick "totient is even" totientProperty1
    , testSmallAndQuick "totient n < n" totientProperty2
    , testSmallAndQuick "matches sieveTotient" totientProperty3
    , testCase "OEIS" totientOeis
    ]
  , testGroup "Jordan"
    [ testSmallAndQuick "jordan_0 = [== 1]" jordanProperty1
    , testSmallAndQuick "jordan_1 = totient" jordanProperty2
    , testCase "OEIS jordan_2" jordan2Oeis
    ]
  , testGroup "Moebius"
    [ testSmallAndQuick "moebius values" moebiusProperty1
    , testCase "OEIS" moebiusOeis
    , testCase "Lazy" moebiusLazy
    ]
  , testGroup "Liouville"
    [ testSmallAndQuick "liouville values" liouvilleProperty1
    , testSmallAndQuick "liouville matches moebius" liouvilleProperty2
    , testCase "OEIS" liouvilleOeis
    ]
  , testGroup "Carmichael"
    [ testSmallAndQuick "carmichael divides totient" carmichaelProperty1
    , testSmallAndQuick "matches sieveCarmichael" carmichaelProperty2
    , testCase "OEIS" carmichaelOeis
    ]
  , testGroup "Omegas"
    [ testSmallAndQuick "smallOmega <= bigOmega" omegaProperty1
    , testCase "OEIS smallOmega" smallOmegaOeis
    , testCase "OEIS bigOmega" bigOmegaOeis
    ]
  , testGroup "Mangoldt"
    [ testCase "OEIS" mangoldtOeis
    ]
  ]
